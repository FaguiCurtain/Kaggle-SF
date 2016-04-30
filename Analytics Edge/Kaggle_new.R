#starts from scratch

table_mean <- function(df){
        df[,2]/(df[,1]+df[,2])
}
# sort(table_mean(table(eBayTrain$carrier,eBayTrain$sold)))

eBayTest <- read.csv("eBayiPadTest.csv")
eBayTrain <- read.csv("eBayiPadTrain.csv")

eBayTrain1 <- eBayTrain
eBayTest1 <- eBayTest

eBayTrain1$description <- factor(eBayTrain1$description=="")
eBayTest1$description <- factor(eBayTest1$description=="")

#there is no such obs in the TEST set
#removing iPad mini Retina (8obs) and iPad5 (1obs). new set=1852 obs
eBayTrain1 <- subset(eBayTrain1,! ( (eBayTrain1$productline=="iPad mini Retina")|
                                            (eBayTrain1$productline=="iPad 5")) )
eBayTrain1$productline <- factor(eBayTrain1$productline)

#reordering levels

levels(eBayTrain1$carrier) <- factor(c("Unknown","T-Mobile","SPrint","AT&T","Verizon","None","Other"))
levels(eBayTest1$carrier) <- factor(c("Unknown","T-Mobile","SPrint","AT&T","Verizon","None","Other"))
levels(eBayTrain1$color) <- factor(c("Gold","White","Space Gray","Unknown","Black"))
levels(eBayTest1$color) <- factor(c("Gold","White","Space Gray","Unknown","Black"))
levels(eBayTrain1$storage) <- factor(c("Unknown","16","32","64","128"))
levels(eBayTest1$storage) <- factor(c("Unknown","16","32","64","128"))
levels(eBayTrain1$productline) <- factor(c("iPad mini 3","Unknown","iPad 4","iPad Air 2","iPad Air","iPad mini 2","iPad mini","iPad 2","iPad 3","iPad 1"))
levels(eBayTest1$productline) <- factor(c("iPad mini 3","Unknown","iPad 4","iPad Air 2","iPad Air","iPad mini 2","iPad mini","iPad 2","iPad 3","iPad 1"))
levels(eBayTrain1$condition) <- factor(c("New","Seller refurbished","Manufacturer refurbished","New other (see details)","Used","For parts or not working"))
levels(eBayTest1$condition) <- factor(c("New","Seller refurbished","Manufacturer refurbished","New other (see details)","Used","For parts or not working"))
eBayTrain1$biddable <- as.factor(eBayTrain1$biddable)
eBayTest1$biddable <- as.factor(eBayTest1$biddable)


#loading libraries
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ROCR)

eBayTrain1$startprice_levels <- cut(eBayTrain1$startprice,breaks=25*(0:40))


nobidTrain <- subset(eBayTrain1,biddable==0)
bidTrain <- subset(eBayTrain1,biddable==1)
nobidTrain_1 <- aggregate(sold~productline+startprice_levels,data=nobidTrain,FUN="mean")
bidTrain_1 <- aggregate(sold~productline+startprice_levels,data=bidTrain,FUN="mean")

bid_sold <- subset(bidTrain,sold==1)
price_table <- tapply(bid_sold$startprice,bid_sold$productline,mean)
price_vol_table <- tapply(bid_sold$startprice,bid_sold$productline,sd)

# x <- as.numeric((eBayTrain1$startprice- price_table[as.numeric(eBayTrain1$productline)])/price_vol_table[as.numeric(eBayTrain1$productline)])

x <- as.numeric((bidTrain$startprice- price_table[as.numeric(bidTrain$productline)])/price_vol_table[as.numeric(bidTrain$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
# summary(y)
bidTrain$price_index <- x

# test with AUC.R
# train <- bidTrain
# train$startprice <- NULL
# train$startprice_levels <- NULL
# 
# train <- train[,c(8,1:7,10)]
#"Random Forest Average AUC: 0.79206306148997"
#"GBM Average AUC: 0.805034119550205"

nobid_sold <- subset(nobidTrain,sold==1)
price_table_nobid <- tapply(nobid_sold$startprice,nobid_sold$productline,mean)
price_vol_table_nobid <- tapply(nobid_sold$startprice,nobid_sold$productline,sd)

x <- as.numeric((nobidTrain$startprice- price_table[as.numeric(nobidTrain$productline)])/price_vol_table[as.numeric(nobidTrain$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
nobidTrain$price_index <- x

# train <- nobidTrain
# train$startprice <- NULL
# train$startprice_levels <- NULL
# 
# train <- train[,c(8,1:7,10)]

###### Logistic Regressions with bid/nobid subsets ######
#biddable subset
bidTrainLR <- bidTrain
bidTrainLR$biddable <- NULL
bidTrainLR$condition <- as.integer(bidTrain$condition)
bidTrainLR$cellular <- as.integer(bidTrain$cellular)
bidTrainLR$carrier <- as.integer(bidTrain$carrier)
bidTrainLR$color <- as.integer(bidTrain$color)
bidTrainLR$storage <- as.integer(bidTrain$storage)
bidTrainLR$productline <- as.integer(bidTrain$productline)
bidTrainLR$startprice <- NULL
bidTrainLR$startprice_levels <- NULL

LogBid <- glm(sold~.-UniqueID,data=bidTrainLR,family="binomial")
# LogBid <- glm(sold~condition+productline+price_index,data=bidTrainLR,family="binomial")
predLR_Bid <- predict(LogBid,type="response")
ROCRpred = prediction(predLR_Bid, bidTrainLR$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8824292 all variables
# [1] 0.88083 simplified

#non-biddable subset
nobidTrainLR <- nobidTrain
nobidTrainLR$biddable <- NULL
nobidTrainLR$condition <- as.integer(nobidTrain$condition)
nobidTrainLR$cellular <- as.integer(nobidTrain$cellular)
nobidTrainLR$carrier <- as.integer(nobidTrain$carrier)
nobidTrainLR$color <- as.integer(nobidTrain$color)
nobidTrainLR$storage <- as.integer(nobidTrain$storage)
nobidTrainLR$productline <- as.integer(nobidTrain$productline)
nobidTrainLR$startprice <- NULL
nobidTrainLR$startprice_levels <- NULL

LognoBid <- glm(sold~.-UniqueID,data=nobidTrainLR,family="binomial")
predLR_noBid <- predict(LognoBid,type="response")
ROCRpred = prediction(predLR_noBid, nobidTrainLR$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.6330098

# combining the 2 sets
temp1 <- cbind(bidTrain$UniqueID,predLR_Bid)
temp2 <- cbind(nobidTrain$UniqueID,predLR_noBid)

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)

print("whole Train set AUC for Logistic Regression")
ROCRpred = prediction(ans[,2], eBayTrain1$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8686322 all variables
# [1] 0.8672644 simplified for biddable subset

###### end of Logistic Regressions with bid/nobid subsets ######

####### GBM with bid/nobid subsets#######
#using GBM
#when CV.folds>0 i have the error message
#Error in object$var.levels[[i]] : subscript out of bounds
#means that when doing CV folds the number of levels is NOT the same
#class.stratify.cv=TRUE doesn't work to solve this
set.seed(999)
# biddable subset
bidTrain$startprice <- NULL
bidTrain$startprice_levels <- NULL
# bidTrain$price_index <- NULL

## choose between gbm models

# brutal gbm
gbm_algo_bid <- gbm(sold~. - UniqueID,data=bidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

# gbm with 33% validation set
# gbm_algo_bid <- gbm(sold~. - UniqueID,data=bidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,train.fraction=0.67,cv.folds=0)

predgbmBidTrain<- predict.gbm(gbm_algo_bid,bidTrain,n.trees=10000,type='response')
# Train set AUC 
library(ROCR)
print("bidTrain subset AUC for GBM")
ROCRpred = prediction(predgbmBidTrain, bidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8996109

#non-biddable subset
nobidTrain$startprice <- NULL
nobidTrain$startprice_levels <- NULL
# nobidTrain$price_index <- NULL
## choose between gbm models
# brutal gbm
gbm_algo_nobid <- gbm(sold~. - UniqueID,data=nobidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

# gbm with 33% validation set
# gbm_algo_nobid <- gbm(sold~. - UniqueID,data=nobidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,train.fraction=0.67,cv.folds=0)

predgbmnoBidTrain<- predict.gbm(gbm_algo_nobid,nobidTrain,n.trees=10000,type='response')

# Train set AUC 
print("nobidTrain subset AUC for GBM")
ROCRpred = prediction(predgbmnoBidTrain, nobidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.6919297


# baseline prediction
# predbase <- rep(0,1017)
# ROCRpred = prediction(predbase, nobidTrain$sold)
# print("base AUC")
# print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.5

# combining the 2 sets
temp1 <- cbind(bidTrain$UniqueID,as.numeric(predgbmBidTrain))
temp2 <- cbind(nobidTrain$UniqueID,as.numeric(predgbmnoBidTrain))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)

print("whole Train set AUC for GBM")
ROCRpred = prediction(ans[,2], eBayTrain1$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8827647
# [1] 0.8729874 33% validation 
## making the submission for GBM
# preparing the test set

nobidTest <- subset(eBayTest1,biddable==0)
bidTest <- subset(eBayTest1,biddable==1)


# biddable subset
x <- as.numeric((bidTest$startprice- price_table[as.numeric(bidTest$productline)])/price_vol_table[as.numeric(bidTest$productline)])
# y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
# summary(y)
bidTest$price_index <- x

bidTest$startprice <- NULL
bidTest$startprice_levels <- NULL
# bidTest$price_index <- NULL
predgbmBidTest<- predict.gbm(gbm_algo_bid,bidTest,n.trees=10000,type='response')

#non-biddable subset
x <- as.numeric((nobidTest$startprice- price_table_nobid[as.numeric(nobidTest$productline)])/price_vol_table_nobid[as.numeric(nobidTest$productline)])
#y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
nobidTest$price_index <- x

nobidTest$startprice <- NULL
nobidTest$startprice_levels <- NULL
# nobidTest$price_index <- NULL
predgbmnoBidTest<- predict.gbm(gbm_algo_nobid,nobidTest,n.trees=10000,type='response')

# combining the 2 sets
temp1 <- cbind(bidTest$UniqueID,as.numeric(predgbmBidTest))
temp2 <- cbind(nobidTest$UniqueID,as.numeric(predgbmnoBidTest))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)

#submitting GBM model
submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission.csv",row.names=FALSE)


####### end of GBM with bid/nobid subsets#######

####### randomForest with bid/nobid subsets #######
set.seed(456)
# necessary for RandomForest
bidTrain$sold <- factor(bidTrain$sold)
nobidTrain$sold <- factor(nobidTrain$sold)

#biddable subset
bidForest <- randomForest(sold ~ .-UniqueID, data = bidTrain, ntree=200, nodesize=10 )
PredbidTrainForest = predict(bidForest,type="prob")[,2]

print("bidTrain subset AUC for Random Forest")
ROCRpred = prediction(PredbidForest, bidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#
#non biddable subset
nobidForest <- randomForest(sold ~ .-UniqueID, data = nobidTrain, ntree=200, nodesize=10 )
PrednobidTrainForest = predict(nobidForest,type="prob")[,2]

print("nobidTrain subset AUC for Random Forest")
ROCRpred = prediction(PrednobidForest, nobidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#
# combining the 2 sets
temp1 <- cbind(bidTrain$UniqueID,as.numeric(PredbidTrainForest))
temp2 <- cbind(nobidTrain$UniqueID,as.numeric(PrednobidTrainForest))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)

print("whole Train set AUC for Random Forest")
ROCRpred = prediction(ans[,2], eBayTrain1$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

####### trying with the whole set #######
print("now Trying with the whole set")
eBayTrain2 <- rbind(bidTrain,nobidTrain)
eBayTrain2 <- eBayTrain2[order(eBayTrain2[,9]),] #1852 obs 11 variables

## LogReg on the whole set
#LogReg <- glm(sold~. - UniqueID,data=eBayTrain2,family="binomial")
LogReg <- glm(sold~biddable+condition+productline+storage+price_index+carrier,data=eBayTrain2,family="binomial")
predLR <- predict(LogReg,type="response")
ROCRpred = prediction(predLR, eBayTrain2$sold)
print("LogReg AUC for the whole set")
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8712623 a little bit higher
# [1] 0.8692716 with simplified LogReg model
## GBM on whole set
gbm_algo <- gbm(sold~. - UniqueID,data=eBayTrain2,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

predgbmTrain<- predict.gbm(gbm_algo_bid,eBayTrain2,n.trees=10000,type='response')

print("eBayTrain2 AUC for GBM")
ROCRpred = prediction(predgbmTrain, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8499328

## randomForest on whole set
set.seed(456)
# necessary for RandomForest
eBayTrain2$sold <- factor(eBayTrain2$sold)

eBayForest = randomForest(sold ~ .-UniqueID, data = eBayTrain2, ntree=200, nodesize=10 )
PredForest = predict(eBayForest,type="prob")[,2]

print("Train set AUC for Random Forest")
ROCRpred = prediction(PredForest, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8630998
