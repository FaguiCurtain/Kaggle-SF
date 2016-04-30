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

# levels(eBayTrain1$carrier) <- factor(c("Unknown","T-Mobile","SPrint","AT&T","Verizon","None","Other"))
# levels(eBayTest1$carrier) <- factor(c("Unknown","T-Mobile","SPrint","AT&T","Verizon","None","Other"))
# levels(eBayTrain1$color) <- factor(c("Gold","White","Space Gray","Unknown","Black"))
# levels(eBayTest1$color) <- factor(c("Gold","White","Space Gray","Unknown","Black"))
# levels(eBayTrain1$storage) <- factor(c("Unknown","16","32","64","128"))
# levels(eBayTest1$storage) <- factor(c("Unknown","16","32","64","128"))
# levels(eBayTrain1$productline) <- factor(c("iPad mini 3","Unknown","iPad 4","iPad Air 2","iPad Air","iPad mini 2","iPad mini","iPad 2","iPad 3","iPad 1"))
# levels(eBayTest1$productline) <- factor(c("iPad mini 3","Unknown","iPad 4","iPad Air 2","iPad Air","iPad mini 2","iPad mini","iPad 2","iPad 3","iPad 1"))
# levels(eBayTrain1$condition) <- factor(c("New","Seller refurbished","Manufacturer refurbished","New other (see details)","Used","For parts or not working"))
# levels(eBayTest1$condition) <- factor(c("New","Seller refurbished","Manufacturer refurbished","New other (see details)","Used","For parts or not working"))

eBayTrain1$biddable <- as.factor(eBayTrain1$biddable)
eBayTest1$biddable <- as.factor(eBayTest1$biddable)


#loading libraries
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(ROCR)
library(caret)
library(e1071)

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
bidTrain$price_index <- x #choose between x and y 4 places in total

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
nobidTrain$price_index <- x #choose between x and y 4 places in total

# train <- nobidTrain
# train$startprice <- NULL
# train$startprice_levels <- NULL
# 
# train <- train[,c(8,1:7,10)]

###### Logistic Regressions with bid/nobid subsets ######
print("dividing into bid/nobid subsets")

#biddable subset
bidTrainLR <- bidTrain
bidTrainLR$biddable <- NULL
# bidTrainLR$condition <- as.integer(bidTrain$condition)
# bidTrainLR$cellular <- as.integer(bidTrain$cellular)
# bidTrainLR$carrier <- as.integer(bidTrain$carrier)
# bidTrainLR$color <- as.integer(bidTrain$color)
# bidTrainLR$storage <- as.integer(bidTrain$storage)
# bidTrainLR$productline <- as.integer(bidTrain$productline)
bidTrainLR$startprice <- NULL
bidTrainLR$startprice_levels <- NULL

# LogBid <- glm(sold~.-UniqueID,data=bidTrainLR,family="binomial")
# AIC: 614.29
LogBid <- glm(sold~condition+productline+price_index+storage,data=bidTrainLR,family="binomial")
# AIC: 609.16
predLR_Bid <- predict(LogBid,type="response")
ROCRpred = prediction(predLR_Bid, bidTrainLR$sold)
print("LogReg BidTrain subset for Logistic Regression")
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8824292 all variables
# [1] 0.88083 simplified

#non-biddable subset
nobidTrainLR <- nobidTrain
nobidTrainLR$biddable <- NULL
# nobidTrainLR$condition <- as.integer(nobidTrain$condition)
# nobidTrainLR$cellular <- as.integer(nobidTrain$cellular)
# nobidTrainLR$carrier <- as.integer(nobidTrain$carrier)
# nobidTrainLR$color <- as.integer(nobidTrain$color)
# nobidTrainLR$storage <- as.integer(nobidTrain$storage)
# nobidTrainLR$productline <- as.integer(nobidTrain$productline)
nobidTrainLR$startprice <- NULL
nobidTrainLR$startprice_levels <- NULL

# LognoBid <- glm(sold~.-UniqueID,data=nobidTrainLR,family="binomial")
# AIC: 1034
LognoBid <- glm(sold~price_index,data=nobidTrainLR,family="binomial")
# AIC: 1027.7

predLR_noBid <- predict(LognoBid,type="response")
ROCRpred = prediction(predLR_noBid, nobidTrainLR$sold)
print("LogReg noBidTrain subset for Logistic Regression")
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
## using GBM

# its necessary for GBM that the dependent variable is an integer 0 or 1
# NOT a factor variable !!! 

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
# [1] 0.8827647 -> TEST AUC = 0.83667
# [1] 0.8729874 33% validation -> TEST AUC = 0.81235

## making the submission for GBM
# preparing the test set

nobidTest <- subset(eBayTest1,biddable==0)
bidTest <- subset(eBayTest1,biddable==1)


# biddable subset
x <- as.numeric((bidTest$startprice- price_table[as.numeric(bidTest$productline)])/price_vol_table[as.numeric(bidTest$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
# summary(y)
bidTest$price_index <- x #choose between x and y

bidTest$startprice <- NULL
bidTest$startprice_levels <- NULL
# bidTest$price_index <- NULL
predgbmBidTest<- predict.gbm(gbm_algo_bid,bidTest,n.trees=10000,type='response')

#non-biddable subset
x <- as.numeric((nobidTest$startprice- price_table_nobid[as.numeric(nobidTest$productline)])/price_vol_table_nobid[as.numeric(nobidTest$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
nobidTest$price_index <- x #choose between x and y

nobidTest$startprice <- NULL
nobidTest$startprice_levels <- NULL
# nobidTest$price_index <- NULL

## submitting GBM model
predgbmnoBidTest<- predict.gbm(gbm_algo_nobid,nobidTest,n.trees=10000,type='response')

# combining the 2 sets
temp1 <- cbind(bidTest$UniqueID,as.numeric(predgbmBidTest))
temp2 <- cbind(nobidTest$UniqueID,as.numeric(predgbmnoBidTest))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)
predGBM_Test <- ans

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission_GBM_bid_nobid.csv",row.names=FALSE)

## submitting the Logistic Regression bid/nobid model
#biddable subset
bidTestLR <- bidTest
bidTestLR$biddable <- NULL
# bidTestLR$condition <- as.integer(bidTest$condition)
# bidTestLR$cellular <- as.integer(bidTest$cellular)
# bidTestLR$carrier <- as.integer(bidTest$carrier)
# bidTestLR$color <- as.integer(bidTest$color)
# bidTestLR$storage <- as.integer(bidTest$storage)
# bidTestLR$productline <- as.integer(bidTest$productline)
bidTestLR$startprice <- NULL
bidTestLR$startprice_levels <- NULL

#non-biddable subset
nobidTestLR <- nobidTest
nobidTestLR$biddable <- NULL
nobidTestLR$condition <- as.integer(nobidTest$condition)
nobidTestLR$cellular <- as.integer(nobidTest$cellular)
nobidTestLR$carrier <- as.integer(nobidTest$carrier)
nobidTestLR$color <- as.integer(nobidTest$color)
nobidTestLR$storage <- as.integer(nobidTest$storage)
nobidTestLR$productline <- as.integer(nobidTest$productline)
nobidTestLR$startprice <- NULL
nobidTestLR$startprice_levels <- NULL

predBidTestLR<- predict(LogBid,newdata=bidTestLR,type='response')
prednoBidTestLR<- predict(LognoBid,newdata=nobidTestLR,type='response')

# combining the 2 sets
temp1 <- cbind(bidTest$UniqueID,as.numeric(predBidTestLR))
temp2 <- cbind(nobidTest$UniqueID,as.numeric(prednoBidTestLR))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)
pred_bid_nobid_TestLR <- ans

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission_LR_bid_nobid.csv",row.names=FALSE)

###### end of GBM with bid/nobid subsets#######

####### randomForest with bid/nobid subsets #######
set.seed(456)
# necessary for RandomForest
bidTrain$sold <- factor(bidTrain$sold)
nobidTrain$sold <- factor(nobidTrain$sold)

#biddable subset
bidForest <- randomForest(sold ~ .-UniqueID, data = bidTrain, ntree=200, nodesize=10 )
PredbidTrainForest = predict(bidForest,type="prob")[,2]

print("bidTrain subset AUC for Random Forest")
ROCRpred = prediction(PredbidTrainForest, bidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8669382
#non biddable subset
nobidForest <- randomForest(sold ~ .-UniqueID, data = nobidTrain, ntree=200, nodesize=10 )
PrednobidTrainForest = predict(nobidForest,type="prob")[,2]

print("nobidTrain subset AUC for Random Forest")
ROCRpred = prediction(PrednobidTrainForest, nobidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.6352362
# combining the 2 sets
temp1 <- cbind(bidTrain$UniqueID,as.numeric(PredbidTrainForest))
temp2 <- cbind(nobidTrain$UniqueID,as.numeric(PrednobidTrainForest))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)

print("whole Train set AUC for Random Forest")
ROCRpred = prediction(ans[,2], eBayTrain1$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8611965


####### trying with the whole set #######

print("")
print("now Trying directly with the whole set")

eBayTrain2 <- rbind(bidTrain,nobidTrain)
eBayTrain2 <- eBayTrain2[order(eBayTrain2[,"UniqueID"]),] #1852 obs 11 variables

## LogReg on the whole set (without BOW)

# LogReg <- glm(sold~. - UniqueID,data=eBayTrain2,family="binomial")
# AIC: 1720.8
LogReg <- glm(sold~biddable+condition+productline+storage+price_index+carrier,data=eBayTrain2,family="binomial")
# AIC: 1715.3
predLR <- predict(LogReg,type="response")
ROCRpred = prediction(predLR, eBayTrain2$sold)
print("LogReg AUC for the whole set")
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8712623 a little bit higher
# [1] 0.8692716 with simplified LogReg model





## GBM on whole set
set.seed(123)
# necessary for GBM that the dependent variable is integer 0 or 1
eBayTrain2$sold <- eBayTrain1$sold

gbm_algo <- gbm(sold~. -UniqueID,data=eBayTrain2,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

predgbmTrain<- predict.gbm(gbm_algo,eBayTrain2,n.trees=10000,type='response')

print("eBayTrain2 AUC for GBM")
ROCRpred = prediction(predgbmTrain, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# 0.8688399



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

## CART with cross-validation on the whole set

set.seed(123)
# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))

# Cross-validation
tr = train(sold ~ . -UniqueID, data = eBayTrain2, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

eBayTree_CV <- rpart(sold ~ . -UniqueID, data = eBayTrain2,method="class", cp = 0.01, minbucket=10)

best.tree.pred = predict(best.tree)[,2]
#best.tree.pred = predict(eBayTree_CV)

print("Train set AUC for CART with CV")
ROCRpred = prediction(best.tree.pred, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] [1] 0.8155713
# looks too simple, too poor


####### lets have fun with description #######

eBayTrain4 <- eBayTrain
#there is no such obs in the TEST set
#removing iPad mini Retina (8obs) and iPad5 (1obs). new set=1852 obs
eBayTrain4 <- subset(eBayTrain4,! ( (eBayTrain4$productline=="iPad mini Retina")|
                                            (eBayTrain4$productline=="iPad 5")) )
eBayTrain4$productline <- factor(eBayTrain4$productline)

library(tm)
corpus <- Corpus(VectorSource(eBayTrain4$description))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

spdtm= removeSparseTerms(dtm, 0.98)
eBayTrain_des <- as.data.frame(as.matrix(spdtm))
colnames(eBayTrain_des) = make.names(colnames(eBayTrain_des))


# eBayTrain_des$sold = as.factor(eBayTrain_des$sold)

library("wordcloud")
library("RColorBrewer")
# wordcloud(corpus, scale=c(2, .25))
wordcloud(colnames(eBayTrain_des), colSums(eBayTrain_des), rot.per=0.3, colors=palette(),scale=c(2, .25))

# adding new variables
eBayTrain3 <- eBayTrain2
#0.95 threshold
eBayTrain3$Dgood <- as.factor(eBayTrain_des$good)
eBayTrain3$Dnew <- as.factor(eBayTrain_des$new)
eBayTrain3$Dscratch <- as.factor(eBayTrain_des$scratch)
eBayTrain3$Dcondit <- as.factor(eBayTrain_des$condit)
eBayTrain3$Dscreen <- as.factor(eBayTrain_des$screen)
eBayTrain3$Duse <- as.factor(eBayTrain_des$use)
eBayTrain3$Dwork <- as.factor(eBayTrain_des$work)
eBayTrain3$Dipad <- as.factor(eBayTrain_des$ipad)

#0.98 threshold
eBayTrain3$Dbox <- as.factor(eBayTrain_des$box)
eBayTrain3$Dcase <- as.factor(eBayTrain_des$case)
eBayTrain3$Dinclud <- as.factor(eBayTrain_des$includ)
eBayTrain3$Dperfect <- as.factor(eBayTrain_des$perfect)
eBayTrain3$Dcrack <- as.factor(eBayTrain_des$crack)
eBayTrain3$Dexcel <- as.factor(eBayTrain_des$excel)

# LogReg3 <- glm(sold~. - UniqueID,data=eBayTrain3,family="binomial")
# AIC: 1757.5
# LogReg3 <- glm(sold~biddable+condition+productline+price_index+Dgood+Dscratch+Dscreen+Dwork+Dipad+Dcrack,data=eBayTrain3,family="binomial")
# AIC: 1724.8
# LogReg3 <- glm(sold~. - UniqueID-cellular-carrier-color-Dinclud-Dcase-Dipad-Dcondit-Dnew-Dperfect-Dgood-Duse-Dscreen-Dexcel-Dcrack,data=eBayTrain3,family="binomial")
# AIC: 1710.3
# LogReg3 <- glm(sold~. - UniqueID-cellular-carrier-color-Dinclud-Dcase-Dipad-Dcondit-Dnew-Dperfect-Dgood-Duse-Dscreen-Dexcel-Dcrack-Dscratch,data=eBayTrain3,family="binomial")
# AIC: 1708.2
LogReg3 <- glm(sold~biddable+condition+storage+productline+price_index+Dwork+Dbox,data=eBayTrain3,family="binomial")
# AIC: 1706.4

# the preferred model is the one with minimum AIC
# be careful: AIC can be compared only with same data set.

predLR3 <- predict(LogReg3,type="response")
ROCRpred = prediction(predLR3, eBayTrain3$sold)
print("LogReg3 AUC for the whole set")
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8848329 all variables -> TEST AUC 0.78668 overfitting ? !
#[1] 0.8767167 simplified -> TEST AUC 0.7921 ??
#[1] 0.8710559 LogReg3 

## preparing the Test Set for LogReg3
# prepare the Corpus for the Test Set

corpusTest <- Corpus(VectorSource(eBayTest$description))
corpusTest <- tm_map(corpusTest, content_transformer(tolower))
corpusTest = tm_map(corpusTest, PlainTextDocument)
corpusTest = tm_map(corpusTest, removePunctuation)
corpusTest = tm_map(corpusTest, removeWords, stopwords("english"))
corpusTest = tm_map(corpusTest, stemDocument)
dtmTest = DocumentTermMatrix(corpusTest)

spdtmTest= removeSparseTerms(dtmTest, 0.99)
eBayTest_des <- as.data.frame(as.matrix(spdtmTest))
colnames(eBayTest_des) = make.names(colnames(eBayTest_des))

eBayTest2 <- rbind(bidTest,nobidTest)
eBayTest2 <- eBayTest2[order(eBayTest2[,"UniqueID"]),] #798 obs 10 variables

eBayTest3 <- eBayTest2

#0.95 threshold
eBayTest3$Dgood <- as.factor(eBayTest_des$good)
eBayTest3$Dnew <- as.factor(eBayTest_des$new)
eBayTest3$Dscratch <- as.factor(eBayTest_des$scratch)
eBayTest3$Dcondit <- as.factor(eBayTest_des$condit)
eBayTest3$Dscreen <- as.factor(eBayTest_des$screen)
eBayTest3$Duse <- as.factor(eBayTest_des$use)
eBayTest3$Dwork <- as.factor(eBayTest_des$work)
eBayTest3$Dipad <- as.factor(eBayTest_des$ipad)

#0.98 threshold
eBayTest3$Dbox <- as.factor(eBayTest_des$box)
eBayTest3$Dcase <- as.factor(eBayTest_des$case)
eBayTest3$Dinclud <- as.factor(eBayTest_des$includ)
eBayTest3$Dperfect <- as.factor(eBayTest_des$perfect)
eBayTest3$Dcrack <- as.factor(eBayTest_des$crack)
eBayTest3$Dexcel <- as.factor(eBayTest_des$excel)

predLR3_Test <- predict(LogReg3,newdata=eBayTest3,type="response")

#submitting LogReg3 model
submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predLR3_Test)
write.csv(submission,"submission_LogReg3.csv",row.names=FALSE)

## GBM with BOW for the whole set
# necessary for GBM that the dependent variable is integer 0 or 1
eBayTrain3$sold <- eBayTrain1$sold 

gbm_bow_algo <- gbm(sold~biddable+condition+storage+productline+price_index+Dwork+Dbox,data=eBayTrain3,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)
predgbm_bow_Train<- predict.gbm(gbm_bow_algo,eBayTrain3,n.trees=10000,type='response')

print("eBayTrain3 (BOW) AUC for GBM")
ROCRpred = prediction(predgbm_bow_Train, eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8686181

#submitting GBM prediction files
predgbm_algo_Test <- predict.gbm(gbm_algo,eBayTest2,n.trees=10000,type='response')

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predgbm_algo_Test)
write.csv(submission,"submission_GBM.csv",row.names=FALSE)


predgbm_bow_Test<- predict.gbm(gbm_bow_algo,eBayTest3,n.trees=10000,type='response')

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predgbm_bow_Test)
write.csv(submission,"submission_GBM_BOW.csv",row.names=FALSE)



####### checking predictors consistency between each other ######
# predGBM_Test is the predictor for the best model so far
# GBM with split bid/nobid subsets
# pred_bid_nobid_TestLR[,2] 
# table((predLR3_Test>0.65) == (predGBM_Test[,2]>0.65))
# FALSE  TRUE 
# 157   641
# table((predLR3_Test>0.65), (predGBM_Test[,2]>0.65))
#      FALSE TRUE
# FALSE   384   18
# TRUE    139  257

# table((pred_bid_nobid_TestLR[,2]>0.65), (predGBM_Test[,2]>0.65))
# 
#        FALSE TRUE
# FALSE   518   11
# TRUE      5  264

# not submitted yet
# table((predLR3_Test>0.65), (predgbm_bow_Test>0.65))
#        FALSE TRUE
# FALSE   358   19
# TRUE     38  383