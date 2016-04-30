#compile this *after* Kaggle_new1

print("bid/no bid BOW")

eBayTrain5 <- eBayTrain1

#0.95 threshold
eBayTrain5$Dgood <- as.factor(eBayTrain_des$good)
eBayTrain5$Dnew <- as.factor(eBayTrain_des$new)
eBayTrain5$Dscratch <- as.factor(eBayTrain_des$scratch)
eBayTrain5$Dcondit <- as.factor(eBayTrain_des$condit)
eBayTrain5$Dscreen <- as.factor(eBayTrain_des$screen)
eBayTrain5$Duse <- as.factor(eBayTrain_des$use)
eBayTrain5$Dwork <- as.factor(eBayTrain_des$work)
eBayTrain5$Dipad <- as.factor(eBayTrain_des$ipad)

#0.98 threshold
eBayTrain5$Dbox <- as.factor(eBayTrain_des$box)
eBayTrain5$Dcase <- as.factor(eBayTrain_des$case)
eBayTrain5$Dinclud <- as.factor(eBayTrain_des$includ)
eBayTrain5$Dperfect <- as.factor(eBayTrain_des$perfect)
eBayTrain5$Dcrack <- as.factor(eBayTrain_des$crack)
eBayTrain5$Dexcel <- as.factor(eBayTrain_des$excel)

eBayTrain5$startprice_levels <- cut(eBayTrain5$startprice,breaks=25*(0:40))


nobidTrain <- subset(eBayTrain5,biddable==0)
bidTrain <- subset(eBayTrain5,biddable==1)

# nobidBOWTrain_1 <- aggregate(sold~productline+startprice_levels,data=nobidBOWTrain,FUN="mean")
# bidBOWTrain_1 <- aggregate(sold~productline+startprice_levels,data=bidBOWTrain,FUN="mean")

# bid_sold <- subset(bidTrain,sold==1)
# price_table <- tapply(bid_sold$startprice,bid_sold$productline,mean)
# price_vol_table <- tapply(bid_sold$startprice,bid_sold$productline,sd)

# x <- as.numeric((eBayTrain1$startprice- price_table[as.numeric(eBayTrain1$productline)])/price_vol_table[as.numeric(eBayTrain1$productline)])

x <- as.numeric((bidTrain$startprice- price_table[as.numeric(bidTrain$productline)])/price_vol_table[as.numeric(bidTrain$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
# summary(y)
bidTrain$price_index <- x #choose between x and y 4 places in total
# 
# nobid_sold <- subset(nobidTrain,sold==1)
# price_table_nobid <- tapply(nobid_sold$startprice,nobid_sold$productline,mean)
# price_vol_table_nobid <- tapply(nobid_sold$startprice,nobid_sold$productline,sd)

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

# LogBOWBid <- glm(sold~.-UniqueID,data=bidTrainLR,family="binomial")
# AIC : 644.19
LogBOWBid <- glm(sold~condition+storage+color+Dscratch+price_index,data=bidTrainLR,family="binomial")
# AIC: 610.81
predLR_Bid <- predict(LogBOWBid,type="response")
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

# LogBOWnoBid <- glm(sold~.-UniqueID,data=nobidTrainLR,family="binomial")
# AIC: 1082
LogBOWnoBid <- glm(sold~productline+price_index+Dscreen+Dwork+Dbox,data=nobidTrainLR,family="binomial")
# AIC: 1030.7 

predLR_noBid <- predict(LogBOWnoBid,type="response")
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
# gbm_BOW_algo_bid <- gbm(sold~. - UniqueID,data=bidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)
gbm_BOW_algo_bid <- gbm(sold~condition+storage+color+Dscratch+price_index,data=bidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

predgbmBidTrain<- predict.gbm(gbm_BOW_algo_bid,bidTrain,n.trees=10000,type='response')
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
gbm_BOW_algo_nobid <- gbm(sold~productline+price_index+Dscreen+Dwork+Dbox,data=nobidTrain,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

predgbmnoBidTrain<- predict.gbm(gbm_BOW_algo_nobid,nobidTrain,n.trees=10000,type='response')

# Train set AUC 
print("nobidTrain subset AUC for GBM")
ROCRpred = prediction(predgbmnoBidTrain, nobidTrain$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.6919297

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

#0.95 threshold
eBayTest1$Dgood <- as.factor(eBayTest_des$good)
eBayTest1$Dnew <- as.factor(eBayTest_des$new)
eBayTest1$Dscratch <- as.factor(eBayTest_des$scratch)
eBayTest1$Dcondit <- as.factor(eBayTest_des$condit)
eBayTest1$Dscreen <- as.factor(eBayTest_des$screen)
eBayTest1$Duse <- as.factor(eBayTest_des$use)
eBayTest1$Dwork <- as.factor(eBayTest_des$work)
eBayTest1$Dipad <- as.factor(eBayTest_des$ipad)

#0.98 threshold
eBayTest1$Dbox <- as.factor(eBayTest_des$box)
eBayTest1$Dcase <- as.factor(eBayTest_des$case)
eBayTest1$Dinclud <- as.factor(eBayTest_des$includ)
eBayTest1$Dperfect <- as.factor(eBayTest_des$perfect)
eBayTest1$Dcrack <- as.factor(eBayTest_des$crack)
eBayTest1$Dexcel <- as.factor(eBayTest_des$excel)

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


#non-biddable subset
x <- as.numeric((nobidTest$startprice- price_table_nobid[as.numeric(nobidTest$productline)])/price_vol_table_nobid[as.numeric(nobidTest$productline)])
y <- cut(x,breaks=c(-10,-2,-1.5,-1,-0.5,-0.25,0,0.25,0.5,1,1.5,2,3,5,8,20))
nobidTest$price_index <- x #choose between x and y

nobidTest$startprice <- NULL
nobidTest$startprice_levels <- NULL
# nobidTest$price_index <- NULL

## submitting GBM model
predgbmBidTest<-   predict.gbm(gbm_BOW_algo_bid,bidTest,n.trees=10000,type='response')
predgbmnoBidTest<- predict.gbm(gbm_BOW_algo_nobid,nobidTest,n.trees=10000,type='response')

# combining the 2 sets
temp1 <- cbind(bidTest$UniqueID,as.numeric(predgbmBidTest))
temp2 <- cbind(nobidTest$UniqueID,as.numeric(predgbmnoBidTest))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)
predGBM_BOW_Test <- ans

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission_GBM_BOW_bid_nobid.csv",row.names=FALSE)

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
# nobidTestLR$condition <- as.integer(nobidTest$condition)
# nobidTestLR$cellular <- as.integer(nobidTest$cellular)
# nobidTestLR$carrier <- as.integer(nobidTest$carrier)
# nobidTestLR$color <- as.integer(nobidTest$color)
# nobidTestLR$storage <- as.integer(nobidTest$storage)
# nobidTestLR$productline <- as.integer(nobidTest$productline)
# nobidTestLR$startprice <- NULL
nobidTestLR$startprice_levels <- NULL

predBidTestLR<- predict(LogBOWBid,newdata=bidTestLR,type='response')
prednoBidTestLR<- predict(LogBOWnoBid,newdata=nobidTestLR,type='response')

# combining the 2 sets
temp1 <- cbind(bidTest$UniqueID,as.numeric(predBidTestLR))
temp2 <- cbind(nobidTest$UniqueID,as.numeric(prednoBidTestLR))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp)
pred_BOW_bid_nobid_TestLR <- ans

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission_LR_BOW_bid_nobid.csv",row.names=FALSE)

#TEST AUC 0.82682

###### end of GBM with bid/nobid subsets#######

###### testing against reference model ######
# table((pred_BOW_bid_nobid_TestLR[,2]>0.65), (predGBM_Test[,2]>0.65))
# 
#         FALSE TRUE
# FALSE   510    9
# TRUE     13  266

# table((predGBM_BOW_Test[,2]>0.65), (predGBM_Test[,2]>0.65))

#         FALSE TRUE
# FALSE   515    2
# TRUE      8  273