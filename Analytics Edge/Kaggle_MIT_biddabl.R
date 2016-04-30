biddableTrain <- subset(eBayTrain2,biddable==1) #837 obs
nonbiddableTrain <- subset(eBayTrain2,biddable==0) #1024 obs


# LogRegbiddable <- glm(sold~. -UniqueID,data=biddableTrain,family="binomial")
# LogRegnonbiddable <- glm(sold~. -UniqueID,data=nonbiddableTrain,family="binomial")
# 
# predLogRegbiddable <- predict(LogRegbiddable,type="response")
# resLRbiddable <- table(biddableTrain$sold,predLogRegbiddable>cutoff)
# accLRbiddable <- (resLRbiddable[1,1]+resLRbiddable[2,2])/sum(resLRbiddable)
# 
# predLogRegnonbiddable <- predict(LogRegnonbiddable,type="response")
# resLRnonbiddable <- table(nonbiddableTrain$sold,predLogRegbiddable>cutoff)
# accLRnonbiddable <- (resLRnonbiddable[1,1]+resLRnonbiddable[2,2])/sum(resLRnonbiddable)                     

#necessary for Random Forest
biddableTrain$sold <- as.factor(biddableTrain$sold)
nonbiddableTrain$sold <- as.factor(nonbiddableTrain$sold)

set.seed(456)

eBay_bid_Forest = randomForest(sold ~ .-UniqueID, data = biddableTrain, ntree=1000, nodesize=10 )
eBay_nonbid_Forest = randomForest(sold ~ .-UniqueID, data = nonbiddableTrain, ntree=1000, nodesize=10 )


PredbidForest = predict(eBay_bid_Forest,type="prob")[,2]
resbidForest <- table(biddableTrain$sold, PredbidForest>cutoff)
accbidForest <- (resbidForest[1,1]+resbidForest[2,2])/sum(resbidForest)
#0.8255675 set.seed(123)
PrednonbidForest = predict(eBay_nonbid_Forest,type="prob")[,2]
resnonbidForest <- table(nonbiddableTrain$sold, PrednonbidForest>cutoff)
accnonbidForest <- (resnonbidForest[1,1]+resnonbidForest[2,2])/sum(resnonbidForest)
#0.7880859 set.seed(123)

eBayTest2 <- eBayTest
eBayTest2$biddable <- as.factor(eBayTest2$biddable)

eBayTest2$Dgood <- as.factor(eBayTest_des$good)
eBayTest2$Dnew <- as.factor(eBayTest_des$new)
eBayTest2$Dcondit <- as.factor(eBayTest_des$condit)
eBayTest2$Dscratch <- as.factor(eBayTest_des$scratch)
eBayTest2$Dscreen <- as.factor(eBayTest_des$screen)
eBayTest2$Duse <- as.factor(eBayTest_des$use)
eBayTest2$Dwork <- as.factor(eBayTest_des$work)
eBayTest2$Dipad <- as.factor(eBayTest_des$ipad)
eBayTest2$Dbox <- as.factor(eBayTest_des$box)
eBayTest2$Dcase <- as.factor(0)
eBayTest2$Dinclud<- as.factor(eBayTest_des$includ)
eBayTest2$Dperfect<- as.factor(0)
eBayTest2$Dcrack<- as.factor(eBayTest_des$crack)
eBayTest2$Dexcel <- as.factor(eBayTest_des$excel)

#if test set has less levels than in training set, then this is necessary
#levels(test$SectionName) <- levels(train$SectionName)
levels(eBayTest2$biddable) <- levels(eBayTrain2$biddable)
levels(eBayTest2$condition) <- levels(eBayTrain2$condition)
levels(eBayTest2$cellular) <- levels(eBayTrain2$cellular)
levels(eBayTest2$carrier) <- levels(eBayTrain2$carrier)
levels(eBayTest2$color) <- levels(eBayTrain2$color)
levels(eBayTest2$storage) <- levels(eBayTrain2$storage)
levels(eBayTest2$productline) <- levels(eBayTrain2$productline)

levels(eBayTest2$Dgood) <- levels(eBayTrain2$Dgood)
levels(eBayTest2$Dnew) <- levels(eBayTrain2$Dnew)
levels(eBayTest2$Dcondit) <- levels(eBayTrain2$Dcondit)
levels(eBayTest2$Dscratch) <- levels(eBayTrain2$Dscratch)
levels(eBayTest2$Dscreen) <- levels(eBayTrain2$Dscreen)
levels(eBayTest2$Duse) <- levels(eBayTrain2$Duse)
levels(eBayTest2$Dwork) <- levels(eBayTrain2$Dwork)
levels(eBayTest2$Dipad) <- levels(eBayTrain2$Dipad)
levels(eBayTest2$Dbox) <- levels(eBayTrain2$Dbox)
levels(eBayTest2$Dcase) <- levels(eBayTrain2$Dcase)
levels(eBayTest2$Dinclud) <- levels(eBayTrain2$Dinclud)
levels(eBayTest2$Dperfect) <- levels(eBayTrain2$Dperfect)
levels(eBayTest2$Dcrack) <- levels(eBayTrain2$Dcrack)
levels(eBayTest2$Dexcel) <- levels(eBayTrain2$Dexcel)

eBayTest2$description <- NULL

biddableTest <- subset(eBayTest2,biddable==1)
nonbiddableTest <- subset(eBayTest2,biddable==0)

predTest_bid_Forest <- predict(eBay_bid_Forest,newdata=biddableTest,type="prob")
predTest_nonbid_Forest <- predict(eBay_nonbid_Forest,newdata=nonbiddableTest,type="prob")

temp1 <- cbind(biddableTest$UniqueID,as.numeric(predTest_bid_Forest [,2]))
temp2 <- cbind(nonbiddableTest$UniqueID,as.numeric(predTest_nonbid_Forest [,2]))

temp <- rbind(temp1,temp2)
ans <- temp[order(temp[,1]),]
# remove(temp1); remove(temp2)
# remove(temp)

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission.csv",row.names=FALSE)

#TEST AUC 0.81598

#using GBM
#when CV.folds>0 i have the error message
#Error in object$var.levels[[i]] : subscript out of bounds
#means that when doing CV folds the number of levels is NOT the same
#class.stratify.cv=TRUE doesn't work to solve this
set.seed(999)
gbm_algo <- gbm(sold~. - UniqueID,data=eBayTrain2,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)
predictgbmTest <- predict.gbm(gbm_algo,eBayTest2,n.trees=10000,type='response')
predictgbmTrain<- predict.gbm(gbm_algo,eBayTrain2,n.trees=10000,type='response')
# Train set AUC 
library(ROCR)
print("Train set AUC for GBM")
ROCRpred = prediction(predictgbmTrain, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8630892

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predictgbmTest)
write.csv(submission,"submission.csv",row.names=FALSE)

#TEST SET AUC 0.81376

#using a 33% validation test
#1861*0.67=1247

gbm1 <- gbm(sold~. - UniqueID,data=eBayTrain2,distribution="bernoulli",shrinkage=0.001,n.trees=10000,train.fraction=0.67,cv.folds=0)
best.iter <- gbm.perf(gbm1,method="test")
summary(gbm1,n.trees=best.iter)
print("Train set AUC for GBM1")
predictgbm1Train<- predict.gbm(gbm1,eBayTrain2,n.trees=10000,type='response')
predictgbm1Test<- predict.gbm(gbm1,eBayTest2,n.trees=10000,type='response')
ROCRpred = prediction(predictgbm1Train, eBayTrain2$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predictgbm1Test)
write.csv(submission,"submission.csv",row.names=FALSE)