# k-means clustering k=3

cutoff=0.65

eBayTrain5 <- eBayTrain3

library(Hmisc)

# eBayTrain5$startprice <- NULL
eBayTrain5$UniqueID <- NULL
eBayTrain5$sold <- NULL

eBayTrain5$cellular <- NULL
eBayTrain5$carrier <- NULL
eBayTrain5$color <- NULL

eBayTrain5$Dgood <- NULL
eBayTrain5$Dnew<- NULL
eBayTrain5$Dcondit<- NULL
eBayTrain5$Duse<- NULL
eBayTrain5$Dipad<- NULL
eBayTrain5$Dcase<- NULL
eBayTrain5$Dinclud<- NULL
eBayTrain5$Dperfect<- NULL
eBayTrain5$Dcrack<- NULL
eBayTrain5$Dexcel<- NULL

eBayTrain5$description <- as.numeric(eBayTrain5$description)
eBayTrain5$biddable <- as.numeric(eBayTrain5$biddable)
eBayTrain5$condition <- as.numeric(eBayTrain5$condition)
eBayTrain5$storage <- as.numeric(eBayTrain5$storage)
eBayTrain5$productline <- as.numeric(eBayTrain5$storage)
eBayTrain5$Dwork <- 0.5* as.numeric(eBayTrain5$Dwork)
eBayTrain5$Dscratch <- 0.5* as.numeric(eBayTrain5$Dscratch)
eBayTrain5$Dscreen <- 0.5* as.numeric(eBayTrain5$Dscreen)
eBayTrain5$Dbox <- 0.5* as.numeric(eBayTrain5$Dbox)

eBayTrain5$price_index <- eBayTrain5$price_index

eBayTrain5mat <- as.matrix(eBayTrain5)
       
set.seed(144)
km = kmeans(eBayTrain5mat, centers = 3)

library(flexclust)
km.kcca <- as.kcca(km,eBayTrain5)
clusterTrain <- predict(km.kcca) #same as clusterTrain <-km$cluster


kmcluster1Train <- subset(eBayTrain3,clusterTrain==1) #484obs
kmcluster2Train <- subset(eBayTrain3,clusterTrain==2) #868obs
kmcluster3Train <- subset(eBayTrain3,clusterTrain==3) #500obs

## Logistic Regression

# LogRegkmClust1 <- glm(sold~. -UniqueID,data=kmcluster1Train,family="binomial")
# AIC: 472.17
LogRegkmClust1 <- glm(sold~biddable+condition+storage+productline+price_index+Dgood+Dcondit,data=kmcluster1Train,family="binomial")
# AIC: 434.89

# LogRegkmClust2 <- glm(sold~. -UniqueID,data=kmcluster2Train,family="binomial")
# AIC: 743.49
LogRegkmClust2 <- glm(sold~biddable+condition+productline+price_index+Dwork,data=kmcluster2Train,family="binomial")
# AIC: 706.54

# LogRegkmClust3 <- glm(sold~. -UniqueID,data=kmcluster3Train,family="binomial")
# AIC: 539.34
LogRegkmClust3 <- glm(sold~biddable+condition+productline+price_index+Dwork+Dipad+Dbox,data=kmcluster3Train,family="binomial")
# AIC: 501.74

predLR_kmC1 <- predict(LogRegkmClust1,type="response")
resLR_kmC1 <- table(kmcluster1Train$sold,predLR_kmC1>cutoff)
accLR_kmC1 <- (resLR_kmC1[1,1]+resLR_kmC1[2,2])/sum(resLR_kmC1)

predLR_kmC2 <- predict(LogRegkmClust2,type="response")
resLR_kmC2 <- table(kmcluster2Train$sold,predLR_kmC2>cutoff)
accLR_kmC2 <- (resLR_kmC2[1,1]+resLR_kmC2[2,2])/sum(resLR_kmC2)


predLR_kmC3 <- predict(LogRegkmClust3,type="response")
resLR_kmC3 <- table(kmcluster3Train$sold,predLR_kmC3>cutoff)
accLR_kmC3 <- (resLR_kmC3[1,1]+resLR_kmC3[2,2])/sum(resLR_kmC3)

#combine 3 results for the Training Set.

temp1 <- cbind(kmcluster1Train$UniqueID,as.numeric(predLR_kmC1))
temp2 <- cbind(kmcluster2Train$UniqueID,as.numeric(predLR_kmC2))
temp3 <- cbind(kmcluster3Train$UniqueID,as.numeric(predLR_kmC3))

temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3);remove(temp)

# Train set AUC 
library(ROCR)
print("Train set AUC for clustering + LogReg")
ROCRpred = prediction(ans[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

## GBM
gbm1 <- gbm(sold~. - UniqueID,data=kmcluster1Train,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)
gbm2 <- gbm(sold~. - UniqueID,data=kmcluster2Train,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)
gbm3 <- gbm(sold~. - UniqueID,data=kmcluster3Train,distribution="bernoulli",shrinkage=0.001,n.trees=10000,cv.folds=0)

predgbm1 <- predict.gbm(gbm1,kmcluster1Train,n.trees=10000,type='response')
predgbm2 <- predict.gbm(gbm1,kmcluster2Train,n.trees=10000,type='response')
predgbm3 <- predict.gbm(gbm1,kmcluster3Train,n.trees=10000,type='response')

#combine 3 results for the Training Set.

temp1 <- cbind(kmcluster1Train$UniqueID,as.numeric(predgbm1))
temp2 <- cbind(kmcluster2Train$UniqueID,as.numeric(predgbm2))
temp3 <- cbind(kmcluster3Train$UniqueID,as.numeric(predgbm3))

temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3);remove(temp)

# Train set AUC 
library(ROCR)
print("Train set AUC for clustering + GBM")
ROCRpred = prediction(ans[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))


# Prepare the Test Set
eBayTest5 <- eBayTest3

eBayTest5$UniqueID <- NULL
eBayTest5$sold <- NULL

eBayTest5$cellular <- NULL
eBayTest5$carrier <- NULL
eBayTest5$color <- NULL

eBayTest5$Dgood <- NULL
eBayTest5$Dnew<- NULL
eBayTest5$Dcondit<- NULL
eBayTest5$Duse<- NULL
eBayTest5$Dipad<- NULL
eBayTest5$Dcase<- NULL
eBayTest5$Dinclud<- NULL
eBayTest5$Dperfect<- NULL
eBayTest5$Dcrack<- NULL
eBayTest5$Dexcel<- NULL

eBayTest5$description <- as.numeric(eBayTest5$description)
eBayTest5$biddable <- as.numeric(eBayTest5$biddable)
eBayTest5$condition <- as.numeric(eBayTest5$condition)
eBayTest5$storage <- as.numeric(eBayTest5$storage)
eBayTest5$productline <- as.numeric(eBayTest5$storage)
eBayTest5$Dwork <- 0.5* as.numeric(eBayTest5$Dwork)
eBayTest5$Dscratch <- 0.5* as.numeric(eBayTest5$Dscratch)
eBayTest5$Dscreen <- 0.5* as.numeric(eBayTest5$Dscreen)
eBayTest5$Dbox <- 0.5* as.numeric(eBayTest5$Dbox)

##

clusterTest <- predict(km.kcca,newdata=eBayTest5)

kmcluster1Test <- subset(eBayTest3,clusterTest==1) 
kmcluster2Test <- subset(eBayTest3,clusterTest==2) 
kmcluster3Test <- subset(eBayTest3,clusterTest==3) 

predTestLR_kmC1 <- predict(LogRegkmClust1,newdata=kmcluster1Test,type="response")
predTestLR_kmC2 <- predict(LogRegkmClust2,newdata=kmcluster2Test,type="response")
predTestLR_kmC3 <- predict(LogRegkmClust3,newdata=kmcluster3Test,type="response")

temp1 <- cbind(kmcluster1Test$UniqueID,as.numeric(predTestLR_kmC1))
temp2 <- cbind(kmcluster2Test$UniqueID,as.numeric(predTestLR_kmC2))
temp3 <- cbind(kmcluster3Test$UniqueID,as.numeric(predTestLR_kmC3))

temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3)
remove(temp)
# table with predTestLR3
# FALSE  TRUE 
# 36   762 

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission_cluster.csv",row.names=FALSE)


