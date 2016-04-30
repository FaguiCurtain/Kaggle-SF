# Kmeans with 5 clusters

eBayTrain5 <- eBayTrain3


library(Hmisc)
cutsTrain <- cut2(eBayTrain5$startprice,g=4,onlycuts=TRUE)
eBayTrain5$pricequartile <- as.numeric(cut2(eBayTrain5$startprice,cuts=cutsTrain))
# eBayTrain3$pricequartile <- as.numeric(cut2(eBayTrain5$startprice,cuts=cutsTrain))

# eBayTrain5$startprice <- NULL
eBayTrain5$UniqueID <- NULL
eBayTrain5$sold <- NULL

eBayTrain5$biddable <- as.numeric(eBayTrain5$biddable)
eBayTrain5$condition <- as.numeric(eBayTrain5$condition)
eBayTrain5$storage <- as.numeric(eBayTrain5$storage)
eBayTrain5$productline <- as.numeric(eBayTrain5$storage)
eBayTrain5$Dexcel <- as.numeric(eBayTrain5$Dexcel)
eBayTrain5$Dscratch <- as.numeric(eBayTrain5$Dscratch)
eBayTrain5$pricequartile <- as.numeric(eBayTrain5$pricequartile)

eBayTrain5$cellular <- as.numeric(eBayTrain5$cellular)
# eBayTrain5$carrier <- as.numeric(eBayTrain5$carrier)
eBayTrain5$color <- as.numeric(eBayTrain5$color)

eBayTrain5mat <- as.matrix(eBayTrain5)
       
set.seed(144)
km = kmeans(eBayTrain5mat, centers = 5)

library(flexclust)
km.kcca <- as.kcca(km,eBayTrain5)
clusterTrain <- predict(km.kcca) #same as clusterTrain <-km$cluster


kmcluster1Train <- subset(eBayTrain3,clusterTrain==1) 
kmcluster2Train <- subset(eBayTrain3,clusterTrain==2) 
kmcluster3Train <- subset(eBayTrain3,clusterTrain==3) 
kmcluster4Train <- subset(eBayTrain3,clusterTrain==4)
kmcluster5Train <- subset(eBayTrain3,clusterTrain==5)


LogRegkmClust1 <- glm(sold~. -UniqueID,data=kmcluster1Train,family="binomial") #157 obs
LogRegkmClust2 <- glm(sold~. -UniqueID,data=kmcluster2Train,family="binomial") #368 obs
LogRegkmClust3 <- glm(sold~. -UniqueID,data=kmcluster3Train,family="binomial") #496 obs
LogRegkmClust4 <- glm(sold~. -UniqueID,data=kmcluster4Train,family="binomial") #390 obs
LogRegkmClust5 <- glm(sold~. -UniqueID,data=kmcluster5Train,family="binomial") #450 obs


predLR_kmC1 <- predict(LogRegkmClust1,type="response")
resLR_kmC1 <- table(kmcluster1Train$sold,predLR_kmC1>cutoff)
accLR_kmC1 <- (resLR_kmC1[1,1]+resLR_kmC1[2,2])/sum(resLR_kmC1)

predLR_kmC2 <- predict(LogRegkmClust2,type="response")
resLR_kmC2 <- table(kmcluster2Train$sold,predLR_kmC2>cutoff)
accLR_kmC2 <- (resLR_kmC2[1,1]+resLR_kmC2[2,2])/sum(resLR_kmC2)


predLR_kmC3 <- predict(LogRegkmClust3,type="response")
resLR_kmC3 <- table(kmcluster3Train$sold,predLR_kmC3>cutoff)
accLR_kmC3 <- (resLR_kmC3[1,1]+resLR_kmC3[2,2])/sum(resLR_kmC3)

predLR_kmC4 <- predict(LogRegkmClust4,type="response")
resLR_kmC4 <- table(kmcluster4Train$sold,predLR_kmC4>cutoff)
accLR_kmC4 <- (resLR_kmC4[1,1]+resLR_kmC4[2,2])/sum(resLR_kmC4)

predLR_kmC5 <- predict(LogRegkmClust5,type="response")
resLR_kmC5 <- table(kmcluster5Train$sold,predLR_kmC5>cutoff)
accLR_kmC5 <- (resLR_kmC5[1,1]+resLR_kmC5[2,2])/sum(resLR_kmC5)


#combine 5 results for the Training Set.

temp1 <- cbind(kmcluster1Train$UniqueID,as.numeric(predLR_kmC1))
temp2 <- cbind(kmcluster2Train$UniqueID,as.numeric(predLR_kmC2))
temp3 <- cbind(kmcluster3Train$UniqueID,as.numeric(predLR_kmC3))
temp4 <- cbind(kmcluster4Train$UniqueID,as.numeric(predLR_kmC4))
temp5 <- cbind(kmcluster5Train$UniqueID,as.numeric(predLR_kmC5))



temp <- rbind(temp1,temp2,temp3,temp4,temp5)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3);remove(temp4);remove(temp5)
remove(temp)

# Train set AUC 
library(ROCR)
print("Train set AUC for clustering")
ROCRpred = prediction(ans[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

#[1] 0.8972742

# Prepare the Test Set
eBayTest5 <- eBayTest3
eBayTest5$pricequartile <- as.numeric(cut2(eBayTest5$startprice,cuts=cutsTrain))
eBayTest5$UniqueID <- NULL
eBayTest5$description <- NULL
# eBayTest5$cellular <- NULL
eBayTest5$carrier <- NULL
# eBayTest5$color <- NULL

eBayTest5$biddable <- as.numeric(eBayTest5$biddable)
eBayTest5$condition <- as.numeric(eBayTest5$condition)
eBayTest5$storage <- as.numeric(eBayTest5$storage)
eBayTest5$productline <- as.numeric(eBayTest5$storage)
eBayTest5$Dexcel <- as.numeric(eBayTest5$Dexcel)
eBayTest5$Dscratch <- as.numeric(eBayTest5$Dscratch)
eBayTest5$pricequartile <- as.numeric(eBayTest5$pricequartile)

eBayTest5$cellular <- as.numeric(eBayTest5$cellular)
#eBayTest5$carrier <- as.numeric(eBayTest5$carrier)
eBayTest5$color <- as.numeric(eBayTest$color)

clusterTest <- predict(km.kcca,newdata=eBayTest5)

kmcluster1Test <- subset(eBayTest3,clusterTest==1) 
kmcluster2Test <- subset(eBayTest3,clusterTest==2) 
kmcluster3Test <- subset(eBayTest3,clusterTest==3) 
kmcluster4Test <- subset(eBayTest3,clusterTest==4)
kmcluster5Test <- subset(eBayTest3,clusterTest==5)

#ok jusque la

predTestLR_kmC1 <- predict(LogRegkmClust1,newdata=kmcluster1Test,type="response")
predTestLR_kmC2 <- predict(LogRegkmClust2,newdata=kmcluster2Test,type="response")
predTestLR_kmC3 <- predict(LogRegkmClust3,newdata=kmcluster3Test,type="response")
predTestLR_kmC4 <- predict(LogRegkmClust4,newdata=kmcluster4Test,type="response")
predTestLR_kmC5 <- predict(LogRegkmClust5,newdata=kmcluster5Test,type="response")

print("check")

temp1 <- cbind(kmcluster1Test$UniqueID,as.numeric(predTestLR_kmC1))
temp2 <- cbind(kmcluster2Test$UniqueID,as.numeric(predTestLR_kmC2))
temp3 <- cbind(kmcluster3Test$UniqueID,as.numeric(predTestLR_kmC3))
temp4 <- cbind(kmcluster4Test$UniqueID,as.numeric(predTestLR_kmC4))
temp5 <- cbind(kmcluster5Test$UniqueID,as.numeric(predTestLR_kmC5))

temp <- rbind(temp1,temp2,temp3,temp4,temp5)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3);remove(temp4);remove(temp5)
remove(temp)
# table with predTestLR3
# FALSE  TRUE 
# 59   739
print("check")

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission.csv",row.names=FALSE)



# TEST AUC = 0.83375