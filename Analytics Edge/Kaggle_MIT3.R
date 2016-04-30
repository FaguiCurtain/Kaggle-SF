# k-means clustering k=3

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

eBayTrain5mat <- as.matrix(eBayTrain5)
       
set.seed(144)
km = kmeans(eBayTrain5mat, centers = 3)

library(flexclust)
km.kcca <- as.kcca(km,eBayTrain5)
clusterTrain <- predict(km.kcca) #same as clusterTrain <-km$cluster


kmcluster1Train <- subset(eBayTrain3,clusterTrain==1) #228obs
kmcluster2Train <- subset(eBayTrain3,clusterTrain==2) #686obs
kmcluster3Train <- subset(eBayTrain3,clusterTrain==3) #947obs

LogRegkmClust1 <- glm(sold~. -UniqueID,data=kmcluster1Train,family="binomial")
LogRegkmClust2 <- glm(sold~. -UniqueID,data=kmcluster2Train,family="binomial")
LogRegkmClust3 <- glm(sold~. -UniqueID,data=kmcluster3Train,family="binomial")

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
print("Train set AUC for clustering")
ROCRpred = prediction(ans[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))


# Prepare the Test Set
eBayTest5 <- eBayTest3
eBayTest5$pricequartile <- as.numeric(cut2(eBayTest5$startprice,cuts=cutsTrain))
eBayTest5$UniqueID <- NULL
eBayTest5$description <- NULL
eBayTest5$cellular <- NULL
eBayTest5$carrier <- NULL
eBayTest5$color <- NULL

eBayTest5$biddable <- as.numeric(eBayTest5$biddable)
eBayTest5$condition <- as.numeric(eBayTest5$condition)
eBayTest5$storage <- as.numeric(eBayTest5$storage)
eBayTest5$productline <- as.numeric(eBayTest5$storage)
eBayTest5$Dexcel <- as.numeric(eBayTest5$Dexcel)
eBayTest5$Dscratch <- as.numeric(eBayTest5$Dscratch)
eBayTest5$pricequartile <- as.numeric(eBayTest5$pricequartile)

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
remove(temp1); remove(temp2);remove(temp3);remove(temp4);remove(temp5)
remove(temp)
# table with predTestLR3
# FALSE  TRUE 
# 36   762 

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission.csv",row.names=FALSE)


