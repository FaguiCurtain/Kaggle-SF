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
km = kmeans(eBayTrain5mat, centers = 3)

library(flexclust)

km.kcca <- as.kcca(km,eBayTrain5)
clusterTrain <- predict(km.kcca) #same as clusterTrain <-km$cluster

eBayTrain3$sold <- as.factor(eBayTrain3$sold) #necessary for CART/RF

kmcluster1Train <- subset(eBayTrain3,clusterTrain==1) 
kmcluster2Train <- subset(eBayTrain3,clusterTrain==2) 
kmcluster3Train <- subset(eBayTrain3,clusterTrain==3) 
# kmcluster4Train <- subset(eBayTrain3,clusterTrain==4)
# kmcluster5Train <- subset(eBayTrain3,clusterTrain==5)

#make the CART with CV model



set.seed(789)
# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))

tr1 = train(sold ~ . -UniqueID, data = kmcluster1Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
tr2 = train(sold ~ . -UniqueID, data = kmcluster2Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
tr3 = train(sold ~ . -UniqueID, data = kmcluster3Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
# tr4 = train(sold ~ . -UniqueID, data = kmcluster4Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=5)
# tr5 = train(sold ~ . -UniqueID, data = kmcluster5Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=5)

# Extract tree
best.tree1 = tr1$finalModel
best.tree2 = tr2$finalModel
best.tree3 = tr3$finalModel
# best.tree4 = tr4$finalModel
# best.tree5 = tr5$finalModel

# prp(best.tree)

# Make predictions on training set
best.tree.pred1 = predict(best.tree1)
best.tree.pred2 = predict(best.tree2)
best.tree.pred3 = predict(best.tree3)
# best.tree.pred4 = predict(best.tree4)
# best.tree.pred5 = predict(best.tree5)

print("check")

resBestTree1 <- table(kmcluster1Train$sold,best.tree.pred1[,2]>cutoff)
#accBestTree1 <- (resBestTree1[1,1]+resBestTree1[2,2])/sum(resBestTree1)
resBestTree2 <- table(kmcluster2Train$sold,best.tree.pred2[,2]>cutoff)
#accBestTree2 <- (resBestTree2[1,1]+resBestTree2[2,2])/sum(resBestTree2)
resBestTree3 <- table(kmcluster3Train$sold,best.tree.pred3[,2]>cutoff)
#accBestTree3 <- (resBestTree3[1,1]+resBestTree3[2,2])/sum(resBestTree3)
# resBestTree4 <- table(kmcluster4Train$sold,best.tree.pred4[,2]>cutoff)
# accBestTree4 <- (resBestTree4[1,1]+resBestTree4[2,2])/sum(resBestTree4)
# resBestTree5 <- table(kmcluster5Train$sold,best.tree.pred5[,2]>cutoff)
# accBestTree5 <- (resBestTree5[1,1]+resBestTree5[2,2])/sum(resBestTree5)

#combine 5 results for the Training Set.

temp1 <- cbind(kmcluster1Train$UniqueID,as.numeric(best.tree.pred1[,2]))
temp2 <- cbind(kmcluster2Train$UniqueID,as.numeric(best.tree.pred2[,2]))
temp3 <- cbind(kmcluster3Train$UniqueID,as.numeric(best.tree.pred3[,2]))
# temp4 <- cbind(kmcluster4Train$UniqueID,as.numeric(best.tree.pred4[,2]))
# temp5 <- cbind(kmcluster5Train$UniqueID,as.numeric(best.tree.pred5[,2]))



# temp <- rbind(temp1,temp2,temp3,temp4,temp5)
temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3)# ;remove(temp4);remove(temp5)
remove(temp)

# Train set AUC 
library(ROCR)
print("Train set AUC for clustering with RF")
ROCRpred = prediction(ans[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

#[1] 0.8393548
# 
# # Prepare the Test Set
# eBayTest5 <- eBayTest3
# eBayTest5$pricequartile <- as.numeric(cut2(eBayTest5$startprice,cuts=cutsTrain))
# eBayTest5$UniqueID <- NULL
# eBayTest5$description <- NULL
# # eBayTest5$cellular <- NULL
# eBayTest5$carrier <- NULL
# # eBayTest5$color <- NULL
# 
# eBayTest5$biddable <- as.numeric(eBayTest5$biddable)
# eBayTest5$condition <- as.numeric(eBayTest5$condition)
# eBayTest5$storage <- as.numeric(eBayTest5$storage)
# eBayTest5$productline <- as.numeric(eBayTest5$storage)
# eBayTest5$Dexcel <- as.numeric(eBayTest5$Dexcel)
# eBayTest5$Dscratch <- as.numeric(eBayTest5$Dscratch)
# eBayTest5$pricequartile <- as.numeric(eBayTest5$pricequartile)
# 
# eBayTest5$cellular <- as.numeric(eBayTest5$cellular)
# #eBayTest5$carrier <- as.numeric(eBayTest5$carrier)
# eBayTest5$color <- as.numeric(eBayTest$color)
# 
# clusterTest <- predict(km.kcca,newdata=eBayTest5)
# 
# kmcluster1Test <- subset(eBayTest3,clusterTest==1) 
# kmcluster2Test <- subset(eBayTest3,clusterTest==2) 
# kmcluster3Test <- subset(eBayTest3,clusterTest==3) 
# kmcluster4Test <- subset(eBayTest3,clusterTest==4)
# kmcluster5Test <- subset(eBayTest3,clusterTest==5)
# 
# #ok jusque la
# 
# predTestLR_kmC1 <- predict(LogRegkmClust1,newdata=kmcluster1Test,type="response")
# predTestLR_kmC2 <- predict(LogRegkmClust2,newdata=kmcluster2Test,type="response")
# predTestLR_kmC3 <- predict(LogRegkmClust3,newdata=kmcluster3Test,type="response")
# predTestLR_kmC4 <- predict(LogRegkmClust4,newdata=kmcluster4Test,type="response")
# predTestLR_kmC5 <- predict(LogRegkmClust5,newdata=kmcluster5Test,type="response")
# 
# print("check")
# 
# temp1 <- cbind(kmcluster1Test$UniqueID,as.numeric(predTestLR_kmC1))
# temp2 <- cbind(kmcluster2Test$UniqueID,as.numeric(predTestLR_kmC2))
# temp3 <- cbind(kmcluster3Test$UniqueID,as.numeric(predTestLR_kmC3))
# temp4 <- cbind(kmcluster4Test$UniqueID,as.numeric(predTestLR_kmC4))
# temp5 <- cbind(kmcluster5Test$UniqueID,as.numeric(predTestLR_kmC5))
# 
# temp <- rbind(temp1,temp2,temp3,temp4,temp5)
# ans <- temp[order(temp[,1]),]
# remove(temp1); remove(temp2);remove(temp3);remove(temp4);remove(temp5)
# remove(temp)
# # table with predTestLR3 table((predTestLR3>cutoff)==(ans[,2]>cutoff))
# # FALSE  TRUE 
# # 59   739
# print("check")
# 
# submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
# write.csv(submission,"submission.csv",row.names=FALSE)
# 
# # TEST AUC = 0.83375 taking out carrier, color,cellular
