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
eBayTrain5$carrier <- as.numeric(eBayTrain5$carrier)
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
tr.control = trainControl(method = "cv", number = 5)

# cp values
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))

tr1 = train(sold ~ . -UniqueID, data = kmcluster1Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
#cp=0.5
tr2 = train(sold ~ . -UniqueID, data = kmcluster2Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
#cp=0.01
tr3 = train(sold ~ . -UniqueID, data = kmcluster3Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)
#cp=0.01

# tr4 = train(sold ~ . -UniqueID, data = kmcluster4Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=5)
# tr5 = train(sold ~ . -UniqueID, data = kmcluster5Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=5)

# Extract tree

eBayTree1_CV <- rpart(sold ~ . -UniqueID, data = kmcluster1Train,method="class", cp = 0.5, minbucket=10)
eBayTree2_CV <- rpart(sold ~ . -UniqueID, data = kmcluster2Train,method="class", cp = 0.01, minbucket=10)
eBayTree3_CV <- rpart(sold ~ . -UniqueID, data = kmcluster3Train,method="class", cp = 0.01, minbucket=10)

# prp(best.tree)
# 
# # Make predictions on training set
PredTree1_CV_Train = predict(eBayTree1_CV)[,2]
PredTree2_CV_Train = predict(eBayTree2_CV)[,2]
PredTree3_CV_Train = predict(eBayTree3_CV)[,2]

print("check")
resBestTree1 <- table(kmcluster1Train$sold,PredTree1_CV_Train>cutoff)
# accBestTree1 <- (resBestTree1[1,1]+resBestTree1[2,2])/sum(resBestTree1)
resBestTree2 <- table(kmcluster2Train$sold,PredTree2_CV_Train>cutoff)
accBestTree2 <- (resBestTree2[1,1]+resBestTree2[2,2])/sum(resBestTree2)
resBestTree3 <- table(kmcluster3Train$sold,PredTree3_CV_Train>cutoff)
accBestTree3 <- (resBestTree3[1,1]+resBestTree3[2,2])/sum(resBestTree3)

#combine 5 results for the Training Set.
 
temp1 <- cbind(kmcluster1Train$UniqueID,as.numeric(PredTree1_CV_Train))
temp2 <- cbind(kmcluster2Train$UniqueID,as.numeric(PredTree2_CV_Train))
temp3 <- cbind(kmcluster3Train$UniqueID,as.numeric(PredTree3_CV_Train))

temp <- rbind(temp1,temp2,temp3)
ansTrain <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3)# ;remove(temp4);remove(temp5)
remove(temp)

# Train set AUC 
# library(ROCR)
print("Train set AUC for clustering with RF")
ROCRpred = prediction(ansTrain[,2], eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# 
# #[1] 0.8393548
 
# Prepare the Test Set
eBayTest5 <- eBayTest3
eBayTest5$pricequartile <- as.numeric(cut2(eBayTest5$startprice,cuts=cutsTrain))
eBayTest5$UniqueID <- NULL
eBayTest5$description <- NULL
# eBayTest5$cellular <- NULL
# eBayTest5$carrier <- NULL
# eBayTest5$color <- NULL

eBayTest5$biddable <- as.numeric(eBayTest5$biddable)
eBayTest5$condition <- as.numeric(eBayTest5$condition)
eBayTest5$storage <- as.numeric(eBayTest5$storage)
eBayTest5$productline <- as.numeric(eBayTest5$storage)
eBayTest5$Dexcel <- as.numeric(eBayTest5$Dexcel)
eBayTest5$Dscratch <- as.numeric(eBayTest5$Dscratch)
eBayTest5$pricequartile <- as.numeric(eBayTest5$pricequartile)

eBayTest5$cellular <- as.numeric(eBayTest5$cellular)
eBayTest5$carrier <- as.numeric(eBayTest5$carrier)
eBayTest5$color <- as.numeric(eBayTest$color)

clusterTest <- predict(km.kcca,newdata=eBayTest5)

kmcluster1Test <- subset(eBayTest3,clusterTest==1) 
kmcluster2Test <- subset(eBayTest3,clusterTest==2) 
kmcluster3Test <- subset(eBayTest3,clusterTest==3) 

# # #ok jusque la

predTest_Tree_CV_kmC1 <- predict(eBayTree1_CV,newdata=kmcluster1Test,type="prob")
predTest_Tree_CV_kmC2 <- predict(eBayTree2_CV,newdata=kmcluster2Test,type="prob")
predTest_Tree_CV_kmC3 <- predict(eBayTree3_CV,newdata=kmcluster3Test,type="prob")

print("check")

temp1 <- cbind(kmcluster1Test$UniqueID,as.numeric(predTest_Tree_CV_kmC1[,2]))
temp2 <- cbind(kmcluster2Test$UniqueID,as.numeric(predTest_Tree_CV_kmC2[,2]))
temp3 <- cbind(kmcluster3Test$UniqueID,as.numeric(predTest_Tree_CV_kmC3[,2]))

temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3) #;remove(temp4);remove(temp5)
remove(temp)
# table with predTestLR3 table((predTestLR3>cutoff)==(ans[,2]>cutoff))
# FALSE  TRUE 
# 68   730 
print("check")
# # 
submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = ans[,2])
write.csv(submission,"submission.csv",row.names=FALSE)

# TEST AUC = 0.79167 
