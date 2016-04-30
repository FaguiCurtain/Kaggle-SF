eBayTrain4 <- eBayTrain3


library(Hmisc)
eBayTrain4$pricequartile <- as.factor(cut2(eBayTrain4$startprice,g=4))
eBayTrain4$startprice <- NULL
eBayTrain4$UniqueID <- NULL
eBayTrain4$sold <- NULL

a <- model.matrix(~ . + 0, data=eBayTrain4, contrasts.arg = lapply(eBayTrain4, contrasts, contrasts=FALSE))
str(a)



# LogReg4 <- glm(sold~. -UniqueID,data=eBayTrain4,family="binomial")
# predLR4 <- predict(LogReg4,type="response")
# resLR4 <- table(eBayTrain4$sold,predLR4>cutoff)
# accLR4 <- (resLR4[1,1]+resLR4[2,2])/sum(resLR4)

# convert factor variables

# Compute distances


distances = dist(a, method = "euclidean")

# Hierarchical clustering
clustereBay = hclust(distances, method = "ward.D") 
plot(clustereBay)
# Assign points to clusters
clusterGroups = cutree(clustereBay, k = 3)

# put the variables again
eBayTrain4$UniqueID <- eBayTrain3$UniqueID
eBayTrain4$sold <- eBayTrain3$sold

Cluster1Train <- subset(eBayTrain4,clusterGroups==1)
Cluster2Train <- subset(eBayTrain4,clusterGroups==2)
Cluster3Train <- subset(eBayTrain4,clusterGroups==3)
#for all elements in Cluster3Train, biddable=0,DScratch=0
Cluster3Train$biddable <- NULL
Cluster3Train$Dscratch <- NULL

LogRegClust1 <- glm(sold~. -UniqueID,data=Cluster1Train,family="binomial")
LogRegClust2 <- glm(sold~. -UniqueID,data=Cluster2Train,family="binomial")
LogRegClust3 <- glm(sold~. -UniqueID,data=Cluster3Train,family="binomial")

predLR_C1 <- predict(LogRegClust1,type="response")
resLR_C1 <- table(Cluster1Train$sold,predLR_C1>cutoff)
accLR_C1 <- (resLR_C1[1,1]+resLR_C1[2,2])/sum(resLR_C1)

predLR_C2 <- predict(LogRegClust2,type="response")
resLR_C2 <- table(Cluster2Train$sold,predLR_C2>cutoff)
accLR_C2 <- (resLR_C2[1,1]+resLR_C2[2,2])/sum(resLR_C2)


predLR_C3 <- predict(LogRegClust3,type="response")
resLR_C3 <- table(Cluster3Train$sold,predLR_C3>cutoff)
accLR_C3 <- (resLR_C3[1,1]+resLR_C3[2,2])/sum(resLR_C3)

#focus on Cluster1
Cluster1Train$pricequartile <- NULL
Cluster1Train$startprice <- subset(eBayTrain3,clusterGroups==1)$startprice
#Random Forest for Cluster1
library(randomForest)
set.seed(123)
eBayForest_C1 = randomForest(sold ~ .-UniqueID, data = Cluster1Train,node=5,ntree=500 )
PredForest_C1 = predict(eBayForest_C1)
resForest_C1 <- table(Cluster1Train$sold, PredForest_C1>cutoff)
accForest_C1 <- (resForest_C1[1,1]+resForest_C1[2,2])/sum(resForest_C1)

#CART for Cluster1
# Regression Tree with cross-validation
# Load libraries for cross-validation
library(caret)
library(e1071)
# Number of folds
tr.control = trainControl(method = "cv", number = 10)
# cp values
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))
# What did we just do?
1*0.001 
10*0.001 
0:10
0:10 * 0.001

# Cross-validation
tr_C1 = train(sold ~ . -UniqueID, data = Cluster1Train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)

# Extract tree
best.tree_C1 = tr_C1$finalModel
prp(best.tree_C1)

# Make predictions
best.tree.pred_C1 = predict(best.tree_C1)
resBestTree_C1 <- table(Cluster1Train$sold,best.tree.pred_C1>cutoff)
accBestTree_C1 <- (resBestTree_C1[1,1]+resBestTree_C1[2,2])/sum(resBestTree_C1)

#combine 3 results for the Training Set.

temp1 <- cbind(Cluster1Train$UniqueID,as.numeric(predLR_C1))
temp2 <- cbind(Cluster2Train$UniqueID,as.numeric(predLR_C2))
temp3 <- cbind(Cluster3Train$UniqueID,as.numeric(predLR_C3))

temp <- rbind(temp1,temp2,temp3)
ans <- temp[order(temp[,1]),]
remove(temp1); remove(temp2);remove(temp3);remove(temp)

# Train set AUC 
library(ROCR)
print("Train set AUC for clustering")
ROCRpred = prediction(ans[,2], eBayTrain4$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))