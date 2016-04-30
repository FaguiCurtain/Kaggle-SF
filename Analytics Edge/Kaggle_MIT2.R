eBayTrainB <- eBayTrain
eBayTrainB$sold <- NULL

eBayAll <- rbind(eBayTrainB,eBayTest)
AllID <- eBayAll$UniqueID

library(Hmisc)
eBayAll$pricequartile <- as.factor(cut2(eBayAll$startprice,g=4))
eBayAll$startprice <- NULL
eBayAll$UniqueID <- NULL
eBayAll$sold <- NULL
eBayAll$biddable <- as.factor(eBayAll$biddable)

aa <- model.matrix(~ . + 0, data=eBayAll, contrasts.arg = lapply(eBayAll, contrasts, contrasts=FALSE))
str(aa)

# Compute distances


distancesAll = dist(aa, method = "euclidean")

# Hierarchical clustering
clustereBayAll = hclust(distancesAll, method = "ward.D") 
plot(clustereBayAll)
# Assign points to clusters
clusterGroupsAll = cutree(clustereBayAll, k = 3)

# put the variables again
eBayAll$UniqueID <- AllID
# eBayTrain4$sold <- eBayTrain3$sold

Cluster1All <- subset(eBayAll,clusterGroups==1)
Cluster2All <- subset(eBayAll,clusterGroups==2)
Cluster3All <- subset(eBayAll,clusterGroups==3)
#for all elements in Cluster3Train, biddable=0,DScratch=0
