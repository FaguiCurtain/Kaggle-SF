eBayTest <- read.csv("eBayiPadTest.csv")
eBayTrain <- read.csv("eBayiPadTrain.csv")


eBayTrain1 <- eBayTrain
eBayTrain1$description <- NULL
eBayTrain1$biddable <- as.factor(eBayTrain1$biddable)

library(rpart)
library(rpart.plot)

cutoff <- 0.5 #0.65 to 0.70 looks good

eBayTree1 <- rpart(sold~. -UniqueID,data=eBayTrain1,method="class")
predCART1 <- predict(eBayTree1)
resCART1 <- table(eBayTrain$sold,predCART1[,2]>cutoff) #change cutOFF
accCART1 <- (resCART1[1,1]+resCART1[2,2])/sum(resCART1)


# lets have fun with description

library(tm)
corpus <- Corpus(VectorSource(eBayTrain$description))
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
eBayTrain2 <- eBayTrain1
#0.95 threshold
eBayTrain2$Dgood <- as.factor(eBayTrain_des$good)
eBayTrain2$Dnew <- as.factor(eBayTrain_des$new)
eBayTrain2$Dscratch <- as.factor(eBayTrain_des$scratch)
eBayTrain2$Dcondit <- as.factor(eBayTrain_des$condit)
eBayTrain2$Dscreen <- as.factor(eBayTrain_des$screen)
eBayTrain2$Duse <- as.factor(eBayTrain_des$use)
eBayTrain2$Dwork <- as.factor(eBayTrain_des$work)
eBayTrain2$Dipad <- as.factor(eBayTrain_des$ipad)

#0.98 threshold
eBayTrain2$Dbox <- as.factor(eBayTrain_des$box)
eBayTrain2$Dcase <- as.factor(eBayTrain_des$case)
eBayTrain2$Dinclud <- as.factor(eBayTrain_des$includ)
eBayTrain2$Dperfect <- as.factor(eBayTrain_des$perfect)
eBayTrain2$Dcrack <- as.factor(eBayTrain_des$crack)
eBayTrain2$Dexcel <- as.factor(eBayTrain_des$excel)

#removing unsignificant variables and adding significant text
eBayTrain3 <- eBayTrain1
# eBayTrain3$cellular <- NULL
# eBayTrain3$carrier <- NULL
# eBayTrain3$color <- NULL
eBayTrain3$Dexcel <- eBayTrain2$Dexcel
eBayTrain3$Dscratch <- eBayTrain2$Dscratch

eBayTree2 <- rpart(sold~. -UniqueID,data=eBayTrain2,method="class")
predCART2 <- predict(eBayTree2)
resCART2 <- table(eBayTrain2$sold,predCART2[,2]>cutoff) #change cutOFF
accCART2 <- (resCART2[1,1]+resCART2[2,2])/sum(resCART2)


eBayTree3 <- rpart(sold~. -UniqueID,data=eBayTrain3,method="class")
predCART3 <- predict(eBayTree3)
resCART3 <- table(eBayTrain3$sold,predCART3[,2]>cutoff) #change cutOFF
accCART3 <- (resCART3[1,1]+resCART3[2,2])/sum(resCART3)

# trying logistic regression

LogReg1 <- glm(sold~. -UniqueID,data=eBayTrain1,family="binomial")
LogReg2 <- glm(sold~. -UniqueID,data=eBayTrain2,family="binomial")
LogReg3 <- glm(sold~. -UniqueID,data=eBayTrain3,family="binomial")

predLR1 <- predict(LogReg1,type="response")
resLR1 <- table(eBayTrain1$sold,predLR1>cutoff)
accLR1 <- (resLR1[1,1]+resLR1[2,2])/sum(resLR1)

predLR2 <- predict(LogReg2,type="response")
resLR2 <- table(eBayTrain2$sold,predLR2>cutoff)
accLR2 <- (resLR2[1,1]+resLR2[2,2])/sum(resLR2)

predLR3 <- predict(LogReg3,type="response")
resLR3 <- table(eBayTrain3$sold,predLR3>cutoff)
accLR3 <- (resLR3[1,1]+resLR3[2,2])/sum(resLR3)

LR3ok <- eBayTrain3$sold==(predLR3>cutoff)
LR3perf <- mean(LR3ok)
tapply(LR3ok,eBayTrain$productline,mean)
tapply(LR3ok,eBayTrain$biddable,mean)

# Train set AUC 
library(ROCR)
print("Train set AUC for LR1")
ROCRpred = prediction(predLR1, eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8652028


print("Train set AUC for LR3")
ROCRpred = prediction(predLR3, eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
# [1] 0.8629527

# prepare the Corpus for the Test Set
corpusTest <- Corpus(VectorSource(eBayTest$description))
corpusTest <- tm_map(corpusTest, content_transformer(tolower))
corpusTest = tm_map(corpusTest, PlainTextDocument)
corpusTest = tm_map(corpusTest, removePunctuation)
corpusTest = tm_map(corpusTest, removeWords, stopwords("english"))
corpusTest = tm_map(corpusTest, stemDocument)
dtmTest = DocumentTermMatrix(corpusTest)

spdtmTest= removeSparseTerms(dtmTest, 0.98)
eBayTest_des <- as.data.frame(as.matrix(spdtmTest))
colnames(eBayTest_des) = make.names(colnames(eBayTest_des))

eBayTest3 <- eBayTest
eBayTest3$biddable <- as.factor(eBayTest3$biddable)
eBayTest3$Dexcel <- as.factor(eBayTest_des$excel)
eBayTest3$Dscratch <- as.factor(eBayTest_des$scratch)

predTestLR3 <- predict(LogReg3,newdata=eBayTest3,type="response")
# predTestLR3cut <- as.numeric(predTestLR3>0.65)

# Regression Tree with cross-validation
# Load libraries for cross-validation
library(caret)
library(e1071)

set.seed(123)
# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = seq(0.01,0.5,0.01))

# Cross-validation
tr = train(sold ~ . -UniqueID, data = eBayTrain3, method = "rpart", trControl = tr.control, tuneGrid = cp.grid,minbucket=10)

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

eBayTree_CV <- rpart(sold ~ . -UniqueID, data = eBayTrain3,method="class", cp = 0.06, minbucket=10)

# Make predictions
best.tree.pred = predict(best.tree)
resBestTree <- table(eBayTrain3$sold,best.tree.pred>cutoff)
accBestTree <- (resBestTree[1,1]+resBestTree[2,2])/sum(resBestTree)

# Train set AUC 
library(ROCR)
print("Train set AUC for CART with CV")
ROCRpred = prediction(best.tree.pred, eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))

#Random Forest
library(randomForest)

# Try again

eBayTrain3$sold <- as.factor(eBayTrain3$sold) #necessary for Random Forest

set.seed(456)

eBayForest = randomForest(sold ~ .-UniqueID, data = eBayTrain3, ntree=200, nodesize=10 )

# Make predictions
PredForest = predict(eBayForest,type="prob")[,2]
resForest <- table(eBayTrain3$sold, PredForest>cutoff)
accForest <- (resForest[1,1]+resForest[2,2])/sum(resForest)

# Train set AUC 
library(ROCR)
print("Train set AUC for Random Forest")
ROCRpred = prediction(PredForest, eBayTrain3$sold)
print(as.numeric(performance(ROCRpred, "auc")@y.values))
#[1] 0.8589567

submission  <-  data.frame(UniqueID = eBayTest$UniqueID, Probability1 = predTestLR3)
write.csv(submission,"submission.csv",row.names=FALSE)

