#Unit 5 AUTOMATING REVIEWS IN MEDICINE

trials <- read.csv("clinical_trial.csv",stringsAsFactors=FALSE)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract,0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)

library(rpart)
library(rpart.plot)
trialCART <- rpart(trial~.,data=train,method="class")
prp(trialCART)

predTrain = predict(trialCART)[,2]
table(train$trial,predTrain)

predTest = predict(trialCART, newdata=test)
pred.prob=predTest [,2]

library(ROCR)

predROCR = prediction(pred.prob, test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

performance(predROCR, "auc")@y.values

#SEPARATING SPAM FROM HAM (part1)

emails <- read.csv("emails.csv",stringsAsFactors=FALSE)
library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)

spdtm= removeSparseTerms(dtm, 0.95)
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)


spamLog <- glm(spam~.,data=train,family="binomial")

library(rpart)
library(rpart.plot)
spamCART <- rpart(spam~.,data=train,method="class")

library(randomForest)
set.seed(123)
spamRD <- randomForest(spam~.,data=train)



predROCR = prediction(predRF, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

#clouds
# https://georeferenced.wordpress.com/2013/01/15/rwordcloud/
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))