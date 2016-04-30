require(verification)

library(gbm)

k=5 #k-folds validation

#rounded down to avoid going out of bounds on the last fold
err.vect <- rep(NA,k)

#define here the train set and resample it
set.seed(19112)
eBayTrain3$sold <- eBayTrain1$sold
train <- eBayTrain3[sample(nrow(eBayTrain3)),]
train <- train[,c(9,1:8,11:25)] #good for eBayTrain3

# dependent variable = integer 0 or 1
### CV for Logreg ###
for (i in 1:k){
        s1 = ((i-1)*n+1)
        s2=  (i*n)
        subset=s1:s2
        
        cv.train = train[-subset,]
        cv.test = train[subset,]
        # for eBayTrain2 and LogReg
        # fit=glm(sold~biddable+condition+productline+storage+price_index+carrier,data=cv.train,family="binomial")
        
        # for eBayTrain3 and LogReg3
        fit=glm(sold~biddable+condition+storage+productline+price_index+Dwork+Dbox,data=cv.train,family="binomial")
        
        prediction=predict(fit,newdata=cv.test[,-1],type="response")
        
        err.vect[i] <- roc.area(cv.test[,1],prediction)$A
        print(paste("LogReg AUC fold",i,":",err.vect[i]))
}
print(paste("LogReg Average AUC:",mean(err.vect)))

# 
# 
# ###### GBM with cross validation ######
# 
# n=floor(nrow(train)/k)
# 
# ntrees=10000
# 
# for (i in 1:k){
#         s1 = ((i-1)*n+1)
#         s2=  (i*n)
#         subset=s1:s2
#         
#         cv.train = train[-subset,]
#         cv.test = train[subset,]
#         
#         fit=gbm.fit(x=cv.train[,-1],y=cv.train[,1],
#                     n.trees=ntrees,verbose=FALSE,shrinkage=0.001,
#                     interaction.depth=20,n.minobsinnode=5,distribution="bernoulli")
#         #use bernouilli for classification problems
#         
#         prediction=predict(fit,newdata=cv.test[,-1],n.trees=ntrees)
#         
#         err.vect[i] <- roc.area(cv.test[,1],prediction)$A
#         print(paste("GBM AUC fold",i,":",err.vect[i]))
# }
# print(paste("GBM Average AUC:",mean(err.vect)))