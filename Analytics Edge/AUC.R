require(verification)
library(randomForest)
library(gbm)

k=5 #k-folds validation
n=floor(nrow(train)/k)
#rounded down to avoid going out of bounds on the last fold
err.vect <- rep(NA,k)

# dependent variable = integer 0 or 1
### CV for random forest ###
for (i in 1:k){
        s1 = ((i-1)*n+1)
        s2=  (i*n)
        subset=s1:s2

        cv.train = train[-subset,]
        cv.test = train[subset,]
        
        fit=randomForest(x=cv.train[,-1],y=as.factor(cv.train[,1]))
        prediction=predict(fit,newdata=cv.test[,-1],type="prob")[,2]
        
        err.vect[i] <- roc.area(cv.test[,1],prediction)$A
        print(paste("Random Forest AUC fold",i,":",err.vect[i]))
}
print(paste("Random Forest Average AUC:",mean(err.vect)))

### CV for GBM ###

ntrees=5000

for (i in 1:k){
        s1 = ((i-1)*n+1)
        s2=  (i*n)
        subset=s1:s2
        
        cv.train = train[-subset,]
        cv.test = train[subset,]
        
        fit=gbm.fit(x=cv.train[,-1],y=cv.train[,1],
                    n.trees=ntrees,verbose=FALSE,shrinkage=0.001,
                    interaction.depth=20,n.minobsinnode=5,distribution="bernoulli")
        #use bernouilli for classification problems
        
        prediction=predict(fit,newdata=cv.test[,-1],n.trees=ntrees)
        
        err.vect[i] <- roc.area(cv.test[,1],prediction)$A
        print(paste("GBM AUC fold",i,":",err.vect[i]))
}
print(paste("GBM Average AUC:",mean(err.vect)))

