SSE_predict_vs_real <- function(prediction,real){
        sum((prediction-real)^2)
}

SST_train_vs_test <- function(train,test){
        sum((mean(train) - test)^2)
}