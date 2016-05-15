submission <- read.table("/Users/francois-guillaume.rideau/Documents/Kaggle/samplesubmission.csv", header=TRUE, sep=",",na.strings = "NA",check.names=FALSE)

head(submission)


# RANN with traindata_NN=traindata[,c("X","Y","hourslice","DayOfWeek")]
submission[,2:40]=predRANN_test[,1:39]

write.csv(submission,"/Users/francois-guillaume.rideau/Documents/Kaggle/submit_RANN1.csv",row.names=FALSE)

# baseline 
res = table3$Prob

submission[,2:40]=matrix(rep(res,Num_RawTestObs),
                         ncol=length(res),
                         byrow=T)
write.csv(submission,"/Users/francois-guillaume.rideau/Documents/Kaggle/baseline.csv",row.names=FALSE)

trucmuche=read.table("/Users/francois-guillaume.rideau/Documents/Kaggle/submit_RANN1.csv", header=TRUE, sep=",",na.strings = "NA",check.names=FALSE)