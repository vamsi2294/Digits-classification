##Libraries
library(caret)
library(nnet)
library(neuralnet)

##Read Data from the csv files######
digits<-read.csv("optdigits_raining.csv",header=FALSE)
test<-read.csv("optdigits_test.csv",header=FALSE)

##Calculating the attributes,means and standard deviation####
no_attributes=ncol(Digits)
no_observations=length(digits$V65)
sds<-as.data.frame(apply(test[,1:ncol(digits)], 2, function(x) sd(x, na.rm=FALSE)),header=FALSE)
Means<-as.data.frame(apply(test[,1:ncol(digits)], 2, function(x) mean(x, na.rm=FALSE)),header=FALSE)



##multinom function  to fit the neural network
p=multinom(digits$V65~.,digits,maxit=100,hidden=c(12,1),decay=1e-06)

##Analysing the accuracy for the Iterations.
i=10
acc=0
while(i<300){
p=multinom(digits$V65~.,digits,maxit=i,hidden=c(12,1),decay=1e-06)
pred <- predict(p, type="class", newdata=test[1:64])
acc=sum(test$V65==pred)/length(pred)
acc=c(acc,acc)
i=i+10
}

##Plotting the accuracy graph
plot(1:30,acc,type="o",col="blue",main="Accuracy VS Iterations",xlab="Iterations",ylab = "Accuracy")
max(k)

##Corss validation for the training data using multinom function
cvDivider <- floor(nrow(digits) / (cv+1))
index <- c(1:2676)
dtrain=digits[index,]
dtest=digits[-index,]
p=multinom(dtrain$V65~.,dtrain,maxit=150,hidden=c(12,1),decay=1e-06)

##important variable to classify 
mostImportantVariables <- varImp(p)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))


##Prediction  function for the test data
pred <- predict(p, type="class", newdata=dtest[1:64])

###Accuracy for the predicted output
acc=sum(dtest$V65==pred)/length(pred)

##table for the input vs predicted output
table(test$V65,pred)
plot(table(test$V65,pred))
plot(pred,test$V65) 