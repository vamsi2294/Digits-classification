#####Libraries####
library(rpart)
library(party)

## Set Directory####
setwd("setdir")

##Read Data from the csv files######
digits<-read.csv("optdigits_raining.csv",header=FALSE)
test1<-read.csv("optdigits_test1.csv",header=FALSE)

##Calculating the attributes,means and standard deviation####
no_attributes=ncol(digits)
no_observations=length(digits$V65)
sds<-as.data.frame(apply(test1[,1:ncol(digits)], 2, function(x) sd(x, na.rm=FALSE)),header=FALSE)
Means<-as.data.frame(apply(test1[,1:ncol(digits)], 2, function(x) mean(x, na.rm=FALSE)),header=FALSE)


## rpart Alogorithm for the Decision Tree without pruning#### 
rpart_nopruning<-rpart(digits$V65~.,digits,method = "class",cp=0)

##calculating CP values to prune the Decision tree## 
plot(rpart_nopruning)


##rpart with pruning  based on cp values##
rpart_control<-rpart.control(minsplit = 5,minbucket = 1,cp=0)
rpart_pruning<-rpart(digits$V65~.,digits,method = "class",control=rpart_control)

##ctre withour pruning##
ctree_nopruning<-ctree(digits$V65~.,digits)

## ctree algorithm with pruning#######
c=ctree_control(minsplit=20,minbucket = 1)
ctree_pruning<-ctree(digits$V65~.,digits,controls =c)

##plotting the Decision Trees####
plot(ctree_nopruning,main="ctree Decision Tree without pruning")
plot(ctree_pruning,main="ctree Decision Tree with pruning")
plot(rpart_nopruning,main="rpart Decision tree without puning")
text(rpart_nopruning,pretty = 2)
plot(rpart_pruning,main="rpart Decision tree without puning")
text(rpart_pruning,pretty = 2)

## Predicting the output####
test1out1<-predict(ctree_nopruning,test1,"prob")            #prediction for ctree DT without pruning 
test1out2<-predict(ctree_pruning,test1,type="prob")         #prediction for ctree DT with pruning
test1out3<-predict(rpart_nopruning,test1,type="class")    #prediction for rpart DT without pruning      
test1out4<-predict(rpart_pruning,test1,type="class")      #prediction for rpart DT with pruning

### Calculating Accuracy###
ctree_nop_accuracy=sum(test1$V65==test1out1)/length(test1out1)
ctree_p_accuracy=sum(test1$V65==test1out2)/length(test1out2)
rpart_nop_accuracy=sum(test1$V65==test1out3)/length(test1out3)
rpart_p_accuracy=sum(test1$V65==test1out4)/length(test1out4)

##plotting Predicted values for the test1 data###
plot(test1out1,main="Predicted digits for ctree no pruning") 
plot(test1out3,main="Predicted digits for rpart no pruning") 
plot(test1out4,main="Predicted digits for rpart pruning") 


