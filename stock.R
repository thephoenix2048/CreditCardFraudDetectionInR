library(neuralnet)
library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)

set.seed(123)

df <- read.delim("C:/Users/DEEPTI/Downloads/complete_data_set_v1/complete_data_set_v1/AXISBANK.NS.csv", header = TRUE, sep=",")

str(df)

dim(df)

apply(df,2,range)   #unevenly distributed values

x <- 30
ch0<-1
ch1<-30
ch2 <- 1799


order <- 2

###Hence scaling required
maxValue<- apply(df,2,max)
minValue <- apply(df,2,min)

#df <- as.data.frame(scale(df,center=minValue,scale = maxValue-minValue))

#creating train and test data set
ind<- createDataPartition(df$Predicted, p=0.7, list=F)
train <- df[ind,]
test <- df[-ind,]

#order as 2
for(i in 0:2)
{
  if(i==0)
  {
    df<-df*ch0
  }
  if(i==1)
  {
    df<- df*ch1
  }
  if(i==2)
  {
    df<- df*ch2
  }
  
  #hidden layer as 2 with configuration 30-10-4-1
  allVars <- colnames(df)
  predictorVars <- allVars[!allVars%in%"Predicted"]
  predictorVars <- paste(predictorVars,collapse = "+")
  form = as.formula("Open+High+Low+Close+Adj_Close+Volume")
  
  nn <- neuralnet(formula = form,data=train.cv,hidden=c(3,2),linear.output=T)   
  pr.nn <- compute(nn,test.cv[,1:6])
  pr.nn <- pr.nn$net.result*(max(df$Predicted)-min(df$Predicted))+min(df$Predicted)   
  test.cv.r <- (test.cv$Predicted)*(max(df$Predicted)-min(df$Predicted))+min(df$Predicted)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)  
  i<-i+1
  pbar$step()
}

predictions <- predictions$net.result*(max(test$Predicted)-min(test$Predicted))+min(test$Predicted)
actualValues <- (test$Predicted)*(max(test$Predicted)-min(test$Predicted))+min(test$Predicted)#unscaled

MSE <- sum((predictions-actualValues)^2)/nrow(test)
MSE

plot(test$Predicted,predictions,col=c("red","blue"),main="Real vs Predicted",pch=1,cex=0.9,type="p",xlab="Actual",ylab = "Predicted")


legend(x="top",legend = c("Actual","Predicted"),col=c("red","blue"),pch=1)

confusionMatrix(predictions,test$Predicted,positive="1")
