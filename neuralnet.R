library(neuralnet)

set.seed(123)

df <- read.csv("F:/Deepti/R Programs/creditcard.csv", header = TRUE, sep=",")

str(df)

hist(df$Class)

colSums(df["Class"]!=0)   #492 are fraud

dim(df)

apply(df,2,range)   #unevenly distributed values

###Hence scaling required
maxValue<- apply(df,2,max)
minValue <- apply(df,2,min)

df <- as.data.frame(scale(df,center=minValue,scale = maxValue-minValue))

ind<- createDataPartition(df$Class, p=0.7, list=F)

#creating train and test data set
train <- df[ind,]
test <- df[-ind,]

#hidden layer as 2 with configuration 30-10-4-1
allVars <- colnames(df)
predictorVars <- allVars[!allVars%in%"Class"]
predictorVars <- paste(predictorVars,collapse = "+")
form = as.formula(paste("Class~",predictorVars,collapse = "+"))

start.time <- Sys.time()
neuralModel <- neuralnet(formula = form, hidden = c(3,2),linear.output = T,
                         data = train)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(neuralModel)

#predict
predictions <- compute(neuralModel,test[,1:30])
str(predictions)

predictions <- predictions$net.result*(max(test$Class)-min(test$Class))+min(test$Class)
actualValues <- (test$Class)*(max(test$Class)-min(test$Class))+min(test$Class)#unscaled

MSE <- sum((predictions-actualValues)^2)/nrow(test)
MSE


plot(test$Class,predictions,col=c("red","blue"),main="Real vs Predicted",pch=1,cex=0.9,type="p",xlab="Actual",ylab = "Predicted")

#plot(test$Class,type="o",col="red")
#lines(predictions,type="o",col="blue")

legend(x="top",legend = c("Actual","Predicted"),col=c("red","blue"),pch=1)
#abline(0,1,col='black')



#Cross validation
library(boot)
set.seed(450)
cv.error <- NULL
k <- 10   #10 fold cross validation
library(plyr)
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  maxValue<- apply(df,2,max)
  minValue <- apply(df,2,min)
  
  df <- as.data.frame(scale(df,center=minValue,scale = maxValue-minValue))
  
  index <- sample(1:nrow(df),round(0.9*nrow(df)))
  train.cv <- df[index,]
  test.cv <- df[-index,]
  
  #hidden layer as 2 with configuration 30-10-4-1
  allVars <- colnames(df)
  predictorVars <- allVars[!allVars%in%"Class"]
  predictorVars <- paste(predictorVars,collapse = "+")
  form = as.formula(paste("Class~",predictorVars,collapse = "+"))
  
  nn <- neuralnet(formula = form,data=train.cv,hidden=c(3,2),linear.output=T)   
  pr.nn <- compute(nn,test.cv[,1:30])
  pr.nn <- pr.nn$net.result*(max(df$Class)-min(df$Class))+min(df$Class)   
  test.cv.r <- (test.cv$Class)*(max(df$Class)-min(df$Class))+min(df$Class)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

