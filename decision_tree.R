library(rpart)
library(caret)
library(rpart.plot)
library(ROCR)

set.seed(500)

df = read.csv("F:/Deepti/R Programs/creditcard_full.csv", header=TRUE ,sep = ",")

apply(df,2,function(x) round(length(unique(x))/nrow(df),3)*100)

library(caTools)


df$Class <- factor(df$Class)

#summary(df)

df = rescale(df, to = c(0,1))
summary(df$Amount)#Normalizing Amount

#summary(df)

'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)



ind<- createDataPartition(df$Class, p=0.7, list=F)

train_data <- df[ind,]
test_data <- df[-ind,]

down_train <- downSample(x = train_data[, colnames(train_data) %ni% "Class"],
                         y = train_data$Class)
table(down_train$Class)


#Fitting the model  
decisiontreemodel <- rpart(Class~., down_train,method='class')

fit <- rpart(Class~., down_train,method = "class")
rpart.plot(fit, type=4, extra=101)

p<- predict(decisiontreemodel, test_data, type = "class")

confusionMatrix(p,test_data$Class,positive="1")

table(p,test_data$Class)

#plot(pfit, uniform=TRUE, 
 #    main="Decision Classification Tree")


# plot the pruned tree 
#plot(pfit, uniform=TRUE, 
 #   main="Pruned Classification Tree")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#printcp(pfit)





#pred1 <- prediction(as.numeric(p), as.numeric(test_data$Class))
#perf1 <- performance(pred1,"tpr","fpr")
#perf2 <- performance(pred1,"tnr","fnr")
#plot(perf1,col="blue")
#plot(perf2,col="red")
