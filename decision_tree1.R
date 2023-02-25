library(rpart)
library(rpart.plot)

df <- read.csv("F:/Deepti/R Programs/creditcard.csv",header = TRUE, sep=",")

s<- sample(4217,3500)

train_data <- df[s,]
test_data <- df[-s,]

dtm<- rpart(Class~., train_data, method="class")

rpart.plot(dtm, type=4, extra=101)

p<- predict(dtm, test_data, type = "class")
table(p,test_data$Class)

p<-data.frame(p)

difference<- as.numeric(p)-as.numeric(test_data$Class)

#MSE <- sum((as.numeric(p)-as.numeric(test_data$Class)^2)/nrow(test_data)
#test_data["Class"]

