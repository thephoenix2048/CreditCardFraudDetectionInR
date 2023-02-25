library(rpart)

df <- read.csv("F:/Deepti/R Programs/creditcard.csv",header = TRUE, sep=",")

ind <- sample.split(df$Class, SplitRatio = 0.8)

train<- df[ind,]
test<- df[-ind,]

dtm <- rpart(Class~., train, method = "class")

printcp(dtm)
plotcp(dtm)

plot(dtm, uniform = TRUE, main="Classification tree for Dataset")
text(dtm, use.n = TRUE, all = TRUE, cex=0.8)

predicted <- predict(dtm,test,type="class")
table(predicted, test$Class)
