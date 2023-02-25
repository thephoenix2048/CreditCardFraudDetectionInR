library(plotly)

Accuracy<- c(96.27,99.13,96.98,96.402)

Algorithms<-c("Logistic Regression", "KNN","Naive Bayes","Decision Trees")

data<-data.frame(Algorithms,Accuracy)

#plot(Algorithms,Accuracy,type="b", col=c("white","red","blue","green","yellow"))

#legend("bottomright", legend=c("Logistic Regression", "KNN","Naive Bayes","Decision Trees"),
 #      col=c("red", "blue","green","yellow"), lty=1:2, cex=0.8)

#points( Algorithms,Accuracy, col=c("white","red", "blue","green","yellow"), pch=15 )


p <- plot_ly(data, x = ~Algorithms, y = ~Accuracy, type = 'bar',
             marker = list(color = c("#0081FF", "#80FE1A","#FDEE02","#FF3300"))) %>%
  layout(title = "Comparison of Algorithms",
         xaxis = list(title = "Algorithms"),
         yaxis = list(title = "Accuracy"))

#text(x = Algorithms, y = Accuracy, label = Accuracy, pos = 3, cex = 0.8, col = "red")
#axis(1, at=Algorithms, labels=Accuracy, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)
p

text(p,Accuracy,labels=as.character(Accuracy))

#pal <- function(col, border = "light gray", ...){
 # n <- length(col)
  #plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
   #    axes = FALSE, xlab = "", ylab = "", ...)
  #rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
#}

#rich8equal = c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300")
#pal(rich8equal)









x<-barplot(as.matrix(Accuracy), beside=TRUE,
           legend.text=Algorithms, args.legend=list(bty="n",horiz=TRUE),
           col=brewer.pal(5,"Set1"),border="white",
           ylim=c(0,100),ylab="Sales Revenue (1,000's of USD)",
           main="Sales Figures")

y<-as.matrix(Accuracy)

text(x,y+2,labels=as.character(y))