library(plotly)

df = read.csv("creditcard.csv", header=TRUE ,sep = ",")

names(df)

summary(df)

sum(is.na(df))  #0

colSums(df["Class"] != 0)   #fraud transactions = 492 , valid = 284315

summary(df$Time[df$Class == 1])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   Fraud cases
#406   41242   75569   80747  128483  170348

summary(df$Time[df$Class == 0])   #Normal wrt time
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0   54230   84711   94838  139333  172792

