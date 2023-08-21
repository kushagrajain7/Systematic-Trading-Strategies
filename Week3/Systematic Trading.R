library(rstudioapi)  # This is a external library of functions
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
install.packages("forecast")

#install.packages(Quandl)
#library(Quandl)                      
#Quandl.api_key('EfNYF1EymebW8saMFp5B')

load("universe.rdata")
sectors <- read.csv("sectors.csv")

for(sym in unique(stock$symbol))
{
  if(sym %in% unique(sectors$symbol))
  {
    temp <- head(sectors[sectors$symbol==sym,],1)
    if(temp[2]!="Energy")
    {
      stock <- stock[stock$symbol != sym,]
    }
  }
  else
  {
    stock <- stock[stock$symbol != sym,]
  }
}
#stock <- merge(stock,sectors)
#stock <- stock[stock$Sector == "Energy",]
stock <- stock[order(stock$symbol,stock$date),]
#stock <- stock %>%
#  group_by(symbol,date)
#stock <- stock[,c(1,8)]
x <- stock[stock$date>"2019-12-31" & stock$date<"2021-01-01",]

x<-x[order(x$symbol,x$date),]

length(unique(stock$symbol))

for(sym in unique(x$symbol))
{
  if(length(unique(x$date[x$symbol==sym])) != 253)
  {
    stock<-stock[stock$symbol !=sym,]
  }
 
}

stock<-stock %>%
  filter(date>="2020-12-01")

length(unique(stock$symbol))
lst<-list()
########################################################################################
accuracy_train = NULL
accuracy_test = NULL
for(sym in unique(stock$symbol)){
#diffed = diff(stock$closeadj[stock$symbol==sym], differences = 1)
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
supervised = lag_transform(stock$closeadj[stock$symbol==sym], 20)

N = nrow(supervised)
n = round(N *0.8, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N,  ]

train_prices <- train$`x`
fit <- ets(train_prices, model="ZZZ")

forecast <- forecast(fit, h=length(test$x))
plot(forecast, main = sym)

accuracy_train <- rbind(accuracy_train,c(sym,accuracy(forecast, test$x)[1,]))
accuracy_test <- rbind(accuracy_test,c(sym,accuracy(forecast, test$x)[2,]))
}

print(accuracy_train )
print(accuracy_test )

accuracy_test <- as.data.frame(accuracy_test)
accuracy_test <- accuracy_test[order(as.numeric(accuracy_test$MAPE)),]
