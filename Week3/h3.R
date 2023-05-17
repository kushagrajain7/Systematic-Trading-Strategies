# ****************************************************************************************
# 
#   Rolling Regression Backtesting Script for Multiple Stocks with Portfolio Statistics
#
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************

library(rstudioapi)  
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
options(scipen=999)  # removes scientific notation for numbers displayed

# ***************GET DATA AND SET TRADING DATE RANGE **********************************

library(forecast) 
load("universe.rdata")                                                  
from<-as.Date("2021-01-01")
to<-as.Date("2022-12-31")

sectors <- read.csv("sectors.csv")

sectors <- subset(sectors, Sector == "Energy")
stock <- merge(stock,sectors)
universe <- stock[!duplicated(stock),]


symbols<- unique(universe$symbol)
numsymbols<-length(symbols)
stock<-NULL
for (sym in 1:length(symbols)){
  temp<-subset(universe,universe$symbol==symbols[sym]&universe$date>=from&universe$date<=to)
  if (is.null(stock)) {
    stock<-list(temp) 
  } else  {stock[[sym]]<-temp}
}

# ************************************** GENERATE INDICATORS **********************************************************

library(forecast)

genIndicators <- function(stock) {
  sym <- stock$symbol[1]
  print(paste('Generating Indicators for symbol:', sym))
  tradedays <- nrow(stock)
  stock.ts <- ts(stock$close, start = c(1, 1), end = c(1, tradedays), frequency = tradedays)
  stock$predictedclose <- NA
  window <- 20
  i <- window
  for (i in window:(tradedays - 1)) {
    s <- i - window + 1
    train.ts <- window(stock.ts, end = i)
    model.ets <- ets(train.ts, model = "AAA")
    stock$predictedclose[i + 1] <- as.numeric(forecast(model.ets, h = 1)$mean)
  }
  return(list(stock))
}

x <- genIndicators(stock[1])
# **************************************** APPLY RULES ********************************************************************

applyRules=function(stock){
  sym<-stock$symbol[1]
  print(paste('Apply Rules for symbol: ',sym))
  pthreshold<-0.05
  stock$buy<-NA
  stock$sell<-NA
  stock$return<-1
  
  stock$trade<-ifelse(stock$pvalue<=pthreshold,1,0)
  stock$short<-ifelse(stock$trade==1&stock$coefficient<0,1,0)
  stock$long<-ifelse(stock$trade==1&stock$coefficient>0,1,0)
  stock$buy<-ifelse(stock$long==1,stock$open,ifelse(stock$short==1,stock$close,NA))
  stock$sell<-ifelse(stock$long==1,stock$close,ifelse(stock$short==1,stock$open,NA))
  stock$return<-ifelse(stock$trade==1,1+(stock$sell-stock$buy)/stock$buy,1)
  return(list(stock))
}

# ********************************* CALCULATE PERFORMANCE MEASURES *********************************************************

calcPerformance=function(stock){
  sym<-stock$symbol[1]
  tradedays<-nrow(stock)
  print(paste('Calculating Performance for symbol: ',sym))
  stock$cumreturn<-rep(1,tradedays)
  stock$maxreturn<-rep(1,tradedays)
  for (i in c(1:tradedays)){
    stock$cumreturn[i]<-prod(stock$return[c(1:i)],na.rm=TRUE)
    stock$maxreturn[i]<-max(stock$cumreturn[c(1:i)],na.rm=TRUE)
  }
  stock$down<-stock$cumreturn-stock$maxreturn
  stock$downpct<-stock$down/stock$maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:tradedays)){
    streak<-ifelse(stock$down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  totaltrades<-sum(stock$trade,na.rm=TRUE)
  longtrades<-sum(stock$long,na.rm=TRUE)
  shorttrades<-sum(stock$short,na.rm=TRUE)
  cumreturn<-stock$cumreturn[tradedays]
  meanreturn<-mean(stock$return,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(stock$return,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(stock$downpct*100)
  plot(stock$cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylab="Equity Return",main=paste(sym,"Strategy Results"))
  lines(stock$maxreturn,co=2,lw=2)
  lines(stock$return,co=4,lw=2)
  legend(x=0,y=0.7,co=c(1,2,4),lw=c(2,2,2),legend=c("Max Return", "Cum Return","Daily Return"))
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,shorttrades=shorttrades,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,drawlength=maxstreak)
  return(performance)
}

# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************

portfolioStats=function(stock){
  tradedays<-nrow(data.frame(stock[[1]]))
  numsymbols<-length(stock)
  rvalues<-matrix(data=1,nrow=tradedays,ncol=numsymbols)
  totaltrades<-0
  shorttrades<-0
  longtrades<-0
  for (sym in 1:numsymbols) {
    instrument<-data.frame(stock[[sym]])
    totaltrades<-totaltrades+sum(instrument$trade,na.rm=TRUE)
    longtrades<-longtrades+sum(instrument$long,na.rm=TRUE)
    shorttrades<-shorttrades+sum(instrument$short,na.rm=TRUE)
    for (period in 1:tradedays) {
      rvalues[period,sym]<-ifelse(instrument$trade[period]==1,instrument$return[period],1)
    }
  }
  return<-apply(rvalues,1,prod)
  cumreturn<-rep(1,tradedays)
  maxreturn<-rep(1,tradedays)
  for (i in c(1:tradedays)){
    cumreturn[i]<-prod(return[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
  }
  down<-cumreturn-maxreturn
  downpct<-down/maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:tradedays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  plot(cumreturn,type="l",col="black",lwd=2,xlab="Time Period",
       ylab="Portfolio Return",main="Portfolio Results")
  lines(maxreturn,co=2,lw=2)
  lines(return,co=4,lw=2)
  legend(x=500,y=0.75,co=c(1,2,4),lw=c(2,2,2),
         legend=c("Cum Return","Max Return","Daily Return"))
  cumreturn<-cumreturn[tradedays]
  meanreturn<-mean(return,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(return,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(downpct*100)
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,
                    shorttrades=shorttrades,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,
                    drawlength=maxstreak)
  return(performance)
}

# ********************************  RUN STRATEGY **********************************

for (sym in 1:numsymbols) {
  stock[[sym]]<-genIndicators(data.frame(stock[[sym]]))
  stock[[sym]]<-applyRules(data.frame(stock[[sym]]))
  if (sym==1){
    results<-calcPerformance(data.frame(stock[[sym]]))
  } else {
    results<-list(results, calcPerformance(data.frame(stock[[sym]])))
  }
}

for (i in 1:numsymbols) {
  print(results[[i]])
}

performance<-portfolioStats(stock) ; performance
performance

