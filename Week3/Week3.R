# ****************************************************************************************
# 
#   Rolling Regression Backtesting Script for Multiple Stocks with Portfolio Statistics & Portfolio Weighting
#
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************

library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
cat("\014")
# ***************GET DATA AND SET TRADING DATE RANGE **********************************

library(quantmod)
library(TTR)
library(forecast)
library(dplyr)
load("universe.rdata")
window <- 20  ### Have multiples of 2.5
from<-as.Date("2021-01-01")
to<-as.Date("2022-12-31")
from <- from - window - (window/2.5)

sectors <- read.csv("sectors.csv")
sectors <- sectors[!duplicated(sectors),]
sectors <- subset(sectors, Sector == "Energy")

universe <-subset(stock,stock$date>=from&stock$date<=to)
universe <- merge(universe,sectors)



universe <- universe[order(universe$symbol,universe$date),]

####################### Remove stocks which do not remain in S&P 500 for all trading days #####################

totaldays <- length(unique(universe$date))

count_values <- universe %>%
  group_by(symbol) %>% 
  summarise(count=n_distinct(date))

count_values <- subset(count_values,count == totaldays)

universe <- merge(universe,count_values)[,1:10]
symbols<-unique(universe$symbol)
numsymbols<-length(symbols)

################ Initialize parameters #########################

initialequity<-100000
maxtrade<-10000
maxdaylong<-7
maxdayshort<-7
returnthreshold<-0.001
MAPEthreshold<-2.5 

################## GENERATE INDICATORS ###################
genIndicators=function(sym) {
print(paste("Generating Indicators for",sym))
stock<-subset(universe,universe$symbol==sym)
#numdays <- nrow(stock)
#test.ts <- ts(stock$close,start = c(1,1), end=c(1:numdays), frequency=numdays)
#test.ets<- ets(test.ts,model="ZZZ")
#method <- test.ets$components
#grep(paste(method[1:3],sep=""))

stock.xts<-xts(stock[,c(3:7)],stock$date)

stock.xts$predictreturn<-NA
#stock.xts$predictrpct<-NA
stock.xts$tradetype<-NA
stock.xts$MAPE <- NA
stock.xts$predictclose <- NA


for (i in window:nrow(stock.xts)-1) {
  #     print(i)
  s<-i-window+1
  ts<-as.numeric(as.vector(stock.xts$close[s:i]))
  etsmodel <- ets(ts, model = "ANN")
  foresummary <- forecast(etsmodel,h=1)
  if (as.numeric(foresummary$mean[1])>stock.xts$close[i]) {
    stock.xts$tradetype[i+1] = 1
    stock.xts$predictreturn[i+1]<-(as.numeric(foresummary$mean[1])-stock.xts$close[i])/
      stock.xts$close[i]
  } else {
    stock.xts$tradetype[i+1]=-1
    stock.xts$predictreturn[i+1]<-(stock.xts$close[i]-as.numeric(foresummary$mean[1]))/
      as.numeric(foresummary$mean[1])
  }
  stock.xts$predictclose[i+1] <- as.numeric(foresummary$mean[1])
  stock.xts$MAPE[i+1] <- accuracy(foresummary)[5]
 
}
stock<-data.frame(stock.xts)                                
date<-as.Date(rownames(stock))                              
stock<-cbind(date,stock)                                    
stock$symbol<-sym
stock<-stock[,c(11,1:10)]
rownames(stock)<-seq(1,nrow(stock),1)                       
return(stock)
}

#x <- genIndicators("APA")

# **************************************** APPLY RULES ********************************************************************
applyRules=function(day,equity){
  cashin<-0
  cashout<-0
  return<-1
  print(day)
  candidates<-subset(stock,stock$date==day&stock$predictreturn>=returnthreshold&
                       stock$MAPE<=MAPEthreshold)
  
  candidates<-candidates[order(candidates$MAPE),]
  
  numlongs<-nrow(subset(candidates,tradetype==1))
  numshorts<-nrow(subset(candidates,tradetype==-1))
  
    candidateslong<- subset(candidates,tradetype==1)  

    candidatesshort<- subset(candidates,tradetype==-1)  
  
  if (numlongs>maxdaylong) {
    candidateslong<-candidateslong[c(1:maxdaylong),]
    numlongs<-maxdaylong
  }
  if (numshorts>maxdayshort) {
    candidatesshort<-candidatesshort[c(1:maxdayshort),]
    numshorts<-maxdayshort
  }
  
  numtrades <- numlongs + numshorts
  
  
  ######### Calculate slope and assign weights
  if (numtrades>0) {
    
    candidates <- rbind(candidateslong,candidatesshort)
    
    candidates<-candidates[order(candidates$MAPE),]
    candidates$weights <- NA
  
    if (numtrades==1){
      m=1
    } else {
      m <- -1/(candidates$MAPE[numtrades]-candidates$MAPE[1])  
    }  
  
  for (i in 1:numtrades) {
        candidates$weights[i] <- 1 + m*(candidates$MAPE[i]-candidates$MAPE[1])
  }
  
  s <- sum(candidates$weights)
  tradeamount<-min(maxtrade,equity/s)
  
  for (i in 1:numtrades) {
    candidates$tradeamount[i]<-candidates$weights[i]*tradeamount
  }
  
  
    candidates$buy<-NA
    candidates$sell<-NA
    candidates$return<-NA
    candidates$stockwise_cashout<- NA
    for (i in 1:numtrades){
      candidates$buy[i]<-ifelse(candidates$tradetype[i]==1,candidates$open[i],candidates$close[i])
      candidates$sell[i]<-ifelse(candidates$tradetype[i]==1,candidates$close[i],candidates$open[i])
      candidates$return[i]<-1+(candidates$sell[i]-candidates$buy[i])/candidates$buy[i]
      candidates$stockwise_cashout[i] <- candidates$tradeamount[i] * candidates$return[i]
    }
    cashin<-sum(candidates$tradeamount)
    
    #return<-prod(candidates$return)*(cashin/equity)+(1-cashin/equity)
    cashout<-sum(candidates$stockwise_cashout)
    return <- (cashout/cashin)*(cashin/equity) + (1-cashin/equity)
  } else candidates<-NULL
  return(list(trades=candidates,cashin=cashin,cashout=cashout,return=return))
}

#y <- applyRules(as.Date("2022-06-09"),100000)
#View(y[[1]])
#y[[2]]
#y[[3]]
#y[[4]]
# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************
portfolioStats=function(trades,preturn,tdays){
  tradedays<-length(unique(trades$date))
  totaldays<-length(tdays)
  pctdaystraded<-tradedays/totaldays
  totaltrades<-nrow(trades)
  shorttrades<-nrow(subset(trades,trades$tradetype==-1))
  longtrades<-nrow(subset(trades,trades$tradetype==1))
  
  
  pct_win_long <- nrow(subset(trades,tradetype==1&return>1))*100/longtrades
  avg_long_return<-mean(subset(trades,trades$tradetype==1)$return)
  pct_win_short <- nrow(subset(trades,tradetype==-1&return>1))*100/longtrades
  avg_short_return<-mean(subset(trades,trades$tradetype==-1)$return)
  
  
  cumreturn<-rep(1,length(totaldays))
  maxreturn<-cumreturn
  for (i in c(1:totaldays)){
    cumreturn[i]<-prod(preturn[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
  }
  down<-cumreturn-maxreturn
  downpct<-down/maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:totaldays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  plot(cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(0.5,1.5),
       ylab="Portfolio Return",main="Portfolio Results")
  lines(maxreturn,co=2,lw=2)
  lines(preturn,co=4,lw=2)
  
  cumreturn<-cumreturn[totaldays]
  meanreturn<-mean(preturn,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(preturn,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(downpct*100)
  pct_win_trades <- nrow(subset(trades,return>1))*100/totaltrades
  performance<-list(totaltrades=totaltrades,longtrades=longtrades, 
                    pct_win_long = pct_win_long, avg_long_return=avg_long_return,
                    shorttrades=shorttrades,pct_win_short=pct_win_short,avg_short_return=avg_short_return,
                    cumreturn=cumreturn,meanreturn=meanreturn,
                    sharpe=sharpe,maxdraw=maxdraw,drawlength=maxstreak, pct_win_trades=pct_win_trades)
  return(performance)
}


# ********************************  RUN STRATEGY **********************************
stock<-NULL
for (sym in symbols) {
  temp<-genIndicators(sym)
  if (is.null(stock)) {
    stock<-temp} else
      stock<-rbind(stock,temp)
}
#load("stock.rdata")
tdays<-unique(stock$date)
currentequity<-initialequity
trades<-NULL
preturn<-rep(length(tdays),1)
for (day in 1:length(tdays)) {
  date<-tdays[day]
  #  print(date)
  results<-applyRules(date,currentequity)
  if (is.null(trades)) {
    trades<-results$trades
  } else if (!is.null(results$trades)) {
    trades<-rbind(trades,results$trades)
  }
  currentequity<-currentequity-results$cashin+results$cashout
  preturn[day]<-results$return
}  
preturn
performance<-portfolioStats(trades,preturn,tdays) ; performance

