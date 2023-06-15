################################################################################
                         # PART-1 #
################################################################################

# Preliminaries

library(rstudioapi)  # This is a external library of functions
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
install.packages(Quandl)
library(Quandl)                      
Quandl.api_key('EfNYF1EymebW8saMFp5B')


load("universe.rdata") 
#symbols<-stock$symbol
#symbols <- unique(symbols)
#fromdate=as.Date("2018-02-01")
#firsttime<-TRUE
#for (currsymbol in symbols) {
#  print(c(currsymbol))
#  temp<-tryCatch({
#    temp<-Quandl.datatable("SHARADAR/SF2",ticker=currsymbol)   # Use tryCatch to handle the error
#  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
#  if (!is.null(temp)) {
#    if (firsttime) {
#      stock_insider<-temp
#    } else {
#      stock_insider<-rbind(stock_insider,temp)}
#    firsttime<-FALSE
#  }
#  else{
#    print("hi");
#  }
#}

#save(stock_insider,file="insider.rdata")
stock <- stock %>% filter(stock$date>="2018-02-01")
stock <- stock[!duplicated(stock),]
#save(stock,file="universe.rdata")
load("insider.rdata") 
stock_insider <- stock_insider %>% filter(stock_insider$transactiondate>="2018-02-01")

data<-stock_insider[order(stock_insider$ticker,stock_insider$transactiondate,stock_insider$rownum),]

data<-data[,c(1,2,5,10,13,14,15,16,17)]

data$buy <- ifelse(data$transactionshares>=0,data$transactionshares,0)
data$sell <- ifelse(data$transactionshares<0,data$transactionshares,0)

data$buy_transactionprice<- ifelse(data$transactionshares>=0,data$transactionpricepershare,0)
data$sell_transactionprice <- ifelse(data$transactionshares<0,data$transactionpricepershare,0)

data$buy_ownername<- ifelse(data$transactionshares>=0,data$ownername,0)
data$sell_ownername <- ifelse(data$transactionshares<0,data$ownername,0)

data1 <- data %>%
  group_by(ticker,transactiondate) %>%
  summarise(buy = sum(buy), sell = sum(sell), buyers = length(unique(buy_ownername[buy_ownername!=0])), seller = length(unique(sell_ownername[sell_ownername!=0])))

data1<-data1 %>% rename(symbol = ticker, date = transactiondate)

stock <- merge(stock[,-c(4,5,7)],data1,all = TRUE, na.action = "na.pass")

colnames(stock)[7] <- "shares_bought(insider)"

colnames(stock)[8] <- "shares_sold(insider)"

colnames(stock)[9] <- "no_of_insiders_buying"

colnames(stock)[10] <- "no_of_insiders_selling"

stock$net_stocks_traded<-rowSums(stock[,c("shares_bought(insider)", "shares_sold(insider)")], na.rm=TRUE)
stock<-stock %>% relocate(net_stocks_traded,.after="shares_sold(insider)")

################################################################################
                          # PART-2 #
################################################################################
# importing required libraries and clearing environment
library(rstudioapi)  
library(dplyr)
install.packages(Quandl)
library(Quandl) 
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
#Calling the api
Quandl.api_key('EfNYF1EymebW8saMFp5B')

#Loading saved S&P500 universe
load("universe.rdata")
#Removing vals with date less than feb 1,2018 and duplicate values
stock <- stock %>% filter(stock$date>="2018-02-01")
stock <- stock[!duplicated(stock),]

#Below commented code is to create the daily metric table
#commented as we saved the data
#uncomment to create and save


#symbols<-stock$symbol
#symbols <- unique(symbols)
#fromdate=as.Date("2018-02-01")
#firsttime<-TRUE
#for (currsymbol in symbols) {
#  print(c(currsymbol))
#  temp<-tryCatch({
#    temp<-Quandl.datatable("SHARADAR/DAILY",date.gte = "2018-02-01",ticker=currsymbol)   # Use tryCatch to handle the error
#  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
#  if (!is.null(temp)) {
#    if (firsttime) {
#      daily<-temp
#    } else {
#      daily<-rbind(daily,temp)}
#    firsttime<-FALSE
#  }
#  else{
#    print("hi");
#  }
#}
#save(daily,file="daily_metric.rdata")


#loading Daily metric data
load("daily_metric.rdata")

#ordering daily metric data by symbol and date
daily<-daily[order(daily$ticker,daily$date),]

#renaming ticker to symbol and merging with the stock table
daily<-daily%>% rename(symbol = ticker)
stockxdm <- merge(stock[,-c(4,5)],daily,all = TRUE, na.action = "na.pass")

#multiplying market cap by 10^6 to get exact value 
stockxdm$marketcap <- stockxdm$marketcap*1000000

#calculating outstanding shares
stockxdm$outstanding_shares_adj <- (stockxdm$marketcap/stockxdm$closeadj)
#stockxdm$outstanding_shares_unadj <- stockxdm$marketcap/stockxdm$closeunadj

#reorganising columns
stockxdm <- stockxdm %>% relocate(marketcap,.after="lastupdated")
stockxdm <- stockxdm %>% relocate(outstanding_shares_adj,.after="marketcap")
stockxdm <- stockxdm[,-c(11,12,13,14,15,16)]
#stockxdm <- stockxdm %>% relocate(outstanding_shares_unadj,.after="outstanding_shares_adj")

#Below commented code is to create the fundamentals table
#commented as we saved the data
#uncomment to create and save


#symbols<-stock$symbol
#symbols <- unique(symbols)
#fromdate=as.Date("2018-02-01")
#firsttime<-TRUE
#for (currsymbol in symbols) {
#  print(c(currsymbol))
#  temp<-tryCatch({
#    temp<-Quandl.datatable("SHARADAR/SF1",ticker=currsymbol)   # Use tryCatch to handle the error
#  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
#  if (!is.null(temp)) {
#    if (firsttime) {
#      fundamental<-temp
#    } else {
#      fundamental<-rbind(fundamental,temp)}
#    firsttime<-FALSE
#  }
#  else{
#    print("hi");
#  }
#}
#save(fundamental,file="fundamental.rdata")

#Loading Fundamentals table
load("fundamental.rdata")

################################################################################

#Loading S&P stock universe
load("universe.rdata") 

#Below commented code is to create the Institutionalised Investor table
#commented as we saved the data
#uncomment to create and save


#symbols<-stock$symbol
#symbols <- unique(symbols)
#fromdate=as.Date("2018-02-01")
#firsttime<-TRUE
#for (currsymbol in symbols) {
#  print(c(currsymbol))
#  temp<-tryCatch({
#    temp<-Quandl.datatable("SHARADAR/SF3A",ticker=currsymbol)   # Use tryCatch to handle the error
#  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
#  if (!is.null(temp)) {
#    if (firsttime) {
#      insti_investor<-temp
#    } else {
#      insti_investor<-rbind(insti_investor,temp)}
#    firsttime<-FALSE
#  }
#  else{
#    print("hi");
#  }
#}

#Saving institutionalised investor data
save(insti_investor,file="insti_investor.rdata")

#Loading institutionalised investor data
load("insti_investor.rdata")

#Order the table and removing values before feb 1, 2018
insti_investor<-insti_investor[,c(1,2,4,12,20,28,29)]
insti_investor<-insti_investor[order(insti_investor$ticker,insti_investor$calendardate),]
insti_investor$calendardate<-as.Date(insti_investor$calendardate)
insti_investor<-insti_investor%>%filter(calendardate>="2018-02-01")
