################################################################################
#
#                            Create S&P Database
#
################################################################################

# Preliminaries

library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
install.packages(Quandl)
library(Quandl)                      
Quandl.api_key('EfNYF1EymebW8saMFp5B')  # Dummy Key

# Get 5 years of S&P Data through the Quandl API

symbols<-as.vector(read.csv("SP Tickers.csv")[,1])
fromdate=as.Date("2016-02-01")
firsttime<-TRUE
for (currsymbol in symbols) {
  print(c(currsymbol))
  temp<-tryCatch({
    temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker=currsymbol)   # Use tryCatch to handle the error
  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
  if (!is.null(temp)) {
    if (firsttime) {
      stock<-temp
    } else {
      stock<-rbind(stock,temp)}
    firsttime<-FALSE
  }
}

names(stock)[1]<-"symbol"

# Get rid of of data prior to date stock is added to the S&P 500

add<-read.csv("SP additions.csv")
add$date.added<-as.Date(add$date.added,format="%m/%d/%Y")
temp<-merge(stock,add,all.x=TRUE)
temp$date.added<-as.Date(ifelse(is.na(temp$date.added),as.Date("2000-12-31"),temp$date.added))
temp<-subset(temp,temp$date>=temp$date.added)

# Get rid of data after stock is removed from the S&P 500

remove<-read.csv("SP removals.csv")
remove$date.removed<-as.Date(remove$date.removed,format="%m/%d/%Y")
temp<-merge(temp,remove,all.x=TRUE)
temp$date.removed<-as.Date(ifelse(is.na(temp$date.removed),as.Date("2100-12-31"),temp$date.removed))
temp<-subset(temp,temp$date<temp$date.removed)
temp<-temp[order(temp$symbol,temp$date),]
stock<-temp[,c(1:9)]

rownames(stock)<-seq(1,nrow(stock),1)
save(stock,file="universe.rdata")

