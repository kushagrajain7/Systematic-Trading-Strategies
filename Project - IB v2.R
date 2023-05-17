
# *********SET WORKING DIRECTORY AND CLEAR ENVIRONMENT *********************

library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options("getSymbols.warning4.0"=FALSE) # disables warning message from showing 
options(scipen=999)
cat("\014")

# ************ SET DATE RANGES AND SYSTEM PARAMETERS ***************************
library(Quandl)
library(IBrokers)
library(quantmod)
library(dplyr)
library(tidyquant)

longestindicator<-100
currentdate<-Sys.Date()
maxdaytrades<-15                     # maximum trades in one day
currentSP500<-tq_index("SP500")[,c(1,6)]
IBport=7497 #7497 is the port for tws and 4002 is for the gateway
# ************************* GET DATA FROM FROM STORED UNIVERSE AND IMPORT NEW FROM QUANDL *****************
getData=function(){
  stock<-NULL
  fromdate<-currentdate-2*(longestindicator) 
  Quandl.api_key('EfNYF1EymebW8saMFp5B')
  temp<-NULL
  temp<-tryCatch({
    temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker="AAPL")   # Use tryCatch to handle the error  
  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
  if(!is.null(temp)){
    symbols<-currentSP500$symbol
    for (i in 1:length(symbols)) {
      print(c(i,symbols[i]))
      temp<-NULL
      temp<-tryCatch({
        temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker=symbols[i])   # Use tryCatch to handle the error
      }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
      stock<-rbind(stock,temp)
    }
    stock<-na.omit(stock)
    names(stock)[1]<-"symbol"
    stock<-stock[,c(1:7)]
    rownames(stock)<-seq(1,nrow(stock),1)
    days<-unique(stock$date)
    days<-days[order(days)]
    lastdate<-max(stock$date)
    datastart<-which(days==lastdate)-longestindicator+1
    stock<-subset(stock,stock$date>=days[datastart])
  }
  return(stock)  
}

# ************************* GENERATE INDICATORS *******************************
genIndicators <- function(sym) {
  print(paste('Generating Indicators for symbol: ', sym))
  
  stock <- subset(universe, universe$symbol == sym)
  stock.xts <- xts(stock[, c(3:7)], stock$date)
  
  temp.xts <- stock.xts
  temp.xts$high <- stats::lag(stock.xts$high, n = 1)
  temp.xts$low <- stats::lag(stock.xts$low)
  temp.xts$close <- stats::lag(stock.xts$close)
  
  bb <- tryCatch({
    BBands(HLC(temp.xts), n = 5, maType = "SMA", sd = 2)
  }, warning = function(w) {
    bb <- NULL
  }, error = function(e) {
    bb <- NULL
  })
  
  if (is.null(bb)) {
    stock.xts$dn <- NA
    stock.xts$mavg <- NA
    stock.xts$up <- NA
    stock.xts$range <- NA
  } else {
    stock.xts$dn <- bb$dn
    stock.xts$mavg <- bb$mavg
    stock.xts$up <- bb$up
  }
  
  sma50 <- tryCatch({
    sma50 <- SMA(temp.xts$close, n = 50)
  }, warning=function(w) {sma5 <- NULL}, error=function(e) {sma5 <- NULL})
  
  # ema10 <- tryCatch({
  #   ema10 <- EMA(temp.xts$close, n = 10)
  # }, warning=function(w) {ema10 <- NULL}, error=function(e) {ema10 <- NULL})
  # 
  if (is.null(sma50)) {
    stock.xts$sma50 <- NA
  } else {
    stock.xts$sma50 <- sma50$SMA
  }
  
  stock.xts$rsi <- RSI(stock.xts$close, n = 14)
  stock.xts$range<-stock.xts$rsi
  stock <- data.frame(stock.xts)
  date <- as.Date(rownames(stock))
  stock <- cbind(date, stock)
  stock <- cbind(sym, stock)
  names(stock)[1] <- "symbol"
  rownames(stock) <- seq(1, nrow(stock), 1)
  
  return(stock)
}

# **************************** GENERATE SIGNALS *******************************

genSignals <- function(sym) {
  print(paste('Generating Signals for symbol: ',sym))
  stock <- subset(indicators, indicators$symbol == sym)
  stock.xts <- xts(stock[, c(3:ncol(stock))], stock$date)
  
  stock.xts$cross.sma50 <- ifelse(stock.xts$close > stock.xts$sma50 , 1, 0)
  
  # Generate 7-day low and high signals
  stock.xts$low7 <- rollapply(stock.xts$close, width = 7, FUN = min, align = "right", fill = NA)
  stock.xts$high7 <- rollapply(stock.xts$close, width = 7, FUN = max, align = "right", fill = NA)
  stock.xts$cross.low7 <- ifelse(stock.xts$close <= stock.xts$low7, 1, 0)
  stock.xts$cross.high7 <- ifelse(stock.xts$close >= stock.xts$high7, 1, 0)
  
  
  
  stock <- data.frame(stock.xts)                                      
  date <- as.Date(rownames(stock))                                 
  stock <- cbind(date, stock)                                    
  stock <- cbind(sym, stock)
  names(stock)[1] <- "symbol"
  rownames(stock) <- seq(1, nrow(stock), 1)                          
  return(stock)
}

# **************************CLOSE POSITIONS ************************************
# Here we will check our exit signals and compare them to the list of open
# positions, separately for longs and shorts.  If we have a short position
# and price closes above the trend line, then we close it.   If we have a long
# position and prices closes below the trend line, then we close it.  Note we
# will note simultaneously hold long and short positions with this strategy.
# We will only open if we don't already have an open position in a stock.  
# ******************************************************************************
closePositions=function(day,equity,position){
  print(position)
  cash<-0
  closed<-NULL
  if (!is.null(position)) {
    longposition<-subset(position,type=="Long")              # check long and short separately
    shortposition<-subset(position,type=="Short")           
    candidates<-subset(signals,signals$date==day&            # check shorts first
                         ((signals$cross.sma50==0) & signals$rsi<50 & (signals$cross.low7==1)))[,c(1,2,3)] # grab symbol (1), date(2), and price (6)
    names(candidates)[2]<-"closedate"                        # keep track of the close date so we
    names(candidates)[3]<-"price"                         # can check how long we hold our positions
    closeshort<-merge(shortposition,candidates,by="symbol")  # Close only if we have a position
    candidates<-subset(signals,signals$date==day&            # Now do the same for longs
                         ((signals$cross.sma50==1) & signals$rsi>50 & (signals$cross.high7==1) ))[,c(1,2,6)]    
    names(candidates)[2]<-"closedate"
    names(candidates)[3]<-"price"
    closelong<-merge(longposition,candidates,by="symbol")
    closed<-rbind(closeshort,closelong) # put all our positions to close together
    # print(closed)
  } 
  # print("Okay")
  return(closed)  
}
# ************************** OPEN POSITIONS ************************************
# Now we are going to check our entry signals and only enter a position if we
# don't already have a position in the stock.  So we have a signal to open, we
# need to check for the absence of the position in the set of open positions
# ******************************************************************************

openPositions=function(day,equity,position){
  cash=0
  opened<-NULL
  if (!is.null(position)) {                                      # only need to check if we have open positions
    longposition<-subset(position,type=="Long")[,c(1,2)]         # check long and shorts separately
    names(longposition)[2]<-"dummy"                              # use dummy again, see Apply Rules function
    shortposition<-subset(position,type=="Short")[,c(1,2)]       # for further explanation
    names(shortposition)[2]<-"dummy"
    candidates<-subset(signals,signals$date==day&                # check shorts first  
                         signals$cross.sma50==0 & (signals$cross.high7==1)& signals$rsi>50)
    temp<-merge(candidates,shortposition,by="symbol",all.x=TRUE)
    openshort<-subset(temp,is.na(dummy))                         # only short if we don't have a position
    if (nrow(openshort)>0) {                                      
      openshort<-openshort[,c(1:ncol(openshort)-1)]              # get rid of dummy column
      openshort$type<-"Short"                                    # we will open a short position
    } else {openshort<-NULL}                                     # if the dataframe is empty, set it to null
    candidates<-subset(signals,signals$date==day&                # now proceed and do same for longs
                         signals$cross.sma50==1 & (signals$cross.low7==1) & signals$rsi <50)
    temp<-merge(candidates,longposition,by="symbol",all.x=TRUE)
    openlong<-subset(temp,is.na(dummy))
    if (nrow(openlong)>0) {
      openlong<-openlong[,c(1:ncol(openlong)-1)]
      openlong$type<-"Long" 
    } else {openlong<-NULL}
    opened<-rbind(openlong,openshort)                            # put all positions to be opened together
    if (!is.null(opened)) {                                      # convert empty dataframe to null
      if (nrow(opened)==0) opened<-NULL                          # so we don't have to check for both !null
    }                                                            # and that the number of rows>0
  } else {
    opened<-subset(signals,signals$date==day&                  # no open positions so grab all signals to open
                     ((signals$cross.sma50==1&(signals$cross.low7==1))|(signals$cross.sma50==0)& (signals$cross.high7==1))) 
    if (nrow(opened)==0) {opened<-NULL} else {                    
      opened$type<-ifelse(opened$cross.sma50==0,               # set the type of position (long, short)  
                          "Short","Long")}
  }
  if (!is.null(opened)) {                                        # open if we have positions to open
    opened$buyprice<-ifelse(opened$type=="Long",opened$open,NA)
    opened$sellprice<-ifelse(opened$type=="Short",opened$open,NA)
    opened$range <- opened$up - opened$dn
    opened<-opened[order(opened$range),]                         # sort them by the risk 
    numtrades<-nrow(opened)                                      # we will take the best maxtrades to    
    if (numtrades>maxdaytrades) {                                # open - we will not exceed maxtrades
      opened<-opened[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    opened$tradeamount<-(equity*0.002/(0.02*opened$close))            # invest unequally using a risk percentage and stop loss
    # print(opened)
    if (numtrades>0 & (any(opened$tradeamount)>0)) {
      opened$position<-ifelse(opened$type=="Long",               # keep a record of the opening price
                              trunc(opened$tradeamount/opened$open),    # and the size of the position, negative
                              -trunc(opened$tradeamount/opened$open))   # position for shorts
      opened<-subset(opened,opened$position!=0)
      
    } else {opened<-NULL}
  } 
  print(opened)
  opened$price <- ifelse(opened$type=="Long",opened$buyprice,opened$sellprice)
  return(opened)  
}


# ***************************** APPLY RULES ************************************

applyRules=function(currdate,equity,position){
  
  netopen<-position                                        # netopen will hold all open positions after any close orders
  close.results<-closePositions(currdate,equity,position)  # close any orders for which we have positions and signals
  if (!is.null(close.results$closed)) {                    # Did we actually close out any positions
    temp<-close.results$close[,c(1,2)]                     # if we we need to remove them from our open positions
    names(temp)[2]<-"dummy"                                # we need one field to check if it is empty after the merge
    temp<-merge(position,temp,by="symbol",all.x=TRUE)      # and we don't want to generate duplicate columns, hence dummy
    netopen<-subset(temp,is.na(temp$dummy))                # so if dummy is NA, then the position is not closed
    netopen<-netopen[,c(1:ncol(netopen)-1)]                # get rid of the dummy column
    equity<-equity+close.results$cashin                    # update our equity position with the cash from closing
  }
  open.results<-openPositions(currdate,equity,netopen)     # now check for opening new positions
  return(list(open=open.results,close=close.results,
              posnetofcloses=netopen,cashin=close.results$cash,cashout=open.results$cash))
}

# *************************** REVIEW TRADES ************************************
reviewTrades=function(trades,tradetype) {
  done<-FALSE
  if (nrow(trades)>0){
    trades<-trades[,c("symbol","price","position")]
    rownames(trades)<-seq(1,nrow(trades),1)
  } else {
    done<-TRUE
    choice<-"Q"
  }
  while (!done) {
    View(trades)
    cat("\014")
    print("The first screen corresponds to details about opening positions while the second corresponds to closing positions for the day")
    print(paste("REVIEWING CANDIDATE",toupper(tradetype),
                " AND CHOOSING TO EXECUTE WILL SEND TRADES IMMEDIATELY TO IB."))
    choice<-readline(prompt="Choose D)elete trade, M)odify Price, C)hange position, E)xecute, Q)uit without trading:  ")
    choice<-toupper(choice)
    done<-ifelse(choice=="E"|choice=="Q",TRUE,FALSE)
    if (choice=="M"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to modify: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newprice<-as.numeric(readline(prompt=paste("Enter the new limit price for",
                                                   trades$symbol[rownum],": ")))
        valid<-is.numeric(newprice)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$price[rownum]<-newprice
        } else { 
          print("invalid price")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    if (choice=="C"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to change: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newposition<-as.numeric(readline(prompt=paste("Enter the new position size for",trades$symbol[rownum],": ")))
        valid<-is.numeric(newposition)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$position[rownum]<-newposition
        } else { 
          print("invalid position")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    if (choice=="D"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to delete: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        confirm<-as.character(readline(prompt=paste("Enter Y)es to confirm, N)o to abort removing the trade for ",trades$symbol[rownum],": ")))
        valid<-is.character(confirm)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          if (toupper(confirm)=="Y") {
            trades<-trades[-rownum,]
            if(nrow(trades)==0){
              print("Last candidate trade deleted")
              Sys.sleep(2)
            }
          }
        } else { 
          print("invalid entry")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
  }
  if (choice!="E"){
    trades<-trades[-c(1:nrow(trades)),]
  }
  return(trades)
}

# ************************************** EXECUTE TRADES **********************************************************

executeTrades=function(opentrades,closetrades,tradetype,tws){
  if (isConnected(twsconn=tws)){
    print("Transmitting Orders...")
    openAction<-ifelse(toupper(tradetype)=="LONG","BUY","SELL")
    closeAction<-ifelse(toupper(tradetype)=="LONG","SELL","BUY")
    
    if (!is.null(opentrades)) {
      for(i in c(1:nrow(opentrades))){ # Enter orders to open positions
        equity<-twsEquity(as.character(opentrades$symbol[i]),'SMART',primary="ISLAND")
        OrdId<-reqIds(tws)
        order<-twsOrder(OrdId,action=openAction,totalQuantity = abs(opentrades$position[i]),
                        orderType="MKT",tif="OPG") 
        placeOrder(tws,equity,order)
        # cancelOrder(tws,OrdId)      
      }
    }
    
    
    if (!is.null(closetrades)) {
      for(i in c(1:nrow(closetrades))){ # Enter orders to close positions
        equity<-twsEquity(as.character(closetrades$symbol[i]),'SMART',primary="ISLAND")
        OrdId<-reqIds(tws)
        order<-twsOrder(OrdId,action=closeAction,totalQuantity = abs(closetrades$position[i]),
                        orderType="MKT") 
        placeOrder(tws,equity,order)
        # cancelOrder(tws,OrdId)      
      }
    }
    
    dummy<-readline(prompt="Orders submitted, press <Enter> to continue:")
  } else {
    print("Connection with Interactive Brokers lost.  Trades not submitted!")
    dummy<-readline(prompt="Press <Enter> to continue:")
  }
}

oktoProceed=function(lasttradedate){
  done<-FALSE
  proceed<-"N"
  message<-paste("Make sure that the IB Gateway is open.  Last data date is",
                 as.character(lasttradedate),"proceed (Y/N)?")
  while (!done) {
    cat("\014")
    proceed<-readline(prompt=message)
    proceed<-toupper(proceed)
    done<-ifelse(proceed=="Y"|proceed=="N",TRUE,FALSE)
  }
  return(proceed)
}
# ********************************  RUN STRATEGY **********************************
cat("\014")
universe<-getData()
# save(universe,file = "universe_imported.rdata")
# load(file="universe_imported.rdata")

lasttradedate<-max(as.Date(universe$date))
proceed<-oktoProceed(lasttradedate)
if (proceed=="Y"){
  symbols<-unique(universe$symbol)
  indicators<-NULL            # we will put all OHLC data and our generated
  for (sym in symbols) {      # indicators into a dataframe named "indicators"
    temp<-genIndicators(sym)  # by looping through all the symbols 
    indicators<-rbind(indicators,temp)
  }
  signals <- NULL
  for (sym in symbols) {      # indicators into a dataframe named "indicators"
    temp<-genSignals(sym)  # by looping through all the symbols 
    signals<-rbind(signals,temp)
  }
  
  signals<-na.omit(signals)
}
# save(signals,file="signals.rdata")
# load(file="signals.rdata")
# save(indicators,file="indicators.rdata")
# load(file="indicators.rdata")

# ******************* OPEN CONNECTION & PROCESS TRADES  ************************

tws <-tryCatch({                  # IBport is a global parameter   
  tws = twsConnect(port=IBport)     # connection can fail with an error  
}, warning=function(w) {tws<-NULL }, error=function(e) {tws<-NULL})
if(!is.null(tws)) {
  acc<-reqAccountUpdates(tws)  
  portfolio <- twsPortfolioValue(acc)
  portfolio <- portfolio[,c("local","position")]
  if (!is.null(portfolio)) {
    portfolio$type <- ifelse(portfolio$position<0,"Short","Long")  
  }
  available<-max(as.numeric(acc[[1]]$AvailableFunds[1])/40,100000)
  trades<-applyRules(lasttradedate,available,portfolio)
  
  if (!is.null(trades[[1]])|!is.null(trades[[2]])) {
    
    if (!is.null(trades[[1]])) {
      openlongs<-reviewTrades(subset(trades[[1]],trades[[1]]$type=="Long"),"Long Trades")
    } else {openlongs <- NULL}
    
    if (!is.null(trades[[2]])) {
      closelongs<- reviewTrades(subset(trades[[2]],trades[[2]]$type=="Long"),"Long Trades")
    } else {closelongs <- NULL}
    
    
    if(!is.null(openlongs)|!is.null(closelongs)){
      executeTrades(openlongs,closelongs,"Long",tws)
    } else {print("No long trades executed")}
  } else {print("No long trades to execute.")}
  Sys.sleep(5)
  
  if (!is.null(trades[[1]])|!is.null(trades[[2]])) {
    
    if (!is.null(trades[[1]])) {
    openshorts<-reviewTrades(subset(trades[[1]],trades[[1]]$type=="Short"),"Short Trades")
    } else {openshorts <- NULL}
    
    if (!is.null(trades[[2]])) {
    closeshorts<-reviewTrades(subset(trades[[2]],trades[[1]]$type=="Short"),"Short Trades")
    } else {closeshorts <- NULL}
    
    if (!is.null(openshorts)|!is.null(closeshorts)) {
      executeTrades(openshorts,closeshorts,"Short",tws)
    } else {print("No short trades executed")}
  } else {print("No short trades to execute.")}
  twsDisconnect(tws)
} else {print("Aborting: Cannot connect to Interactive Brokers")}


