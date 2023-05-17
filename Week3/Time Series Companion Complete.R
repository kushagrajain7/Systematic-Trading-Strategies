# Preliminaries
library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")


# **************************** Regression over the entire time period ***********************************************

load("OHLC.rdata")                                                  # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="APA")                            # let's take a look at Apple stock by itself
AAPL$day<-1:nrow(AAPL)                                              # need day number, rownumber will not work
model.lm<-lm(close~day,data=AAPL)                                   # regress on day number in series  
summary(model.lm)                                                   # print model summary     

# instead convert to time series and use the time series linear model (wrapper for lm function)
library(forecast) 
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)  # convert to time series object
model.tslm<-tslm(AAPL.ts~trend)                                     # regress using tslm on trend
summary(model.tslm)                                                  # print model summary

plot(close.ts)
lines(model.tslm$fitted.values)


# **************************** Regression with Rolling 20 period window  ***********************

error<-rep(NA,250)                                                        # vector to store errors
for (i in c(20:249)){                                                     # run through available days
  s<-i-20+1                                                               # find index to start of interval
  roll.ts<-subset(AAPL.ts,start=s,end=i)                                  # get interval data
  model.lm<-tslm(roll.ts~trend)                                           # run regression
  predclose<-forecast(model.lm,h=1)                                       # forecast one period out
  error[i+1]=abs(as.numeric(AAPL.ts[i+1])-as.numeric(predclose$mean))     # compute absolute error
}
mean(error,na.rm=TRUE)                                                    # report average abs error 



# ********************* Class Exercise: Optimize over the choice in the number of periods  ***********************

endtrain<-200
endtest<-250
meanerror<-rep(NA,25)
for (interval in c(3:35)) {
error<-rep(NA,endtest)
print(interval)
for (i in c(interval:endtrain-1)){                                  
  s<-i-interval+1
  roll.ts<-subset(AAPL.ts,start=s,end=i)
  model.lm<-tslm(roll.ts~trend)                         
  predclose<-forecast(model.lm,h=1)
  error[i+1]=abs(as.numeric(AAPL.ts[i+1])-as.numeric(predclose$mean))
}
meanerror[interval]<-mean(error,na.rm=TRUE)

}
bestinterval<-which.min(meanerror)                              # find best interval on training data

for (i in endtrain:endtest-1){                                  # Testing period from end of training to end of testing
  s<-i-bestinterval+1
  roll.ts<-subset(AAPL.ts,start=s,end=i)
  model.lm<-tslm(roll.ts~trend)                   
  predclose<-forecast(model.lm,h=1)
  error[i+1]=abs(as.numeric(AAPL.ts[i+1])-as.numeric(predclose$mean))
}
mean(error[c(endtrain+1:endtest)],na.rm=TRUE)


# *************************** Auto Regression over the entire time period ******************************************

library(forecast) 
load("OHLC.rdata")                                                    # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="APA")                              # let's take a look at Apple stock by itself
par (mfrow=c(1,2))
Acf(AAPL$close,lag.max=12, main="")
Pacf(AAPL$close,lag.max=12, main="")
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)

AAPL.arima <- Arima(AAPL.ts, order = c(1,0,0))
AAPL.arima
predclose<-forecast(AAPL.arima,h=1)
predclose
plot(predclose)
accuracy(predclose)
# *************************** Auto Regression with a rolling time period ******************************************

endtrain<-200
endtest<-250
meanerror<-rep(NA,40)
for (interval in c(20:40)) {
  error<-rep(NA,endtest)
  print(interval)
  for (i in c(interval:endtrain-1)){                                  
    s<-i-interval+1
    roll.ts<-subset(AAPL.ts,start=s,end=i)
    AAPL.arima <- Arima(roll.ts, order = c(1,1,0))
    predclose<-forecast(AAPL.arima,h=1)
    error[i+1]=abs(as.numeric(AAPL.ts[i+1])-as.numeric(predclose$mean))
  }
  meanerror[interval]<-mean(error,na.rm=TRUE)
  
}
bestinterval<-which.min(meanerror)

for (i in endtrain:endtest-1){                                  
  s<-i-bestinterval+1
  roll.ts<-subset(AAPL.ts,start=s,end=i)
  AAPL.arima <- Arima(roll.ts, order = c(1,1,0))
  predclose<-forecast(AAPL.arima,h=1)
  error[i+1]=abs(as.numeric(AAPL.ts[i+1])-as.numeric(predclose$mean))
}
bestinterval
mean(error[c(endtrain+1:endtest)],na.rm=TRUE)


# ***************************  Simple Moving Average ********************************
library(pracma) 
load("OHLC.rdata")                                                    # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="AAPL")                              # let's take a look at Apple stock by itself
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)
plot(AAPL.ts)
ma5<-movavg(AAPL.ts,5,type="s")
ma5.ts<-ts(ma5, start=c(1,1), end=c(1,250), frequency=250)
ma20<-movavg(AAPL.ts,20,type="s")
ma20.ts<-ts(ma20, start=c(1,1), end=c(1,250), frequency=250)
ma63<-movavg(AAPL.ts,63,type="s")
ma63.ts<-ts(ma63, start=c(1,1), end=c(1,250), frequency=250)
lines(ma5.ts,co=2,lw=2)
lines(ma20.ts,co=3,lw=2)
lines(ma63.ts,co=6,lw=2)


# ***************************  Double Moving Average ********************************

library(pracma) 
load("OHLC.rdata")                                                    # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="AAPL")                              # let's take a look at Apple stock by itself
AAPL.close<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250) # create the close time series
AAPL.high<-ts(AAPL$high, start=c(1,1), end=c(1,250), frequency=250)   # create the high time series
AAPL.low<-ts(AAPL$low, start=c(1,1), end=c(1,250), frequency=250)     # create the low time series
plot(AAPL.close)
ma5.high<-movavg(AAPL.high,5,type="s")
ma5.high<-ts(ma5.high, start=c(1,1), end=c(1,250), frequency=250)
dma5.high<-movavg(ma5.high,5,type="s")                                # create double moving average time series for high
dma5.high<-ts(dma5.high, start=c(1,1), end=c(1,250), frequency=250)
ma5.low<-movavg(AAPL.low,5,type="s")
ma5.low<-ts(ma5.low, start=c(1,1), end=c(1,250), frequency=250)     
dma5.low<-movavg(ma5.low,5,type="s")                                  # # create double moving average time series for low   
dma5.low<-ts(dma5.low, start=c(1,1), end=c(1,250), frequency=250)
lines(dma5.low,co=2,lw=2)
lines(dma5.high,co=2,lw=2)

# *************************** Average-Modified Method ********************************

library(pracma) 
load("OHLC.rdata")                                                    # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="AAPL")                              # let's take a look at Apple stock by itself
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)
plot(AAPL.ts)
ma5<-movavg(AAPL.ts,5,type="m")                                       # calculate 5 period modified average    
ma5.ts<-ts(ma5, start=c(1,1), end=c(1,250), frequency=250)
lines(ma5.ts,co=2,lw=2)
ma5<-movavg(AAPL.ts,5,type="s")                                       # calculate 5 period simple moving average for comparison
ma5.ts<-ts(ma5, start=c(1,1), end=c(1,250), frequency=250)            
lines(ma5.ts,co=3,lw=2)

# **************************Weighted Moving Average ********************************

library(pracma) 
load("OHLC.rdata")                                                    # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="AAPL")                              # let's take a look at Apple stock by itself
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)
plot(AAPL.ts)
ma5<-movavg(AAPL.ts,5,type="w")                                       # calculate 5 period weighted moving verage
ma5.ts<-ts(ma5, start=c(1,1), end=c(1,250), frequency=250)
lines(ma5.ts,co=2,lw=2)
ma5<-movavg(AAPL.ts,5,type="s")                                       # calculate 5 period simple moving average for comparison
ma5.ts<-ts(ma5, start=c(1,1), end=c(1,250), frequency=250)            
lines(ma5.ts,co=3,lw=2)

#  trdwma(y, weights, trim = FALSE)  in FRAPO package


#************************* Exponential Smoothing with ets() *************************

library(forecast)
load("OHLC.rdata")                                                        # load the R data file of s&P stock prices for 2019
AAPL<-subset(stock,stock$symbol=="APA")                                  # let's take a look at Apple stock by itself
AAPL.ts<-ts(AAPL$close, start=c(1,1), end=c(1,250), frequency=250)
plot(AAPL$close)
exp.simple<-ets(AAPL$close,model="ANN",alpha=0.2)                            # simple exponential smoothing with alpha=0.2
summary(exp.simple)

plot(AAPL.ts)
exp.simple<-ets(AAPL.ts,model="ANN",alpha=0.2)                            # simple exponential smoothing with alpha=0.2
summary(exp.simple)

lines(exp.simple$fitted,co=2,lw=2)
exp.simple<-ets(AAPL.ts,model="ANN",alpha=0.5)                            # simple exponential smoothing with alpha=0.5 
lines(exp.simple$fitted,co=4,lw=2)
exp.simple<-ets(AAPL.ts,model="ANN",alpha=0.8)                            # simple exponential smoothing with alpha=0.8
lines(exp.simple$fitted,co=11,lw=2)


plot(AAPL.ts)
exp.trend<-ets(AAPL.ts,model="AAN",alpha=0.5,beta = 0.3)                  # additive error, Addititive trend
lines(exp.trend$fitted,co=2,lw=2)

exp.best<-ets(AAPL.ts,model="ZZZ")                                        # fit best model
summary(exp.best)
plot(forecast(exp.best,h=8))

exp.best<-ets(AAPL.ts,model="ZZZ",damped=TRUE)                            # is a damped model better?
summary(exp.best)
plot(forecast(exp.best,h=8))
