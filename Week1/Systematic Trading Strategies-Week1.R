          # ******************************QUESTION 1******************************

# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ************************************************************
library(rstudioapi)  # This is a external library of functions
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load("OHLC.rdata")  # contains customer demographic information

# Load the library into a variable
stock_data <-stock
#get no of trading days
x <- length(unique(stock$date))

#remove stocks which trade less than trading dates
stock_data <- stock_data %>%
  group_by(symbol) %>%
  filter(n() == x)

#Getting all openings on 2019-01-03(first trading day)
stock_data_start <- stock_data %>%
  group_by(symbol)%>%
  filter(date == "2019-01-03" )

#Getting all closings on 2019-12-30(last trading day)
stock_data_end <- stock_data %>%
  group_by(symbol)%>%
  filter(date == "2019-12-30" )

#Calculating Annual returns
annual_return <- ((stock_data_end$close-stock_data_start$open)/stock_data_start$open)*100
format(round(annual_return,1))

#Attaching annual returns to the data
stock_data_end <- cbind(stock_data_end,annual_return = annual_return)

#subsetting data to get stock symbols and annual returns
stock_data_end <- stock_data_end[c(1,8)]

# Identify the top 10 stocks in terms of total annual return
top_10_stocks <- stock_data_end %>%
  top_n(10,annual_return ) %>%
  arrange(desc(annual_return ))

# Identify the bottom 10 stocks in terms of total annual return
bottom_10_stocks <- stock_data_end %>%
  top_n(-10, annual_return ) %>%
  arrange(annual_return )

top_10_stocks$annual_return <- format(round(top_10_stocks$annual_return,1))

bottom_10_stocks$annual_return <- format(round(bottom_10_stocks$annual_return,1))
# Print the top 10 and bottom 10 stocks
print("Top 10 stocks:")
print(top_10_stocks)
print("Bottom 10 stocks:")
print(bottom_10_stocks)

# ***********************************************************************************************************************
# ******************************QUESTION 2******************************

# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************
library(rstudioapi)  # This is a external library of functions
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load("OHLC.rdata")  # contains customer demographic information

# Load the library

#get no of trading days
sectors <- read.csv("sectors.csv")
stock_data <-stock
#get no of trading days
x <- length(unique(stock$date))
#remove stocks which trade less than trading dates
stock_data <- stock %>%
  group_by(symbol) %>%
  filter(n() == x)

#Getting all openings on 2019-01-03
stock_data_start <- stock_data %>%
  group_by(symbol)%>%
  filter(date == "2019-01-03" )

#Getting all closings on 2019-12-30
stock_data_end <- stock_data %>%
  group_by(symbol)%>%
  filter(date == "2019-12-30" )


#Calculating Annual returns
annual_return <- ((stock_data_end$close-stock_data_start$open)/stock_data_start$open)*100
format(round(annual_return,1))

#Attaching annual returns to the data
stock_data_end <- cbind(stock_data_end,annual_return = annual_return)
stock_data_end <- stock_data_end[c(1,8)]
sectors <- merge(sectors,stock_data_end )


stock_data_end <- merge(stock_data_end,sectors)



mean_return_df <- sectors %>%
  group_by(sector) %>%
  summarize(mean_return = mean(annual_return))

mean_return_df[order(mean_return_df$mean_return, decreasing = TRUE),]

# ***********************************************************************************************************************
# ******************************QUESTION 3******************************

# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************
library(rstudioapi)  # This is a external library of functions
library(dplyr)
library(tidyverse)
library(gtools)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load("OHLC.rdata")  # contains customer demographic information

#Reading sectors file
sectors <- read.csv("sectors.csv")

#getting no of trading days
x <- length(unique(stock$date))

#remove stocks which trade less than total trading dates
stock_data <- stock %>%
  group_by(symbol) %>%
  filter(n() == x)

#grouping stocks by month
stock_data <- stock_data %>%
  group_by(month = lubridate::floor_date(date, 'month'))

#Extracting month from dates
stock_data$month <- format(stock_data$date,"%m")


#calculating monthly return for each stock
mat= NULL
x <- c("01","02","03","04","05","06","07","08","09","10","11","12")
for (symbol in sectors$symbol)
{
  for(val in x)
  {
    #print(x)
    c<-tail(stock_data[stock_data$symbol==symbol & stock_data$month==val,],1)
    o<-head(stock_data[stock_data$symbol==symbol & stock_data$month==val,],1)
    
    mat <- rbind(mat,c(sectors$sector[sectors$symbol==symbol],symbol,val,as.numeric((c$close - o$open)/o$open)*100))
    #print(o)
    #print(c)
    #print((c$close - o$open)/o$open)
    print("Please wait,You will see the matrix when its done processing!")
    
  }
  
}

#Cleaning the created dataset
mat<-data.frame(mat)
colnames(mat)<- c("sector","symbol","month","monthly_return")
mat$monthly_return<-as.numeric(mat$monthly_return)
mat<-na.omit(mat)

#Getting sector wise monthly returns
mat1 = NULL

rows <- unique(sectors$sector)


for(r in rows)
{
  for(val in x)
  {
    
    mat1 <- rbind(mat1,c(r,val,mean(as.numeric(mat$monthly_return[mat$sector==r & mat$month==val]))))
    #  ans[r][val] <- mean(as.numeric(mat$monthly_return[mat$sector==r & mat$month==val]))    
  }
}
colnames(mat1)<- c("sector","month","monthly_return")
mat1<-data.frame(mat1)


#Creating the answer matrix to be displayed 
ans = matrix(, nrow = length(unique(sectors$sector)), ncol = 12)


rnames=NULL
ans = NULL
ans1 =NULL
for(sector in rows)
{
  rnames <- rbind(rnames,sector)
  ans1 <- rbind(mat1$monthly_return[mat1$sector == sector])
  ans <- rbind(ans,ans1)
  
}

#Naming Matrix rows and cols
rownames(ans) = rnames
colnames(ans) = c("01","02","03","04","05","06","07","08","09","10","11","12")

#sorting matrix by row names alphabetically
ans <-ans[gtools::mixedorder(rownames(ans)),]

#display matrix
View(ans)

# ***********************************************************************************************************************
# ******************************QUESTION 4******************************

# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************
library(rstudioapi)  # This is a external library of functions
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load("OHLC.rdata")  # contains customer demographic information

# Load the Apple stocks
stock_data <-stock[stock$symbol=="AAPL",]

# Getting Daily Returns for apple stock
stock_data<-stock_data %>%
  mutate(daily_returns = ((close-open)/open))

#Calculating Cummulative and Max Cummulative Returns
stock_data$daily_returns = 1+stock_data$daily_returns
cumm_ret = stock_data$daily_returns[1]
max_cumm_return = cumm_ret
for(i in 2:250)
{
  cumm_ret<- rbind(cumm_ret,cumm_ret[i-1]*(stock_data$daily_returns[i]))
  max_cumm_return<- rbind(max_cumm_return,max(max_cumm_return,cumm_ret))
}
colnames(cumm_ret) <- c("cumm_return")

#Attaching the returns to our data set
stock_data <- cbind(stock_data,cumm_ret,max_cumm_return)

#Printing the plots for the returns
d_r_p<-ggplot(stock_data, aes(stock_data$date, stock_data$daily_returns))+ geom_line(size = 1, color = 'red') + labs(y="daily returns in %", x="Date")+ggtitle("Daily Returns")
print( d_r_p )

c_r_p<-ggplot(stock_data, aes(stock_data$date, stock_data$cumm_ret))+ geom_line(size = 1, color = 'green') + labs(y="cumm returns in %", x="Date")+ggtitle("Cummulative Returns")
print( c_r_p )

m_r_p<-ggplot(stock_data, aes(stock_data$date, stock_data$max_cumm_return))+ geom_line(size = 1, color = 'blue') +labs(y="max cumm returns in %", x="Date")+ggtitle("Max Cummulative Returns")
print( m_r_p)

p4 <- ggplot(stock_data, aes(x = date)) +
  geom_line(aes(y = stock_data$cumm_ret, group = 1, color = "Cumulative Return")) +
  geom_line(aes(y = stock_data$daily_returns, group = 1, color = "Daily Return"), linewidth = 0.7) +
  geom_line(aes(y = stock_data$max_cumm_return, group = 1, color = "Max Cumulative Return"), linetype = "dashed")+
  labs(y="returns in %", x="Date") + ggtitle("Combined Plot")

print(p4)
grid.arrange(d_r_p, c_r_p,m_r_p,p4, nrow = 2)

# ********************************************************************************************************************

