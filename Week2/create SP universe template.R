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
Quandl.api_key('XXXXXHFFwLTPPmFT-yE5')  # Dummy Key

# Get 5 years of S&P Data through the Quandl API

