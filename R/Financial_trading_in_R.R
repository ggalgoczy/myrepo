#Financial trading in R on datacamp

install.packages('remotes')
remotes::install_github("braverock/quantstrat")

rm(list=ls())
library(quantmod)

getSymbols("SPY",
           to = "2016-07-30",
           from = "2000-01-01",
           src = "yahoo",
           adjust = TRUE)

plot(Cl(SPY))

#adding some SMA to the chart
library(TTR)
lines(SMA(Cl(SPY), n = 200), col = 'red')
#I can also add this to the xts
SPY$SMA <-SMA(Cl(SPY), n = 200)


###################lets initialize quantstrat package for backtesting###############
####################################################################################
library(quantstrat)
library(quantmod)
library(TTR)

# Create initdate, from, and to strings
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- '2015-12-31'

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD 
currency("USD")


#getting the data
getSymbols("SPY",
           to = to,
           from = from,
           src = "yahoo",
           adjust = TRUE)

stock('SPY', currency = 'USD')

# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of your strategy, portfolio and account
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Remove the existing strategy if it exists
rm.strat(___) #i would have to write "firststrat" here to remove the existing strategy



# Initialize the portfolio
initPortf(portfolio.st, symbols = 'SPY', initDate = initdate, currency = 'USD')

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = 'USD' , initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)


###################INDICATORS#############################
#Creating the indicators into different xts
spy_sma <- SMA(x= Cl(SPY), n=200)
spy_rsi <- RSI(Cl(SPY), n=3) #this one is from the datacamp description, IMO its too short lookback

# Plot the closing prices of SPY
plot(Cl(SPY))

# Overlay a 200-day SMA
lines(spy_sma, col = 'red')

#plotting the RSI
plot(spy_rsi)



###############ADD INDICATORS TO THE ALREADY EXISTING STRATEGY##################
# Add a 200-day SMA indicator to strategy.st
#add.indicator is like an apply() function
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA",   #its the function from TTR package 
              
              # Create a lookback period (for the SMA in this case)
              arguments = list(x=quote(Cl(mktdata)), n=200), #quote() ensures that the data can dynamically change over the course of running your strategy.
              
              # Label your indicator SMA200
              label = 'SMA200')

#add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA",   #its the function from TTR package 
              
              # Create a lookback period (for the SMA in this case)
              arguments = list(x=quote(Cl(mktdata)), n=50), #quote() ensures that the data can dynamically change over the course of running your strategy.
              
              # Label your indicator SMA50
              label = 'SMA50')

#add a 3-period RSI to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "RSI",   #its the function from TTR package 
              
              # Create a lookback period (for the SMA in this case)
              arguments = list(price=quote(Cl(mktdata)), n=3), #quote() ensures that the data can dynamically change over the course of running your strategy.
              
              # Label your indicator RSI_3
              label = 'RSI_3')


