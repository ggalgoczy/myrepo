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


#############MAKING MY OWN INDICATOR AND ADDING IT TO A STRATEGY##################

#Making a new RSI
# Write the calc_RSI_avg function
calc_RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  RSI_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  RSI_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of RSI_1 and RSI_2
  RSI_avg <- (RSI_1 + RSI_2)/2
  
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = 'calc_RSI_avg', arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = 'RSI_3_4')

##Making a new indicator called DVO

# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}


# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO",
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]


###############ADDING SIGNALS##################################
#with the following function i can create a filter for which scenarios the algo should look for entries

# Add a sigComparison which specifies that SMA50 must be greater than SMA200, call it longfilter
add.signal(strategy.st, name = "sigComparison", 
           
           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            
                            # Particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"),
           
           # Label this signal longfilter
           label = "longfilter")


