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
#with this, the function will always check whether the sma50 is  above sma200 and state if the condition holds
add.signal(strategy.st, name = "sigComparison", 
           
           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            
                            # Particularly, we are interested when the SMA50 is greater than the SMA200
                            relationship = "gt"),
           
           # Label this signal longfilter
           label = "longfilter")

##now i will add a signal that indicates if the sma50 is below the sma200 but only for the 1st time the condition applies

# Add a sigCrossover which specifies that the SMA50 is less than the SMA200 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           
           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"),
                            
                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),
           
           # Label it filterexit
           label = "filterexit")

##the sigThreshold function can be used with oscillator types of indicators to check where they are compared to a given
## threshold.
##If i set the cross parameter to FALSE then the function acts like a sigComparison, so it always compares the value 
##of the indicator to a value always
## with cross=TRUE, it acts as sigCrossover function

##so with sigThreshold and cross=FALSE i could make a 'switch' that needs to be on, for entering a position

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")

# Add a sigThreshold signal to your strategy that specifies that DVO_2_126 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold", 
           
           # Reference the column of DVO_2_126
           arguments = list(column = "DVO_2_126", 
                            
                            # Set a threshold of 80
                            threshold = 80, 
                            
                            # The oscillator must be greater than 80
                            relationship = "gt", 
                            
                            # We are interested only in the cross
                            cross = TRUE), 
           
           # Label it thresholdexit
           label = "thresholdexit")

##################################sigFormula######################################
#In this exercise, you will get a taste of what the sigFormula function can do by stepping through the 
#logic manually. You will need to use the applyIndicators() and applySignals() functions.

# Create your dataset: test (testing what i have so far)
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)



# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.st, name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")

##########################RULES################################
#rules are for setting how to enter and exit into positions based on the indicators and signals

# Fill in the rule's type as exit
add.rule(strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")
#replace: should other signals be cancelled?
#in general it should be left false

#prefer: when to enter a position
#the position will be entered in the next candle once the signals are present and with the prefer we can say
#at which position it should enter. open means at the Open price

#replace: In quantstrat, the replace argument specifies whether or not to ignore all other signals on the 
#same date when the strategy acts upon one signal. This is generally not a desired quality in a well-crafted 
#trading system. Therefore, for your exit rule, you should set replace to FALSE.


#Using add.rule() to implement an entry rule

# Create an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal", 
         
         # Use the longentry column as the sigcol
         arguments=list(sigcol = "longentry", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to 1
                        orderqty = 1,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        # Take the long orderside
                        orderside = "long",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")

############
# Add a rule that uses an osFUN to size an entry position
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE, ordertype = "market",
                          orderside = "long", replace = FALSE, prefer = "Open",
                          
                          # Use the osFUN called osMaxDollar
                          osFUN = osMaxDollar,
                          
                          # The tradeSize argument should be equal to tradesize (defined earlier)
                          tradeSize = tradesize,
                          
                          # The maxSize argument should be equal to tradesize as well
                          maxSize = tradesize),
         type = "enter")
