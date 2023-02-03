#This file is just to test whether i can upload basic r files
library(tidyquant)

getSymbols("AAPL", from = '2022-01-01',
           to = "2022-10-29",warnings = FALSE,
           auto.assign = TRUE)
head(AAPL)
class(AAPL)
chart_Series(AAPL)
chart_Series(AAPL['2022-10-01/2022-10-29'])

