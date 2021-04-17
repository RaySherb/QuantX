library(quantmod)
library(tidyverse)

# Load data
#############################################################################
# start date 
dt <- '2017-2-1'
SPY <- getSymbols.yahoo('SPY', from=dt, periodicity = 'daily', auto.assign=F)
names(SPY) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
colSums(is.na(SPY))

# Adjust for Splits & Dividends
#############################################################################
SPY$Mult <- SPY$Adjusted/SPY$Close
SPY$Price <- SPY$Close
SPY$OpenPrice <- SPY$Open
SPY$Open <- SPY$Open*SPY$Mult
SPY$High <- SPY$High*SPY$Mult
SPY$Low <- SPY$Low*SPY$Mult
SPY$Close <- SPY$Adjusted
SPY$Adjusted <- NULL

# Indicators
#############################################################################
# SMA (simple moving average)
#--------------------------------------------------------------
n <- 20
SPY$SMA20 <- rollapply(SPY[,'Close'],
                             width = n,
                             FUN = mean,
                             by.column = TRUE,
                             fill = NA,
                             align = 'right')

# Bollinger Bands (Same window size as SMA)
#--------------------------------------------------------------
SPY$sd <- rollapply(SPY[,'Close'],
                          width = n,
                          FUN = sd,
                          by.column = TRUE,
                          fill = NA,
                          align = 'right')
SPY$UBB <- SPY$SMA20 + 2*SPY$sd
SPY$LBB <- SPY$SMA20 - 2*SPY$sd

# MACD (Moving average convergence divergence oscillator)
#--------------------------------------------------------------
n1 <- 5
n2 <- 34
SPY$MACD <- rollapply(SPY[,'Close'],
                      width = n2,
                      FUN = function(v){mean(v[(n2-n1+1):n2])-mean(v)},
                      by.column = TRUE,
                      fill = NA,
                      align = 'right')

# Rolling Sharpe Ratio (20-days)
#--------------------------------------------------------------
SPY$Sharpe20 <- SPY$SMA20/SPY$sd

# Plot
#############################################################################
fortify.zoo(SPY) %>% ggplot(aes(Index))+
  geom_line(aes(y=Close))+
  geom_line(aes(y=UBB), color='red')+
  geom_line(aes(y=LBB), color='red')

# Strats
#############################################################################

c <- 5000 # Uninvested Cash
k <- 0 # Number of positions held
K <- 10 # Maximum positions held

# Buy at positive cross-over
SPY$buy <- ifelse(SPY$MACD > 0 & Lag(SPY$MACD) < 0, 1, 0)
# Sell at negative Cross-over
SPY$sell <- ifelse(SPY$MACD < 0 & Lag(SPY$MACD) > 0, -1, 0)
#pg 74/217