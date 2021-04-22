# Load libraries
require(quantmod)
require(PerformanceAnalytics)
require(tidyverse)
# Step 1: get the data
###############################################################################
# Start date
dt <- '2017-2-1'
# SPY is initial ticker to generalize algos for
SPY <- getSymbols.yahoo('SPY', from=dt, periodicity = 'daily', auto.assign=F)
# Rename columns
names(SPY) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
# Verify no missing data
colSums(is.na(SPY))
# Visualize difference between close and adjusted close
chartSeries(SPY$Close, type="line")
addTA(SPY$Adjusted, on=1)
addTA(SPY$Volume)

SPY <- as_tibble(SPY, rownames=NA) %>% rownames_to_column(var='date') %>%
  mutate(date=as.Date(date),
         Mult=Adjusted/Close,
         Price=Close,
         OpenPrice=Open,
         Open=Open*Mult,
         High=High*Mult,
         Low=Low*Mult,
         Close=Adjusted) %>% select(-Adjusted)

# Step 2: Create indicators (TTR package included in quantmod)
###############################################################################
# Note: TTR doesn't require xts/zoo
SPY.MACD <- MACD(SPY$Close)
# Replace NA's with 0
SPY.MACD[is.na(SPY.MACD)] <- 0
# Add as columns to SPY tibble
SPY <- SPY %>% cbind(SPY.MACD)
# Remove unnecessary object
rm(SPY.MACD)

# Plot indicator w/ subplot of price
# Xts objects required
graph <- xts(SPY[,-1], order.by = SPY[,1])
chart_Series(graph$macd)
add_TA(graph$signal, col = 'blue', on=1)
add_TA(graph$Close)

# Step 3: Construct your trading rule
###############################################################################
# If, at close, the macd is above signal then sig = 1, else sig = 0
SPY$sig <- ifelse(SPY$macd > SPY$signal, 1, 0)
# If today crossed over, give the direction
SPY <- SPY %>% mutate(cross = ifelse(sig==1 & lag(sig==0), 1,
                                     ifelse(sig==0 & lag(sig)==1, -1, 0)))

# Initial Conditions
SPY$cash <- 5000
SPY$position <- 0
# Functions for buying and selling
sell.cash <- function(open, position, cash){cash = cash + (position * open)}
buy.position <- function(open, cash){position = round(cash/open)-1}
buy.cash <- function(open, position, cash){cash = cash - (position * open)}
# Trigger that will execute a function
SPY <- SPY %>% mutate(trigger = lag(cross))
# Update position at trigger
for(i in 3:nrow(SPY)){
  if(SPY$trigger[i]==1){
    SPY$position[i] <- buy.position(SPY$OpenPrice[i], SPY$cash[i-1])   
  } else if(SPY$trigger[i]==-1){
    SPY$position[i] <- 0
    } else SPY$position[i] <- SPY$position[i-1]
}
# Update cash at trigger
for(i in 3:nrow(SPY)){
  if(SPY$trigger[i]==1){
    SPY$cash[i] <- buy.cash(SPY$OpenPrice[i], SPY$position[i], SPY$cash[i-1])
  } else if (SPY$trigger[i]==-1){
    SPY$cash[i] <- sell.cash(SPY$OpenPrice[i], SPY$position[i-1], SPY$cash[i-1])
    } else SPY$cash[i] <- SPY$cash[i-1]
}

# Hodl
startPrice <- SPY$OpenPrice[1]
startPosition <- round(5000/startPrice)-1
startCash <- 5000 - (startPosition*startPrice)
SPY <- SPY %>% mutate(assets=(position*Price)+cash,
                      hodl=(Price*startPosition)+startCash)

# Compare to buy and hold
graph <- xts(SPY[,-1], order.by = SPY[,1])
chart_Series(graph$hodl)
add_TA(graph$assets, col = 'blue', on=1)
