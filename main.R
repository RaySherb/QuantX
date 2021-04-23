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
# Changing paramaters of MACD to (5, 34, 5) for a higher sensitivity
SPY.MACD <- MACD(SPY$Close, nFast = 5, nSlow = 34, nSig = 5)
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
# Hodl
hodlPrice <- SPY$OpenPrice[1]
hodlPosition <- round(5000/hodlPrice)-1
hodlCash <- 5000 - (hodlPosition*hodlPrice)
hodl <- SPY %>% mutate(assets=(Price*hodlPosition)+hodlCash)
hodl <- xts(hodl[,-1], order.by = hodl[,1])

#------------------------------------------------------------------------------
# https://school.stockcharts.com/doku.php?id=technical_indicators:moving_average_convergence_divergence_macd
# strat1: MACD crossing over signal line
#------------------------------------------------------------------------------

strat1 <- SPY
# If, at close, the macd is above signal then sig = 1, else sig = 0
strat1$sig <- ifelse(SPY$macd > SPY$signal, 1, 0)
# If today crossed over, give the direction
strat1 <- strat1 %>% mutate(cross = ifelse(sig==1 & lag(sig==0), 1,
                                           ifelse(sig==0 & lag(sig)==1, -1, 0)))

# Initial Conditions
strat1$cash <- 5000
strat1$position <- 0
# Functions for buying and selling
sell.cash <- function(open, position, cash){cash = cash + (position * open)}
buy.position <- function(open, cash){position = round(cash/open)-1}
buy.cash <- function(open, position, cash){cash = cash - (position * open)}
# Trigger that will execute a function
strat1 <- strat1 %>% mutate(trigger = lag(cross))
# Update position at trigger
for(i in 3:nrow(strat1)){
  if(strat1$trigger[i]==1){
    strat1$position[i] <- buy.position(strat1$OpenPrice[i], strat1$cash[i-1])   
  } else if(strat1$trigger[i]==-1){
    strat1$position[i] <- 0
  } else strat1$position[i] <- strat1$position[i-1]
}
# Update cash at trigger
for(i in 3:nrow(strat1)){
  if(strat1$trigger[i]==1){
    strat1$cash[i] <- buy.cash(strat1$OpenPrice[i], strat1$position[i], strat1$cash[i-1])
  } else if (strat1$trigger[i]==-1){
    strat1$cash[i] <- sell.cash(strat1$OpenPrice[i], strat1$position[i-1], strat1$cash[i-1])
  } else strat1$cash[i] <- strat1$cash[i-1]
}

# Strat 1 assets
strat1 <- strat1 %>% mutate(assets=(position*Price)+cash)


# Compare to buy and hold
graph <- xts(strat1[,-1], order.by = strat1[,1])
chart_Series(hodl$assets)
add_TA(graph$assets, col = 'blue', on=1)
