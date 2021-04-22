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
# Bullish Crossover
SPY.MACD$bull <- ifelse(SPY.MACD$sig == 1 & stats::lag(SPY.MACD$sig) == 0, 1, 0) #stats::lag is required due to dplyr::lag causing error
# Bearish Crossover
SPY.MACD$bear <- ifelse(SPY.MACD$sig == 0 & stats::lag(SPY.MACD$sig) == 1, -1, 0)
# Combine Crossovers
SPY.MACD$cross <- SPY.MACD$bull + SPY.MACD$bear
# Erase extra columns
SPY.MACD$bull <- NULL
SPY.MACD$bear <- NULL


# Attempt 3
#---------------------------------------------------------
# Zoo/xts is hard to work with. Temporary Change to tibble/data.frame for manipulation




# Attempt 2
#---------------------------------------------------------
# https://stackoverflow.com/questions/28886253/dplyr-mutate-in-zoo-object
SPY.MACD$cash <- 5000
SPY.MACD$position <- 0

sell.cash <- function(open, position, cash){cash = cash + (position * open)}
buy.position <- function(open, cash){position = round(cash/open)}
buy.cash <- function(open, position, cash){cash = cash - (position * open)}

SPY.MACD <- SPY.MACD %>% transform(trigger = lag(cross))

SPY.MACD <- SPY.MACD %>% transform(position = ifelse(trigger==1,
                                                     buy.position(SPY$Open, stats::lag(cash)),
                                                     ifelse(trigger==0,
                                                            stats::lag(position),
                                                            0)))


SPY.MACD <- SPY.MACD %>% transform(cash = ifelse(trigger==1,
                                                 buy.cash(SPY$Open, position, cash),
                                                 ifelse(trigger==-1,
                                                        sell.cash(SPY$Open, stats::lag(position), cash),
                                                        stats::lag(cash))))


SPY.MACD
