# Load libraries
require(quantmod)
require(PerformanceAnalytics)

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
# Adjust for splits & dividends
SPY$Mult <- SPY$Adjusted/SPY$Close
SPY$Price <- SPY$Close
SPY$OpenPrice <- SPY$Open
SPY$Open <- SPY$Open*SPY$Mult
SPY$High <- SPY$High*SPY$Mult
SPY$Low <- SPY$Low*SPY$Mult
SPY$Close <- SPY$Adjusted
SPY$Adjusted <- NULL

# Step 2: Create indicators (TTR package included in quantmod)
###############################################################################
SPY.MACD <- MACD(SPY$Close)
SPY.MACD[is.na(SPY.MACD)] <- 0
# Plot indicator w/ subplot of price
chart_Series(SPY.MACD$macd)
add_TA(SPY.MACD$signal, col='blue', on=1)
add_TA(SPY$Close)

# Step 3: Construct your trading rule
###############################################################################
# If macd is above signal then sig = 1, else sig = 0
sig <- Lag(ifelse(SPY.MACD$macd > SPY.MACD$signal, 1, 0))
# If sig = 1 then buy and hold, else sell





# Step 4: The trading rules/equity curve
###############################################################################
ret <- ROC(Cl(SPY))*sig
ret <- ret['2017-06-02/2021-04-20']
eq <- exp(cumsum(ret))
plot(eq)

# Step 5: Evaluate strategy performance
###############################################################################
table.Drawdowns(ret, top=10)
table.DownsideRisk(ret)
charts.PerformanceSummary(ret)
