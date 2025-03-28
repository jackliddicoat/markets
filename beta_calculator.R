library(quantmod)
beta_calculator <- function(symbol, days, plot = F) {
  stock <- getSymbols(symbol, from = Sys.Date() - days, auto.assign = F)
  getSymbols("SPY", from = Sys.Date() - days)
  stock_returns <- as.vector(dailyReturn(stock))
  spy_returns <- as.vector(dailyReturn(SPY))
  ols <- lm(stock_returns ~ spy_returns)
  beta <- summary(ols)$coefficients[2, 1]
  if (plot == T) {
    plot(stock_returns ~ spy_returns, 
         main = paste0(symbol, " vs SPY daily returns (past ", days, " days)\n", 
                       "Beta = ", round(beta, 4)),
         xlab = "SPY returns", ylab = paste0(symbol, " returns"))
    abline(ols, col = "red", lty = 2)
  }
  else {
    return(beta)
  }
}
beta_calculator("ARES", 120, plot = F)
