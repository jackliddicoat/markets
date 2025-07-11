library(quantmod)
library(treasury)

portfolio_return_and_sd <- function(tickers, shares, ndays = 90) {
  data_list <- lapply(tickers, function(sym) {
    getSymbols(sym, src = "yahoo", auto.assign = FALSE, from = Sys.Date() - ndays)
  })
  returns <- matrix(nrow = nrow(data_list[[1]]), ncol = length(data_list))
  sds <- numeric()
  for (i in 1:length(data_list)) {
    returns[,i] <- as.vector(dailyReturn(data_list[[i]]))
    sds[i] <- sd(returns[,i])
  }
  colnames(returns) <- tickers
  weights <- shares / sum(shares)
  var_portfolio <- 0
  for (j in 1:length(weights)) {
   var_portfolio <- var_portfolio + weights[j]^2 * sds[j]
  }
  for (k in 1:length(weights)) {
    for (l in k:length(weights)) {
      var_portfolio <- var_portfolio + 2*cor(returns[,k],returns[,l])*sds[k]*sds[l]
    }
  }
  var_portfolio
  
  cum_returns <- last(apply(1+returns, 2, cumprod))
  weighted_return <- sum(cum_returns*weights) - 1
  
  return(list("exp_return" = weighted_return, "sd" = sqrt(var_portfolio)))
}

sharpe <- function(portfolio, ndays = 90, year, plot = T) {
  portfolio <- portfolio_return_and_sd(stocks, shares)
  t_bill_rates <- tr_bill_rates(year)
  t_bill_rates <- t_bill_rates %>% 
    filter(type == "close", maturity == "13 weeks") %>% 
    tail(ndays)
  exp_return <- portfolio$exp_return
  std <- portfolio$sd
  rf_rate <- t_bill_rates$value/100
  sharpe_ratio <- (exp_return - rf_rate)/std
  return(summary(sharpe_ratio))
}

tickers <- c("SPY", "VTI", "GOOGL")
shares <- c(10, 20, 30)
portfolio_return_and_sd(tickers, shares)
sharpe(portfolio, year = "2025")

