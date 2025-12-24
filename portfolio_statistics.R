library(quantmod)

portfolio_statistics <- function(tickers, shares, ndays = 365, year = 2025) {
  #load the data in
  data_list <- lapply(tickers, function(sym) {
    getSymbols(sym, src = "yahoo", auto.assign = FALSE, 
               from = Sys.Date() - ndays)
  })
  
  N <- nrow(data_list[[1]])
  Ticks <- length(data_list)
  
  #fillers for different matrices and vectors
  returns <- matrix(NA, nrow = N, ncol = Ticks)
  prices <- matrix(NA, nrow = N, ncol = Ticks)
  
  for (i in 1:Ticks) {
    prices[,i] <- as.numeric(Cl(data_list[[i]]))
    returns[, i] <- as.numeric(dailyReturn(Cl(data_list[[i]])))
  }
  
  #names
  colnames(returns) <- tickers; colnames(prices) <- tickers
  
  #make the weights of the portfolio
  portfolio_value <- as.numeric(prices %*% shares)
  scaled_prices <- sweep(prices, 2, shares, `*`) # does rowwise multiplication
  weights <- (scaled_prices)/portfolio_value
  weights <- as.numeric(colMeans(weights))
  
  #covariance matrix
  Sigma <- cov(returns)
  
  #portfolio variance and sd
  var_portfolio <- as.numeric(t(weights) %*% Sigma %*% weights)
  sd_portfolio  <- sqrt(var_portfolio)*sqrt(252)
  
  #betas
  getSymbols("SPY", src = "yahoo", from = Sys.Date() - ndays)
  sds <- sapply(data.frame(returns), sd)
  spy_returns <- dailyReturn(SPY)
  spy_sd <- sd(dailyReturn(SPY))
  cors <- as.numeric(cor(returns, spy_returns))
  betas <- cors*(sds/spy_sd)
  weighted_beta <- as.numeric(weights %*% betas)
  
  #return
  return_portfolio <- last(portfolio_value)/first(portfolio_value) - 1
  
  #sharpe
  daily_ret <- diff(portfolio_value) / head(portfolio_value, -1)
  ann_return <- mean(daily_ret) * 252
  ann_sd <- sd(daily_ret) * sqrt(252)
  sharpe_ratio <- ann_return / ann_sd
  
  return(list("exp_return" = return_portfolio, "sd" = sd_portfolio, "beta" = weighted_beta,
              "sharpe" = sharpe_ratio))
}
