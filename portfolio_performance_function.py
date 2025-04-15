import yfinance as yf
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 

def portfolio_performance_ts(stocks, shares, start_date = "2025-01-01"):
    spy = yf.download("SPY", start=start_date)["Close"]
    spy = spy.pct_change()
    spy = spy.reset_index().melt(id_vars="Date", var_name="symbol", value_name="return")
    spy["return"] = 1 + spy["return"]
    spy.loc[0, "return"] = 1
    spy["cum_return"] = np.cumprod(spy["return"])
 
    my_portfolio = yf.download(stocks, start=start_date)["Close"]
    returns = my_portfolio.pct_change()
    returns_long = 1 + returns.reset_index().melt(id_vars="Date", var_name="symbol", value_name="return")["return"]
    my_portfolio = my_portfolio.reset_index().melt(id_vars="Date", var_name="symbol", value_name="price")
    my_portfolio["return"] = returns_long
    my_portfolio["cumulative_return"] = my_portfolio.groupby("symbol")["return"].cumprod()
    my_portfolio.groupby("Date")["price"].sum()
    
    shares = dict(zip(stocks, shares))
    first_prices = my_portfolio.sort_values("Date").groupby("symbol").first()["price"]
    initial_value = {symbol: first_prices[symbol] * shares[symbol] for symbol in shares}
    total_value = sum(initial_value.values())
    weights = {symbol: initial_value[symbol] / total_value for symbol in shares}
    my_portfolio["weight"] = my_portfolio["symbol"].map(weights)
    my_portfolio["weighted_cum_return"] = my_portfolio["weight"] * my_portfolio["cumulative_return"]
    portfolio_cumulative = my_portfolio.groupby("Date")["weighted_cum_return"].sum().reset_index(name="portfolio_cumulative_return")
    portfolio_cumulative.loc[0, "portfolio_cumulative_return"] = 1
    
    my_return = round(portfolio_cumulative["portfolio_cumulative_return"].iloc[-1] - portfolio_cumulative["portfolio_cumulative_return"].iloc[0], 3)
    spy_return = round(spy["cum_return"].iloc[-1] - spy["cum_return"].iloc[0], 3)
    
    plt.plot(portfolio_cumulative["Date"], portfolio_cumulative["portfolio_cumulative_return"], label = f'You: {my_return*100}%')
    plt.plot(spy["Date"], spy["cum_return"], label = f"SPY:{spy_return*100}%")
    plt.legend()
    plt.show()


stocks = ["TSLA", "AAPL", "GOOG"]
shares = [20, 20, 20]

portfolio_performance_ts(stocks, shares, start_date="2024-06-01")