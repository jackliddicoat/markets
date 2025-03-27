import yfinance as yf
import statsmodels.api as sm

def beta_portfolio(stocks, shares, date_from):
    stocks_data = yf.download(stocks, start=date_from)["Close"]
    prices = stocks_data.iloc[-1]  # Get the latest prices

    spy = yf.download("SPY", start=date_from)["Close"]
    spy_change = spy.pct_change().dropna()

    betas = {}  # Store beta values for each stock

    for stock in shares.keys():
        stock_returns = stocks_data[stock].pct_change().dropna()
        
        # Ensure both stock and SPY have matching dates
        combined_data = stock_returns.align(spy_change, join='inner', axis=0)
        stock_returns, spy_change_aligned = combined_data

        # Run regression (adding constant for intercept)
        reg = sm.OLS(stock_returns, sm.add_constant(spy_change_aligned)).fit()
        betas[stock] = reg.params[1]  # Beta is the slope

    # Calculate total portfolio value
    total_value = sum(shares[stock] * prices[stock] for stock in shares)

    # Compute weighted beta
    weighted_beta = sum((shares[stock] * prices[stock]) / total_value * betas[stock] for stock in shares)

    print(f"Total Portfolio Value: ${total_value:.2f}")
    print(f"Portfolio Beta: {weighted_beta:.4f}")

# Example usage:
stocks = ["SPY", "AAPL", "TSLA"]
shares = {"SPY": 10, "AAPL": 20, "TSLA": 40}
date_from = "2023-01-01"

beta_portfolio(stocks, shares, date_from)
