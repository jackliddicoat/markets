import tidyfinance as tf
import pandas as pd 
from datetime import date

def day_portfolio_performance(stocks, shares, date_of_return = date.today(), see_individual_pct = True,
                              see_absolute_chg = True):
    portfolio = tf.download_data(domain="stock_prices", symbols = stocks, start_date="2024-01-01", end_date= date.today())
    portfolio = portfolio.sort_values(["symbol", "date"])
    portfolio["return"] = portfolio.groupby("symbol")["adjusted_close"].pct_change()
    portfolio["return"] = round(portfolio["return"], 3)
    returns = {}
    for symbol in stocks:
        stock_data = portfolio[portfolio["symbol"] == symbol]
        if date_of_return == date.today():
            last_return = stock_data["return"][stock_data["date"] == date_of_return.strftime("%Y-%m-%d")]
        else:
            last_return = stock_data["return"][stock_data["date"] == date_of_return]
        returns[symbol] = last_return
    if see_individual_pct:
        for x in returns:
            print(x,':',''.join(map(str, returns[x].values*100)),"%")
    if see_absolute_chg:
        portfolio["abs_return"] = portfolio.groupby("symbol")["adjusted_close"].diff()
        last_return = portfolio["abs_return"][portfolio["date"] == date_of_return]
        last_price = portfolio["adjusted_close"][portfolio["date"] == date_of_return]
        data = {"last_return": last_return, 'price': last_price}
        df = pd.DataFrame(data)
        df['shares'] = shares
        df["change"] = df["shares"]*df["last_return"]
        df["total"] = df['shares']*df['price']
        print(f"Total account change: ${round(sum(df.change), 3)}")
        print(f"Total account balance: ${round(sum(df.total), 3)}")

stocks = ["TSLA", "AAPL", "GOOG"]
shares = [20, 20, 20]

day_portfolio_performance(stocks, shares, date_of_return = "2025-04-14", see_individual_pct = True, see_absolute_chg = True)