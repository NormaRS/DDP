# Developing Data Products - Course Project
# Portfolio analysis of an investment in 2 risky assets
# The asset chosen are: APPLE stock and MICROSOFT stock

library(tseries)
library(PerformanceAnalytics)
library(zoo)
library(shiny)
library(UsingR)

# Get the stock prices from Yahoo
aapl_prices = get.hist.quote(instrument="AAPL", start="2012-10-01", end="2015-10-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")
msft_prices = get.hist.quote(instrument="MSFT", start="2012-10-01", end="2015-10-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo")

index(aapl_prices) = as.yearmon(index(aapl_prices))
index(msft_prices) = as.yearmon(index(msft_prices))

# Create merged price data
all_prices = merge(aapl_prices, msft_prices)

# Rename columns
colnames(all_prices) = c("APPLE", "MICROSOFT")

# Calculate monthly returns
simple_returns = diff(all_prices)/lag(all_prices, k=-1);

# The ratio APPLE stock vs MICROSOFT stock (adds up to 1)
aapl_weights = seq(from=-1, to=2, by=0.1)
msft_weights = 1 - aapl_weights

# Portfolio parameters (annualized)
mu_hat_annual = apply(simple_returns,2,mean)*12   
sigma2_annual = apply(simple_returns,2,var)*12
sigma_annual = sigma2_annual^0.5
cov_mat_annual = cov(simple_returns)*12 
cov_hat_annual = cov(simple_returns)[1,2]*12    
rho_hat_annual = cor(simple_returns)[1,2]

# Assets parameters
mu_aapl = mu_hat_annual["APPLE"]
mu_msft = mu_hat_annual["MICROSOFT"]
sigma2_aapl =  sigma2_annual["APPLE"]
sigma2_msft = sigma2_annual["MICROSOFT"]
sigma_aapl = sigma_annual["APPLE"]
sigma_msft = sigma_annual["MICROSOFT"]
sigma_aapl_msft = cov_hat_annual
rho_aapl_msft = rho_hat_annual

# Portfolio expected return
mu_portfolio =  aapl_weights*mu_aapl + msft_weights*mu_msft

# Portfolio varianza
sigma2_portfolio =  aapl_weights^2 * sigma2_aapl + msft_weights^2 * sigma2_msft + 2 * aapl_weights * msft_weights * sigma_aapl_msft

# Portfolio standard deviation (risk)
sigma_portfolio = sqrt(sigma2_portfolio)

shinyServer(
  function(input, output) {
    output$myplot <- renderPlot({
      plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col="magenta")
      aapl_wt = input$aapl_wt
      ix = which(aapl_weights == as.character(round(aapl_wt, 1)))
      aapl_w = aapl_weights[ix]
      msft_w = msft_weights[ix]
      mu_w = mu_portfolio[ix]
      sigma_w = sigma_portfolio[ix]
      points(sigma_w, mu_w, col="black", lwd=5, pch=4)
      text(0.00, 0.10, paste("expected return = ", sprintf("%.1f %%", 100*round(mu_w, 2))), pos=4, cex=0.8)
      text(0.00, 0.08, paste("risk = ", sprintf("%.1f %%", 100*round(sigma_w, 2))), pos=4, cex=0.8)
      text(0.00, 0.06, paste("APPLE weight = ", sprintf("%.1f %%", 100*round(aapl_w, 1))), pos=4, cex=0.8)
      text(0.00, 0.04, paste("MICROSOFT weight = ", sprintf("%.1f %%", 100*round(msft_w, 1))), pos=4, cex=0.8)
    })
  }
)