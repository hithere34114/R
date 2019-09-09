
# Introduction to R for Quantitative Finance
# ==========================================

# CHAPTER Nº1: Time Series Analysis

# Install packages
# install.packages("zoo")
# install.packages("forecast")
# install.packages("urca")
# install.packages("FinTS")
# install.packages("rugarch")

# Load packages
library("zoo")
library("forecast")
library("urca")
library("FinTS")
library("rugarch")

# Route in your pc (Set As Working Directory)
setwd("C:/Users/dayquipa/Desktop/Software/R/Introduction_to_R_for_Quantitative_Finance/Chapter_1")

# Create a zoo object called: "appl" from:
# daily closing prices of Apple's stock
aapl <- read.zoo("aapl.csv",
                 sep = ",",
                 header = TRUE,
                 format = "%Y-%m-%d")

# Plot the stock chart and specify a title for the overall plot (using the main argument)
# and labels for the "x" and "y" axis.
plot(aapl,
     main = "APPLE Closing Prices on NASDAQ",
     ylab = "Price (USD)",
     xlab = "Date")

# Show the first part of the object "appl"
head(aapl)

# Show the last part of the object "appl"
tail(aapl)

# Find the Apple's highest price
aapl[which.max(aapl)]

# Calculate Apple's stock returns
# Simple Return:
ret_simple <- diff(aapl) / lag(aapl, k = -1) * 100
# Continuously compounded returns
ret_cont <- diff(log(aapl)) * 100

# Summary statistics 
summary(coredata(ret_simple))

# The biggest single-day loss and the date when it ocurred
ret_simple[which.min(ret_simple)]

# Plot the histogram to get a better understanding of the relative frequency of daily returns
hist(ret_simple,
     breaks = 8, # the number of cells used to group the return data can be specified using "breaks" argument.
     main = "Histogram of Simple Returns",
     xlab = "%")
# A quick search on the Internet reveals that the large movement due to the issuance of a profit warning

# Restrict our analysis to a subset (a window) of the time series. 
aapl_2013 <- window(aapl, start = '2013-01-01', end = '2013-12-31')

# The highest stock price of Apple is 2019 (window subsetted)
aapl_2013[which.max(aapl_2013)]

# Determine the 1 day, 99% VaR of Apple's simple returns using a naive historical approach
quantile(ret_simple,
         probs = 0.01) 
# Interpretation: The probability that the return is below 7% on any given day is only 1%.
# But if this day occurs, 7% is the minimum amount you will lose.

# An important class of linear time series models is the family of Autoregressive
# Integrated Moving Average (ARIMA) models, proposed by "Box and Jenkins (1976).

# According to Box and Jenkins, building an ARIMA model consist of three stages:
# 1. Model identification
#       Determining the order (number of past values and number of past error terms to incorporate) of a tentative
#       model using either graphical methods or information criteria.
# 2. Model estimation
#       The parameters of the model need to be estimated, generally using either the lear squares or maximum
#       likelihood methods.
# 3. Model diagnostic checking
#       Fitted model must be carefully examined to check for possible model inadequacies, by making sure the
#       model residuals behave as a white noise, that is, there is no linear dependence left in the residuals.

# Now, we're going to model and forecast UK house prices:

# Store monthly house price data (Source: Nationwide Building Society)
hp <- read.zoo("UKHP.csv",
               sep = ",",
               header = TRUE,
               format = "%Y-%m",
               FUN = as.yearmon) # Represent the monthly data points
# To make sure we really stored monthly data
frequency(hp)

# Simple returs
hp_ret <- diff(hp) / lag(hp, k = -1) * 100

# With auto.arima function provided by the forecast package, we can identify the optimal model and estimate the
# coefficients in one step
mod <- auto.arima(hp_ret,
                  stationary = TRUE, # Restrict the search to stationary models
                  seasonal = FALSE,  # Restrict the search to non-seasonal models
                  ic = "aic")        # information criteria = "Akaike"

mod
# Interpretation: An AR(2) process sees to fit the data best, according to Akaike's Information Criteria.


# Plot the partial autocorrelation function (pacf)
pacf(hp_ret, 
     ag.max = 10,
     type="o",
     ylim=range(-1,1))
# Interpretation: It shows non-zero partial autocorrelations until lag two, hence an AR process of order two
# seems to be appropiate.

# Query the model output to determine the fitted coefficient values:
# Intercept and two AR coefficients are all significant at the 5% level since the respective confidence invervals
# do not contain zero.
confint(mod)

# A quick way to validate the model is to plot time series diagnostics using the following command:
tsdiag(mod)
# The model looks good since the standarized residuals because:
# - don't show volatility clusters.
# - no significant autocorrelations between the residuals according to the ACF plot.
# - the Ljung-Box test for autocorrelation shows high p-values, so (H0 = Independence residuals)  hull hypothesis of
# independent residuals cannot be rejected.

# Plot the raw monthly returns versus the fitted values
plot(mod$x, # Monthly returns (Black solid line)                                                 
     lty = 1,                                              
     main = "UK house prices: raw data vs. fitted values",
     ylab = "Return in percent",
     xlab = "Date")
lines(fitted(mod),
      lty = 2,
      lwd = 2,
      col = "red") # Fitted values (Red dotted line)

# Calculate common measures of accuracy 
accuracy(mod)
# - ME: Mean Error
# - RMSE: Root Mean Squared Error
# - MAE: Mean Absolute Error
# - MPE: Mean Percentage Error
# - MAPE: Mean Absolute Percentage Error
# - MASE: Mean Absolute Scaled Error
# - ACF1: Autocorrelation of errors at lag 1

# FORECASTING!

# Use predict function to predict the monthly returns for the "n.ahead" months, in this case 3.
predict(mod, n.ahead=3)
# Plot the forecast with standard errors
plot(forecast(mod))

# COINTEGRATION!

# Concept introduced by Granger (1981) and formalized by Engle and Granger (1987)
# The idea behind cointegration is to find a linear combination between non-stationary time series that result in 
# a stationary time series. 
# It is possible to detect stable long-run relationships between non-stationary time series.

# Cross hedgind jet fuel

# Most airlines hedge at least part of their exposure to jet fuel price changes.
# In this section, use the "urca" package

# Import monthly price data for jet fuel and heatin oil (in USD per gallon)
# First column is JetFuel
# Second column is HeatingOil
prices <- read.zoo("JetFuelHedging.csv",
                   sep = ",",
                   FUN = as.yearmon,
                   format = "%Y-%m",
                   header = TRUE)

# Taking into account only the short-term behavior of the two commodities
# The beta coefficient of the regression (linear model that explains changes in jet fuel prices by
# changes in hearing oil prices) is the "optimal hedge ratio".
simple_mod <- lm(diff(prices$JetFuel) ~ diff(prices$HeatingOil) + 0 ) # lm function is for regression
                                                                      # +0 : Set intercept to zero (No cash holdings)

# Summary of the regression
summary(simple_mod) # Obtain a hedge ratio of 0.89059
                    # Residual standard error of 0.0846
# The cross hedge is not perfect, the resulting hedged portfolio is still risky


# Try to improve on this hedge ratio
# Plot price series
plot(prices$JetFuel,
     main = "Jet Fuel and Heating Oil Prices",
     xlab = "Date",
     ylab = "USD")
lines(prices$HeatingOil,
      col = "red")

# Use Engle and Granger's two-step estimation technique.
# First: Test both time series for a unit root using the ADF test. (Non-stationary)
jf_adf <- ur.df(prices$JetFuel, type = "drift") # COINTEGRATION TEST!
summary(jf_adf)
summary(jf_adf)@teststat[1] # test statistic of -1.1335
summary(jf_adf)@cval[1,1]   # Critical value for test statistics -3.46
# H0 = Non-stationarity
# The null hypothesis of non-stationarity cannot be rejected at the 1% significance level.
# The test statistic of -1.1335 is not more negative than the critical value of -3.46.

# The same holds true for heating oil prices
ho_adf <- ur.df(prices$HeatingOil, type = "drift") # COINTEGRATION TEST!
summary(ho_adf)
summary(ho_adf)@teststat[1] # test statistic of -1.041041
summary(ho_adf)@cval[1,1]   # Critical value for test statistics -3,46

# Estimating the static equilibrium model 
mod_static <- summary(lm(prices$JetFuel ~ prices$HeatingOil))
# Testing the residuals for a stationary time series using an ADF-test. 
# Get the residuals of mod_static
error <- residuals(mod_static)
# Cointegration test for residuals
error_cadf <- ur.df(error, type = "none") 
summary(error_cadf)@teststat[1] # test statistic of -8.912047
summary(error_cadf)@cval[1,1]   # Critical value for test statistics -4.00
# H0 = Non-stationarity
# Reject the null hypothesis of non-stationarity.  
# The test statistic of -1.1335 is not more negative than the critical value of -3.46.
# ¡ We have discovered two cointegrated variables !

# Taking into account the long-term behavior of the two commodities
# ¿What is ECM?
# Represent a dynamic model of how (and how fast) the system moves back to the static equilibrium
# estimated earler.
djf <- diff(prices$JetFuel)
dho <- diff(prices$HeatingOil)
error_lag <- lag(error, k = -1)
mod_ecm <- lm(djf ~ dho + error_lag)
summary(mod_ecm)$sigma

# By taking into account the existence of a long-run relationship between jet fuel and heating oil 
# prices (cointegration):
mod_ecm$coefficients[2]            # - Higher ratio is now slightly higher
summary(mod_ecm)$sigma             # - Residual Standard Error significantly lower
summary(mod_ecm)$coefficients[[3]] # The coefficient of the error term is "negative" (-0.6555):
                                   # Large deviations between the two prices are going to be corrected
                                   # and prices move closer to their long-run stable relationship.
  
# We saw ARIMA Models used to model the conditional expectation of a process, given its past.
# For such a process, the conditional variance is constant.


# ARCH Model Specification

# Import monthly return data for Inter Corporation from Jan 1973 to December 2008.
intc <- read.zoo("intc.csv",
                 header = TRUE,
                 sep = ",",
                 format = "%Y-%m",
                 FUN = as.yearmon)

# Plot returns
plot(intc,
     main = "Monthly returns of Intel Corporation",
     xlab = "Date",
     ylab = "Return in percent")
# ARCH effects might exist
                                                                                           
# Use statistical hypothesis test to verify our inkling.
# Two commonly used test are as follows:
# 1. Ljung-Box test. For autocorrelation in squared returns (as a proxy for volatility).
Box.test(coredata(intc^2),
         type = "Ljung-Box",
         lag = 12)
# H0: No autocorrelations in the squared returns
# Reject the null hypothesis at 1% significance level.

# 2. Lagrange Multiplier (LM) by Engle (1982).
# In this case, use the FinTS package to emply LM test
ArchTest(coredata(intc))
# H0: No autocorrelations in the squared returns
# Reject the null hypothesis at 1% significance level.
# In both test, confirm that ARCH effect exist in the monthly returns of Intel Corporation

# GARCH Model Specification

# In this section, use "rugarch" package.
# Specifying the model using the function "ugarchspec".
# An appropiate model for financial time series is a GARCH(1,1) model
intc_garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), # garch order, GARCH(1,1)
                                mean.model = list(armaOrder = c(0, 0))) # Equation to mean model
                                                                        # Should be a white noise process

# Fitting coefficients by the method of maximum likelihood is done by "ugarchfit" function
intc_garch11_fit <- ugarchfit(spec = intc_garch11_spec, # Model specification
                              data = intc)              # Return data

# See results
intc_garch11_fit

# Use "ugarchroll" function to perform a historical backtest on the specified GARCH model
intc_garch11_roll <- ugarchroll(intc_garch11_spec,           # - GARCH Model Specification
                                intc,                        # - Return Data of Intel Corporation
                                n.start = 120,               # - The start period of the backtest
                                refit.every = 1,             # - The model should be reestimated every month
                                refit.window = "moving",     # - Use a moving window for the estimation
                                solver = "hybrid",           # - Use a hybrid solver  
                                calculate.VaR = TRUE,        # - We'd like to calculate the VaR 
                                VaR.alpha = 0.01,            # - Var at the 99% tail level (VaR alpha)
                                keep.coef = TRUE)            # - We'd like to keep the estimated coefficients

# Examine the backtesting report using the "report" function
report(intc_garch11_roll,    # - Backtest
       type = "VaR",         # - Type argument          
       VaR.alpha = 0.01,     # - Tail probability
       conf.level = 0.99)    # - The confidence level o which the conditional coverage hypothesis test will
                             # be based

# + Kupiec's unconditional coverage:
# Compares the number of expected versus actual exceedances given the tail probability of VaR.

# Christoffersen test:
# Is a joint test of the unconditional coverage and the independence of the exceedances

# The results of our Backtest: (H0: Correct Exceedances)
# Expected Exceed:   3,1
# ActualVaR Exceed:  5
# p-values are 0.325 (Kupiec)
#              0,568 (Christoffersen)
# So, we can't reject the H0 (null hypothesis) that the exceedances are correct and
# independent

# Extracted forecasted VaR from the "ugarchroll" object
intc_VaR <- zoo(intc_garch11_roll@forecast$VaR[, 1])

# Extracted actual returs of Intel Corporation from the "ugarchroll" object
intc_actual <- zoo(intc_garch11_roll@forecast$VaR[, 2])

# Overwrite the "index" property with "rownames" in both objects
index(intc_VaR) <- as.yearmon(rownames(intc_garch11_roll@forecast$VaR))
index(intc_actual) <- as.yearmon(rownames(intc_garch11_roll@forecast$VaR))

# Plot the VaR versus the actual returns of Intel Corporation
plot(intc_actual,
     type = "b",
     main = "99% 1 Month VaR Backtesting",
     xlab = "Date",
     ylab = "Return/VaR in percent")
lines(intc_VaR,
      col = "red")   # Color Red: VaR
legend("topright",
       inset = .05,
       c("Intel return","VaR"),
       col = c("black","red"),    # Black: Actual Returns, Red: VaR
       lty = c(1,1))

# We are sure that our risk model works properly

# Produce VaR forecasts with "ugarchforecast" function
intc_garch11_fcst <- ugarchforecast(intc_garch11_fit, # GARCH model
                                    n.ahead = 12)     # Nº periods (forecast)
intc_garch11_fcst

# Since we assume a normal distribution, the 99% VaR can be calculated using the 99% quantile.
qnorm(0.99)
qnorm(0.99) * 0.1167 # = 0.27. Hence, with 99% probability the monthly return is above -27%
