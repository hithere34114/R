
# Introduction to R for Quantitative Finance
# ==========================================

# CHAPTER Nº3: Asset Pricing Models

cat("\f") # Clear the Console
rm(list = ls()) # Clear the environment

# Install packages
# install.packages("tidyquant")
# install.packages("dplyr")
# install.packages("xts)
# install.packages("timetk")

# Load packages
library(tidyquant)
library(dplyr)
library(xts)
library(timetk)

# CAPM.
# Gives an answer to the question asking what can be said of the market by aggregating the rational
# investors' decisions and, also, by what assumption the equilibrium would evolve.
# Sharpe (1964) & Litner (1965), prove the existence of the equilibrium subject to the following assumption:
# - Individual investors are prices takers
# - Single-period investment horizon
# - Investments are limited to traded financial assets
# - No taxes and no transaction costs
# - Information is costless and available to all investors
# - Inverstors are rational mean-variance optimizers
# - Homogenous expectations
# In a world where these assumptions are held, all investors will hold the same portfolio of risky assets
# which is the market portfolio.
# The risk in CAPM is measured by the beta, which is a function of the individual security's covariance with 
# the market and the variance of the market return.

# Beta shows:
# - The sensibility of a stock's return to the return of the market portfolio,
# - A certain security's beta show how much risk that security add to the market portfolio.

# CAPM states that the market gives a higher return only in cases of higher systematic risk since unsystematic
# risk can be diversified, so no risk premium can be paid after that.

# CAPM states that in equilibrium, every security should be on the SML; so, this equiation holds for each security
# or portfolio even if they are not efficient. If this equation is not fulfilled, there is a lack of equilibrium
# on the market. *SML: Security Market Line

# APT: Arbitrage Pricing Theory (Ross 1997)
# Used in finance to determine the return of different securities.
# States that, in equilibirum, no arbitrage opportunity can exist and, also, that the expected return of an asset
# is the linear combination of multiple random factors (Wilmott 2007). These factors can be various macro-economic
# factors or market indices. In this model each factor has a specific beta coefficient.
# A central notion of the APT is the factorportfolio. A factorportfolio is a well-diversified portfolio which
# reacts to only one of the factors, so it has zero beta for all other factors, and a beta of 1 to that specified
# factor. Assuming the existence of the factorportfolios, it can be shown using the arbitrage argument that any
# well-diversified portfolio's risk premium is equal to the weighted sum of the factorportfolios' risk premium.

# There is only one factor in the APT model, which is the return of the market portfolio, we call the model the
# index model.

# If alpha is zero, we will get the exact princing formula of CAPM

# Differences between the CAPM and APT are as follows:

# 1. CAPM is an equilibrium model, building on economic considerations, while APT is a statistical model, using
# arbitrage arguments
# 2. In the case of APT, an expected return-beta relation can be given if one has a well-diversified portoflio so
# that this can be constructed in practice by having a large number of assets in the portfolio. While, in the
# case of CAPM, the so-called market portfolio cannot be constructed.
# 3. CAPM states that the expected return-beta relation holds for every security, while APT states that this is
# for almost every security.
# 4. When there is mispricing on the market, in the case of APT, it is enough if only a few investors change the
# portfolio structure to get the fair price of a secutiry, while, in the case of CAPM, every investor has to do
# so.

# BETA ESTIMATION

# The sensitivity of a secutiry towards a factor can be estimated from past price movements.
# We will estimate the beta from the one-factor index mode.

# DATA SELECTION

# Use "tidyquant" package to download data and select "date" and "close" columns.
# "Google" from Yahoo finance (using "stock.prices" in the code)
G <- tq_get("GOOG",
            get  = "stock.prices",
            from = "2009-06-01",
            to   = "2013-06-01") %>%
     tk_xts(date_col = date) %>%
     na.omit()
G <- G$adjusted

# "SP500" from Yahoo finance (using "stock.prices" in the code)
SP500 <- tq_get("^GSPC",
            get  = "stock.prices",
            from = "2009-06-01",
            to   = "2013-06-01") %>%
         tk_xts(date_col = date) %>%
         na.omit()
SP500 <- SP500$adjusted

# "Libor" from FRED (using "economic.data" in the code)
LIBOR <- tq_get("USD1MTD156N",
                get  = "economic.data",
                from = "2009-06-01",
                to   = "2013-06-01") %>%
         tk_xts(date_col = date) %>%
         na.omit()
LIBOR <- LIBOR$price

# Intersec dates (Normalize dates between SP500, Google, LIBOR)
cdates <- as.Date(Reduce(intersect, list(index(G), index(SP500),index(LIBOR))), format = "%Y-%m-%d")

# Get dates and price with "cdates" object
G <- G[index(G) %in% cdates, 'adjusted']
SP500 <- SP500[index(SP500) %in% cdates, 'adjusted']
LIBOR <- LIBOR[index(LIBOR) %in% cdates, 'price']

# Create a function for logreturns
logreturn <- function(x) log(x/lag(x)[-1,])

# Get risk free daily return
rft <- log(1 + LIBOR[-1,]/36000 * diff(as.numeric(cdates)))[-1,]
str(rft)
  
# Simple Beta Estimation
cov(logreturn(G) - rft, logreturn(SP500) - rft) / var(logreturn(SP500) - rft)

# Risk premium function
riskpremium <- function(x) logreturn(x) - rft
cov(riskpremium(G), riskpremium(SP500)) / var(riskpremium(SP500))

# Compute the regression model
(fit <- lm(riskpremium(G) ~ riskpremium(SP500)))

# plot the characteristic line of Google on a chart that shows the risk premium of Google as a function of the
# market risk premium.
plot(x = as.vector(riskpremium(SP500)), y = as.vector(riskpremium(G)))
abline(fit, col = 'red')

# Compute the regression model
# According to CAPM, ?? equals to zero, therefore we will assume ??i to be 0, then we
# release this restriction. We can force ?? to be zero by passing -1 in the model:
(fit <- lm(riskpremium(G) ~ -1 + riskpremium(SP500)))
summary(fit)
summary(lm(riskpremium(G) ~ riskpremium(SP500)))

# We can check the residuals on a joint plot as shown in the following figure:
par(mfrow = c(2, 2))
plot(fit)

# Model testing

# The first tests on the beta-return relationship used two-phase linear regression

# Set the name of symbols on Yahoo finance
symbols <- c("A", "AA", "AAPL", "ABC", "ABT", "ACE", "ACN", "ACT",
             "ADBE", "ADI", "ADM", "ADP", "ADSK", "AEE", "AEP", "AES","AET", "AFL",
             "AGN", "AIG", "AIV", "AIZ", "AKAM", "ALL", "ALTR", "ALXN", "AMAT", "AMD",
             "AMGN", "AMP", "AMT", "AMZN", "AN", "ANF", "AON", "APA", "APC", "APD",
             "APH", "APOL", "ARG", "ATI", "AVB", "AVP", "AVY", "AXP", "AZO", "BA",
             "BAC", "BAX", "BBBY", "BBT", "BBY", "BCR", "BDX", "BEAM", "BEN", "BF.B",
             "BHI", "BIIB", "BK", "BLK", "BLL", "BMC", "BMS", "BMY", "BRCM", "BRK.B",
             "BSX", "BTU", "BXP", "C", "CA", "CAG", "CAH", "CAM", "CAT", "CB", "CBG",
             "CBS", "CCE", "CCI", "CCL", "CELG", "CERN", "CF", "CHK", "CHRW", "CI",
             "CINF", "CL", "CLF", "CLX", "CMA", "CMCSA", "CME","GOOG","^GSPC")

# Libor
LIBOR <- tq_get("USD1MTD156N",
                get  = "economic.data",
                from = "2003-01-01",
                to   = "2007-01-01") %>%
         tk_xts(date_col = date) %>%
         na.omit() %>%
         tk_xts(date_col = date)
LIBOR <- LIBOR$price


# Descargamos los symbols de Yahoo finance y ordenamos por columnas
stocks <- tq_get(symbols,
                get  = "stock.prices",
                from = "2003-01-01",
                to   = "2007-01-01") %>%
          select(date,symbol,adjusted) %>%
          na.omit(.) %>%
          spread(., symbol, adjusted) %>%
          tk_xts(date_col = date)

# Filter, dates per month
filter_month <- as.numeric(format(as.Date(index(stocks),format="%Y-%m-%d"), format = "%d"))
dif <- data.frame(matrix(nrow=1, ncol = length(filter_month)))
dif[,i] <- NA
for(i in 2:length(filter_month)){
dif[,i] <- filter_month[i] - filter_month[i-1]
if( dif[i] < 0){
dif[,i] = "Ok"} else {
dif[,i] = "No"}
}
dif <- t(dif)
filter_month <- data.frame(index(stocks),dif) %>%
                filter(.,dif == "Ok") %>%
                select(index.stocks.)
filter_month <- as.Date(filter_month$index.stocks., format = "%Y-%m-%d")

# Get dates and price with "cdates" object
cdates <- as.Date(Reduce(intersect, list(filter_month, index(stocks),index(LIBOR))), format = "%Y-%m-%d")
stocks <- stocks[index(stocks) %in% cdates, colnames(stocks)]
LIBOR <- LIBOR[index(LIBOR) %in% cdates, 'price']

# Data base with all stocks
Data_Base <- cbind(stocks,as.vector(LIBOR$price))

# Create a empty vector for "betas" and "means" and fill it with a loop. After that, merge into a data.frame "beta_mean"
beta <- data.frame(matrix(nrow=1,ncol=ncol(stocks)-2))
mean <- data.frame(matrix(nrow=1,ncol=ncol(stocks)-2))
for(i in 2:(ncol(stocks)-1)){
beta[i-1] <- lm(stocks[,i] ~ stocks$`^GSPC`)$coefficients[2]
colnames(beta)[i-1] <- colnames(stocks)[i]
mean[i-1] <- mean(logreturn(stocks[,i]))
colnames(mean)[i-1] <- colnames(stocks)[i]
}

# Set betas and means in an object: "r" 
r <- na.omit(data.frame(t(beta),t(mean))); colnames(r) <- c("beta","mean")
head(r)

# Plot betas and means 
par(mfrow = c(1, 1))
plot(x=r$beta, y=r$mean)
abline(lm(r$mean ~ r$beta), col = 'red')

# Show the summary of the model
summary(lm(r$mean ~ r$beta))

# Reset functions setted before
rft <- log(1 + LIBOR[-1,]/36000 * diff(as.numeric(cdates)))[-1,]
logreturn <- function(x) log(x/lag(x)[-1,])
riskpremium <- function(x) logreturn(x) - rft

# Create "risk" object and save as a column in "r" data.frame
risk <- vector(length = nrow(r))
i = 1
for(i in 1:nrow(r)){
name <- rownames(r)[i]  
risk[i] <- t(var(riskpremium(stocks[,name])) - r$beta[i]^2 * var(riskpremium(stocks[,name]))) 
}
r <- r %>%
     mutate(risk = risk)

# Show the summary of the regression:
summary(lm(r$mean ~ r$beta + r$risk))
