
# Introduction to R for Quantitative Finance
# ==========================================

# CHAPTER Nº2: Portfolio Optimization

# Install packages
# install.packages("xts")
# install.packages("tidyquant")
# install.packages("timeSeries")
# install.packages("fPortfolio")

# Load packages
library(tidyquant)
library(xts)
library(timeSeries)
library(fPortfolio)

# Route in your pc (Set As Working Directory)
setwd("C:/Users/dayquipa/Desktop/Software/R/Introduction_to_R_for_Quantitative_Finance/Chapter_2")

# Mean-Variance Model (By Markowitz)

# Let's start with a simple self-made R function. This is a direct implementation of an algorithm:
minvariance <- function(assets, mu = 0.005) {
                        return <- log(tail(assets, -1) / head(assets, -1))
                        Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))
                        Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
                        b <- c(rep(0, ncol(assets)), 1, mu)
                        solve(Q, b)
}

# Use "tidiquant" package to download financial time series:
# In this case, we download Google, Amazon, Microsoft, IBM and Home Depot
IT <- tq_get(c("GOOG","AMZN", "MSFT", "IBM", "HD"),
             get  = "stock.prices",
             from = "2008-01-01",
             to   = "2012-12-31")

IT <- IT %>%
      select(symbol, date, adjusted) %>%
      spread(., symbol, adjusted)


# Remove "date" column, and show only stock prices
assets <- IT[, -1]
assets 

tail(assets, -1)
head(assets, -1 )
# Create "return" object, by dividing each but the first value (tail) with 
# the preceding (head) and computing "log" for each quotient.
return <- log(tail(assets, -1) / head(assets, -1))
# Show "return" object 
head(return)

# Lagrange Theorem:
# Combine the covariance matrix (cov), ones repeated (rep) by the number of
# columns (ncol) in the dataset and the means (colMeans) of the returns as
# rows (rbind).
Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))
round(Q, 5) 

# Combine the last two rows of the matrix (tail) as new columns (rbind) on the left
# to make it complete for the linear system with the extra zeros specified in the
# Lagrange theorem (matrix of 2x2).
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
round(Q, 5)

# By default, mu is 0.005 (specified in the minvariance function's argument)
mu <- 0.005
b <- c(rep(0, ncol(assets)), 1, mu)
b

# After successfully building the parts of the linear equality system, you are only
# left with the task of solving it:
solve(Q, b)
minvariance(IT[, -1])
# Results are the same, the "minvariance" function is equivalent.

# We can also get the minimun variance for a larger range of returns
frontier <- function(assets) {
            return <- log(tail(assets, -1) / head(assets, -1))
            Q <- cov(return)
            n <- ncol(assets)
            r <- colMeans(return)
            Q1 <- rbind(Q, rep(1, n), r)
            Q1 <- cbind(Q1, rbind(t(tail(Q1, 2)), matrix(0, 2, 2)))
            rbase <- seq(min(r), max(r), length = 100)
            s <- sapply(rbase, function(x) {
              y <- head(solve(Q1, c(rep(0, n), 1, x)), n)
              y %*% Q %*% y
            })
            plot(s, rbase, xlab = 'Return', ylab = 'Variance')
            }

# Plot the return-variance pairs (s and rbase) to illustrate the solution
# of the problem
frontier(assets) # Portfolio Frontier, ignoring its downward sloping part,
                 # we get Efficient Frontier

# Now, create a table with time series format (Using "xts" package)
IT <- cbind(IT[, 1],IT[, 2:6])
IT <- xts(IT, order.by = IT[,1])[,-1]
storage.mode(IT) <- "numeric" # convert from string to numeric format 

# Check number of rows and columns
nrow(IT)
ncol(IT)

# Verify "return" function with log(lag(IT)/IT) and show it
log(lag(IT) / IT)
IT_return <- returns(IT)[-1,]
IT_return

# Plot returns (using "timeSeries" package)
chart.CumReturns(IT_return,
                 legend.loc = 'topleft',
                 main = '')

# Interactuvely frontier chart (Using "fPortfolio" package)
plot(portfolioFrontier(as.timeSeries(IT_return)))

# To mimic the preceding code, let us render the Frontier plot of short sale constraint:
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"  # Optimizes an unlimited short selling portfolio                                                   
Frontier <- portfolioFrontier(as.timeSeries(IT_return), Spec, constraints = "Short")  
frontierPlot(Frontier, col = rep('orange', 2), pch = 19)
monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
grid()

# Modify the variance minimization:
n <- 6        # n: number of assets including the riskless one
mu <- 0.005
Q <- cbind(cov(return), rep(0, n - 1))
Q <- rbind(Q, rep(0, n))
rf <- 0.0001   # rf: risk free

# Use the new covariance matrix and the new return vector to determine the optimal
# portfolio weights and then eliminate the nth asset based on the minvariance code
r <- c(colMeans(return), rf)
Q <- rbind(Q, rep(1, n), r)
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
b <- c(rep(0, n), 1, mu)

round(Q, 6)
b

# Market portfolio
w <- solve(Q, b)
w <- head(w, -3)
w / sum(w)

# When variance is not enough
# Variance as a risk measure is convenient, but has some drawbacks. For instance,
# when using variance, positive changes in the return can be considered as the increase
# of risk. 
# Method applied agains the previously describe:
Spec <- portfolioSpec()
setSolver(Spec) <- "solveRshortExact"
setTargetReturn(Spec) <- mean(colMeans(timeSeries(IT_return)))
efficientPortfolio(timeSeries(IT_return), Spec, 'Short')
minvariancePortfolio(timeSeries(IT_return), Spec, 'Short')
minriskPortfolio(timeSeries(IT_return), Spec)
maxreturnPortfolio(timeSeries(IT_return), Spec)
# These R expressons return different portfolio weights computed by various methods not
# discussed in this introductory chapter.