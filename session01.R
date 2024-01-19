# TA session 1 - Financial Econometrics @ BSE
# January 19th, 2024
t_max <- 500

## number of simulations
nsim <- 3
y_list <- matrix(rep(NA, nsim*t_max), nrow = t_max, ncol = nsim)

for (sim in 1:nsim) {
  U0 <- rnorm(1, sd = 10)
  z <- rnorm(t_max)
  y_list[, sim] <- U0 + 0.25*z
}


plot.ts(y_list[,1], ylim = c(-15, 10))
lines(y_list[,2], col = "steelblue")
lines(y_list[,3], col = "tomato")


## simulate x
nsim <- 10000
x_list <- matrix(rep(NA, nsim*t_max), nrow = t_max, ncol = nsim)

for (sim in 1:nsim) {
  set.seed(sim*123)
  z <- rnorm(t_max + 1)
  x_list[,sim] <- z[2:(t_max+1)] + z[1:t_max]
}

plot.ts(x_list[,1], ylim = c(min(x_list), max(x_list)))
lines(x_list[,2], col = "steelblue")
lines(x_list[,3], col = "tomato")



## US GDP data
x <- read.csv("data/us-gdp.csv")[,2]
plot.ts(x)


# annualized growth rates
xgrowth <- 4*diff(log(x)) * 100
plot.ts(xgrowth)

# moments
library(moments)
rbind(
  mean = mean(xgrowth),
  variance = var(xgrowth),
  skewness = skewness(xgrowth),
  kurtosis = kurtosis(xgrowth),
  min      = min(xgrowth),
  max      = max(xgrowth),
  above_5  = mean(xgrowth > 5),
  annualized_volatility = sqrt(4)*sd(xgrowth)
)

## autocovariance
gamma <- function(x, k) {
  k <- abs(k)
  t_max <- length(x)
  cov(x[1:(t_max-k)], x[(k+1):t_max])
}

## autocorrelation function
rho <- function(x, k) {gamma(x, k) / gamma(x, 0)}

## autocorrelation at different lags
sapply(0:12, rho, x = xgrowth)



acf(xgrowth)
pacf(xgrowth)


## hp testing
library(tseries)

## stationarity: ADF

adf.test(x)
adf.test(xgrowth)


## normality: JB
jarque.bera.test(xgrowth)


## mean zero: t-test
sigmaLR <- sum(sapply(-100:100, gamma, x = xgrowth))
t_stat <- mean(xgrowth)/(sqrt(sigmaLR/t_max))
p_value <- (1-pnorm(abs(t_stat)))*2






