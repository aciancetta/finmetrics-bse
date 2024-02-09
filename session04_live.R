## Financial Econometrics - TA session 4
## February 08, 2024


t_max <- 800
y0    <- 0
const <- 0
phi   <- 0.9
theta <- -0.5
sigma_eps <- 1

## Simulation ARMA(1,1)
eps <- rnorm(t_max+1, sd = sigma_eps)
y <- c()
y[1] <- y0
for (t in 2:t_max) {
  y[t] <- const + phi*y[t-1] + theta*eps[t-1] + eps[t]
}

plot.ts(y)

## Autocorrelogram
acf(y, lwd = 3)
pacf(y, lwd = 3)



## Estimation: filtering

## 1-step ahead conditional mean of ARMA(1,1) model
arma_filter <- function(x, params){
  # get parameters
  const <- params[1]
  phi <- params[2]
  theta <- params[3]

  ## initialize intercept, innovation
  mu <- rep(0, length(x))
  mu[1] <- const/(1-phi)
  eps <- rep(0, length(x))
  eps[1] <- x[1] - mu[1]

  # filter and innovations
  for(t in 2:length(x)){
    mu[t] <- const + phi*x[t-1] + theta*eps[t-1]
    eps[t] <- x[t] - mu[t]
  }

  # OUTPUT: mu = Tx1 vector of conditional means t|t-1;
  #         eps = Tx1 vector of innovations
  list(mu = mu,
       eps = eps)
}


arma_loglik <- function(x, params){
  # params now include (c, phi, theta, sigma2)
  t_max <- length(x)
  sigma2 <- params[4]
  eps <- arma_filter(x, params[1:3])$eps

  #compute loglik
  loglik <- -0.5*t_max*log(2*pi*sigma2) - (1/(2*sigma2))*sum(eps^2)
  -loglik
}

param_estim <- nlm(arma_loglik, p = c(0, 0.1, 0, 1), x = y)$estimate
cbind(c(const, phi, theta, sigma_eps), round(param_estim, 3))



