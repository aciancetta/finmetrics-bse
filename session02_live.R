## Financial Econometrics - TA session 2
## January 25, 2024

fredmd <- read.csv("data/current.csv")
dates <- as.Date(fredmd$sasdate[-1], "%m/%d/%Y")
tcodes <- fredmd[1, -1]
d <- fredmd[-1, -1]


# subset >=1980
data <- d[dates >= "1980-01-01", ]
dates <- dates[dates >= "1980-01-01"]
t_max <- length(dates)

# choose variables
target <- "UNRATE"
predictors <- c("S.P.500", "HOUST", "UMCSENTx")
vars <- c(target, predictors)
data <- data[, vars]
tcodes <- tcodes[, vars]

# plots
plot.ts(data[,2])


transform_fredmd <- list(
  function(x) x,
  function(x) c(0, diff(x)),
  function(x) c(0, 0, diff(x, differences = 2)),
  function(x) log(x),
  function(x) c(0, diff(log(x))),
  function(x) c(0, 0, diff(log(x), differences = 2)),
  function(x) c(0, diff( x[2:length(2)]/x[1:(length(x)-1)] -1 ))
)


## transform and make stationary
unrate <- transform_fredmd[[tcodes$UNRATE]](data$UNRATE)
sp500 <- transform_fredmd[[tcodes$S.P.500]](data$S.P.500)
houst <- transform_fredmd[[tcodes$HOUST]](data$HOUST)
sent <- transform_fredmd[[tcodes$UMCSENTx]](data$UMCSENTx)


## Fit predictive regression

## create dataframe
data_regression <- data.frame(
  unrate = unrate[2:t_max],
  unrate_lag = unrate[1:(t_max-1)],
  houst = houst[1:(t_max-1)],
  sp500 = sp500[1:(t_max-1)],
  sent = sent[1:(t_max-1)],
  d1 = (dates[2:(t_max)] == "2020-04-01")*1,
  d2 = (dates[2:(t_max)] == "2020-05-01")*1,
  d3 = (dates[2:(t_max)] == "2020-06-01")*1,
  d4 = (dates[2:(t_max)] == "2020-07-01")*1,
  d5 = (dates[2:(t_max)] == "2020-08-01")*1,
  d6 = (dates[2:(t_max)] == "2020-09-01")*1
)


## regression
pred_model <- lm(unrate ~ ., data = data_regression)
summary(pred_model)

library(sandwich)
library(lmtest)
vcov_nw <- NeweyWest(pred_model, prewhite = F)

## correct inference
coeftest(pred_model, vcov_nw)

## in-sample fit
plot.ts(data_regression$unrate)
lines(predict(pred_model), col = "darkred")


## analysis residuals
acf(pred_model$residuals)

Box.test(pred_model$residuals, lag = 12)




## local projections

H <- 12
lp <- rep(0, H)
lp_confint <- matrix(0, H, 2)

# compute the impact afer h = 1, ..., 12 horizons
for (h in 1:H) {
  # prepare the dataset
  data_h <- cbind(
    data_regression[h:t_max, c("unrate", paste0("d", 1:6))],
    data_regression[1:(t_max-h+1), c("unrate_lag", "houst", "sp500", "sent")]
  )

  # predict
  pred_model_h <- lm(unrate ~ ., data = data_h)
  vcov_nw_h <- NeweyWest(pred_model_h, prewhite = F)
  confint_h <- coeftest(pred_model_h, vcov_nw_h)

  # store results
  lp[h] <- confint_h["sp500", "Estimate"]
  lp_confint[h,] <- lp[h] + confint_h["sp500", "Std. Error"] * c(-1, 1) * qnorm(0.05)
}

plot(1:H, lp, type = "l", col = "darkred", lwd = 2, ylim = c(-2, 1))
lines(lp_confint[,1])
lines(lp_confint[,2])































