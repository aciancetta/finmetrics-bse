## Financial Econometrics - TA session 3
## February 02, 2024

## load data
fredmd <- read.csv("data/current.csv")
dates  <- as.Date(fredmd$sasdate[-1],'%m/%d/%Y')
target_variables <- c("UNRATE", "W875RX1", "GS10", "CPIAUCSL", "WPSFD49207",
                      "PAYEMS", "HOUST", "INDPRO", "M2SL", "S.P.500")

## get tcodes
tcodes <- fredmd[1,-1]
d      <- fredmd[-1, -1]


## Transform variables
# transformation functions
transform_fredmd <- list(
  function(x) x,
  function(x) c(0, diff(x)),
  function(x) c(0, 0, diff(x, differences = 2)),
  function(x) log(x),
  function(x) c(0, diff(log(x))),
  function(x) c(0, 0, diff(log(x), differences = 2)),
  function(x) c(0, 0, diff( x[2:length(x)]/x[1:(length(x)-1)] - 1))
)

# transform series one by one
for (j in 1:ncol(d)) {
  tcode <- tcodes[[j]]
  d[,j] <- transform_fredmd[[tcode]](d[,j])
}

## Subset and remove columns with NAs
d <- d[dates >= '1980-01-01', ]
dates <- dates[dates >= '1980-01-01']
idx_na <- (colSums(is.na(d))==0)
d <- d[, idx_na]
plot.ts(d[,target_variables])



## PCR
pcr <- function(d, target, horizon, n_compennts) {

  ## scale data
  d_scaled <- scale(d)
  d_mean <- attr(d_scaled, "scaled:center")
  d_sd <- attr(d_scaled, "scaled:scale")

  ## get target
  target_idx <- which(colnames(d) == target)
  y <- d_scaled[, target_idx]

  ## get the PCs
  x <- d_scaled[, -target_idx]
  eigen_list <- prcomp(x, center = FALSE)
  f <- eigen_list$x[, 1:n_components]

  ## define dataframe
  t_max <- length(y)
  d_pcr <- as.data.frame(cbind(
    y = y[(horizon+1):t_max],
    y_lag = y[1:(t_max-horizon)],
    f[1:(t_max-horizon),]
  ))

  ## fit model
  model <- lm(y ~ ., data = d_pcr)

  ## forecast
  pred <- predict(model, newdata = d_pcr[nrow(d_pcr), ])

  ## output
  pred*d_sd[target] + d_mean[target]
}

pcr(d, "INDPRO", 1, 4)



