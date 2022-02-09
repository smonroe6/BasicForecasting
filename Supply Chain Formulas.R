## Breakeven Point ##
UNITS <- 15000
FC <- 25000
VC <- 5
MAKE_COST_REG <- FC + (VC * UNITS)

PRICE <- 7
BUY_COST_REG <- PRICE * UNITS

if (MAKE_COST_REG > BUY_COST_REG) {
  print("Buy")
} else if (MAKE_COST_REG < BUY_COST_REG) {
  print("Make")
} else {
  print("Either Works")
}

### Forecasts ###

TIME_PERIOD <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
ACTUAL <- c(1600,2200,2000,1600,2500,3500,3300,3200,3900,4700,4300,4400,NA)

## Moving Average ##
FORECAST_MOVING <- c()
for (i in 5:13) {
  FORECAST_MOVING[i] <- mean(ACTUAL[(i-4):(i-1)])
}
MOVING_AVERAGE <- data.frame(TIME_PERIOD,ACTUAL,FORECAST_MOVING)

## Weighted Moving ##
WEIGHTS <- c(.1,.2,.3,.4)
FORECAST_WEIGHTED <- c()
for (i in 5:13) {
  FORECAST_WEIGHTED[i] <- sum(ACTUAL[(i-4):(i-1)] * WEIGHTS)
}
WEIGHTED_AVERAGE <- data.frame(TIME_PERIOD,ACTUAL,FORECAST_WEIGHTED)

## Exponential Smoothing ##
ALPHA <- .3
FORECAST_EXPONENTIAL <- c(NA,ACTUAL[1])
for (i in 3:13) {
  FORECAST_EXPONENTIAL[i] <- FORECAST_EXPONENTIAL[i-1] + (ALPHA * (ACTUAL[i-1]-FORECAST_EXPONENTIAL[i-1]))
}
EXPONENTIAL <- data.frame(TIME_PERIOD,ACTUAL,FORECAST_EXPONENTIAL)

## Absolute Deviation ##
AbsoluteDeviation <- c()
  for (i in 1:13) {
    AbsoluteDeviation[i] <- abs(ACTUAL[i]- FORECAST_MOVING[i])
}

##Square Error ##
Square <- c()
  for (i in 1:13) {
    Square[i] <- (ACTUAL[i]- FORECAST_MOVING[i])^2
}

## Percent Error ##
Percent <- c()
  for (i in 1:13) {
    Percent[i] <- ((ACTUAL[i]- FORECAST_MOVING[i])/ACTUAL[i])*100
}

## Error Data Frame ##
Error <- data.frame(AbsoluteDeviation,Square,Percent)
MOVING_AVERAGE[,4:6] <- Error