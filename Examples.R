MOVING <- MovingForecast(c(1600,2200,2000,1600,2500,3500,3300,3200,3900,4700,4300,4400,NA),c(1,2,3,4,5,6,7,8,9,10,11,12,13),5,13,4,1)
WEIGHTED <- WeightedAverage(c(1600,2200,2000,1600,2500,3500,3300,3200,3900,4700,4300,4400,NA),c(1,2,3,4,5,6,7,8,9,10,11,12,13),.1,.2,.3,.4,5,13,4,1)
EXPONENTIAL <- Exponential(c(1600,2200,2000,1600,2500,3500,3300,3200,3900,4700,4300,4400,NA),c(1,2,3,4,5,6,7,8,9,10,11,12,13),.3,3,13)
ABS <- Absolute(MOVING$Actual,MOVING$FORECAST_MOVING)
SQ <- SquareError(MOVING$Actual,MOVING$FORECAST_MOVING)
PERCENT <- PercentError(MOVING$Actual,MOVING$FORECAST_MOVING)
ERROR <- Forecast_ErrorDf(ABS,SQ,PERCENT)
MOVING2 <- Forecast_and_Error(MOVING, ERROR)