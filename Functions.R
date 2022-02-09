## Breakeven Point##
Make <- function(FC,VC,Units) {
  FC + (VC * Units)
}

Buy <- function(Price,Units) {
  Price * Units
}

MakeBuyComparison <- function(Make,Buy) {
  if (Make > Buy) {
    print("Buy")
  } else if (Make < Buy) {
    print("Make")
  } else {
    print("Either Works")
  }
}

## Moving Forecast ##
MovingForecast <- function(Actual,TimePeriods,ForecastStartPeriod,ForecastEndPeriod,FarthestDistanceFromForecastedPeriod,ClosestDistanceToForecastPeriod) {
  FORECAST_MOVING <- c()
  for (i in ForecastStartPeriod:ForecastEndPeriod) {
    FORECAST_MOVING[i] <- mean(Actual[(i-FarthestDistanceFromForecastedPeriod):(i-ClosestDistanceToForecastPeriod)])
  }
  data.frame(TimePeriods,Actual,FORECAST_MOVING)
}

## Weighted Moving ##
WeightedAverage <- function(Actual,TimePeriods,WeightOldest,Weight2Oldest,Weight2Newest,WeightNewsest,ForecastStartPeriod,ForecastEndPeriod,FarthestDistanceFromForecastedPeriod,ClosestDistanceToForecastPeriod) {
  WEIGHTS <- c(WeightOldest,Weight2Oldest,Weight2Newest,WeightNewsest)
  FORECAST_WEIGHTED <- c()
  for (i in ForecastStartPeriod:ForecastEndPeriod) {
    FORECAST_WEIGHTED[i] <- sum(Actual[(i-FarthestDistanceFromForecastedPeriod):(i-ClosestDistanceToForecastPeriod)] * WEIGHTS)
  }
  data.frame(TimePeriods,Actual,FORECAST_WEIGHTED)
}

## Exponential Smoothing ##
Exponential <- function(Actual,TimePeriods,Alpha,ForecastStartPeriod,ForecastEndPeriod) {
  FORECAST_EXPONENTIAL <- c(NA,Actual[1])
  for (i in ForecastStartPeriod:ForecastEndPeriod) {
    FORECAST_EXPONENTIAL[i] <- FORECAST_EXPONENTIAL[i-1] + (Alpha * (Actual[i-1]-FORECAST_EXPONENTIAL[i-1]))
  }
  data.frame(TimePeriods,Actual,FORECAST_EXPONENTIAL)
}

## Absolute Deviation ##
Absolute <- function(Actual,Forecast){
  AbsoluteDeviation <- c()
  for (i in 1:13) {
    AbsoluteDeviation[i] <- abs(Actual[i]- Forecast[i])
  }
  AbsoluteDeviation
}

##Square Error ##
SquareError <- function(Actual,Forecast) {
  Square <- c()
  for (i in 1:13) {
    Square[i] <- (Actual[i]- Forecast[i])^2
  }
  Square
}

## Percent Error ##
PercentError <- function(Actual,Forecast) {
  Percent <- c()
  for (i in 1:13) {
    Percent[i] <- ((Actual[i]- Forecast[i])/Actual[i])*100
  }
  Percent
}

##Error Data Frame ##
Forecast_ErrorDf <- function(Absolute,Square,Percent) {
  Error <- data.frame(Absolute,Square,Percent)
}

## Combine Data Frames ##
Forecast_and_Error <- function(Forecast,Error) {
  Forecast[,4:6] <- Error
  Forecast
}