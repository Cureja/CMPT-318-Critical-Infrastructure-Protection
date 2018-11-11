get_period <- function(datetime){
  # From the Datetime datatype, create a column to represent what time period it is
  # 9am, 6pm, 12am, not inclusive
  period_end <- c(9, 18, 24)
  
  datetime$period[datetime$h < period_end[1]] <- "Morning"
  datetime$period[datetime$h >= period_end[1] & datetime$h < period_end[2]] <- "Midday"
  datetime$period[datetime$h >= period_end[2] & datetime$h < period_end[3]] <- "Evening"
  
  rm(period_end)
  return(datetime$period)
}

format_data <- function(df){
  # Format the dataset to remove/include columns
  
  # Only take Date, Time and Global_active_power
  df <- df[, c(1,2,3)]
  df$Datetime <- paste(df$Date, df$Time)
  df$Datetime <- as.POSIXlt(df$Datetime, format='%d/%m/%Y %H:%M:%S')
  # TRUE if the day is a weekday, False if the day is a weekend
  df$Weekday <- ifelse(df$Datetime$wday >= 1 & df$Datetime$wday <=5, TRUE, FALSE)
  df$Period <- get_period(df$Datetime)
  
  train_data <- na.omit(train_data)
  return(df)
}

train_data = read.table("data/Train Data.txt", 
                        header=TRUE, sep=',')
train_data <- format_data(train_data)

test1_data = read.table("data/test1.txt", 
                        header=TRUE, sep=',')
test1_data <- format_data(test1_data)

#Moving average of 7 observations. If the first observation and the average's difference is past a
#threshold, it will be added to the list of anomalies.
moving_average <- function(df, threshold) {
  anomalies <- data.frame()
  anomalies <- df[0, c(1:6)]
  for(i in c(7:nrow(df))) {
    window <- na.omit(df$Global_active_power[c((i - 7) : i)])
    average <- mean(window, na.rm=TRUE)
    if(abs(window[1] - average) > threshold){
      anomalies <- rbind(anomalies, df[(i - 6), ])
    }
  }
  for(i in 2:6) {
    if(abs(window[i] - average) > threshold) {
      anomalies <- rbind(anomalies, df[(nrow(df) - i), ])
    }
  }
  return(anomalies)
}

anoms <- moving_average(head(test1_data, 10000), 1)
anomstrain <- moving_average(head(train_data, 10000), 1)
