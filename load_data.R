get_period <- function(datetime){
  # From the Datetime datatype, create a column to represent what time period it is
  # 5am-9am, 9am-5pm, 5pm-9pm, 9pm-5am not inclusive
  period_end <- c(5, 9, 17, 21)
  
  datetime$period[datetime$h >= period_end[1] & datetime$h < period_end[2]] <- "Morning"
  datetime$period[datetime$h >= period_end[2] & datetime$h < period_end[3]] <- "Midday"
  datetime$period[datetime$h >= period_end[3] & datetime$h < period_end[4]] <- "Evening"
  datetime$period[datetime$h >= period_end[4] | datetime$h < period_end[1]] <- "Late Night"
  
  return(datetime$period)
}

format_data <- function(df){
  # Format the dataset to remove/include columns
  
  # Only take Date, Time and Global_active_power
  df <- df[, c(1,2,3)]
  df$Datetime <- paste(df$Date, df$Time)
  df$Datetime <- as.POSIXlt(df$Datetime, format='%d/%m/%Y %H:%M:%S')
  df$Date <- as.POSIXlt(df$Date, format='%d/%m/%Y')
  df$Time <- format(df$Time, format = "%H:%M:%S")
  # TRUE if the day is a weekday, False if the day is a weekend
  df$Weekday <- ifelse(df$Datetime$wday >= 1 & df$Datetime$wday <=5, TRUE, FALSE)
  df$Period <- get_period(df$Datetime)
  
  df <- na.omit(df)
  return(df)
}

train_data = read.table("data/Train Data.txt", 
                        header=TRUE, sep=',')
train_data <- format_data(train_data)


test1_data = read.table("data/test1.txt", 
                        header=TRUE, sep=',')
test1_data <- format_data(test1_data)

range <- train_data[0, c(1:2)]
range <- rbind(range, train_data[c(1:300), c(1:2)])

out_of_range <- function(df, range, col_num) {
  anomalies <- data.frame()
  anomalies <- df[0, c(1:6)]

  date_window <- subset(train_data, train_data$Date >= range[1, 1] 
                        & train_data$Date <= range[nrow(range), 1]
                        & train_data$Time >= range[1, 2]
                        & train_data$Time <= range[nrow(range), 2])
  maximum <- max(date_window[, col_num], na.rm = TRUE)
  minimum <- min(date_window[, col_num], na.rm = TRUE)
  for(i in 1:nrow(df)) {
    if(df[i, col_num] > maximum | df[i, col_num] < minimum ) {
      t <- Sys.time()
      anomalies <- rbind(anomalies, df[i, ])
      print(Sys.time() - t)
    }
  }
  return(anomalies)
}

anom_oor <- out_of_range(test1_data, range, 4)

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
