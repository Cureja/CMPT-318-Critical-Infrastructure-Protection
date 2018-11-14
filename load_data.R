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

get_season <- function(datetime) {
  # Creates a column to show which season the datapoint is from
  # 80  = March 21
  # 172 = June 21
  # 264 = Sept 21
  # 355 = Dec  21
  datetime$Season[datetime$yday >= 80 & datetime$yday <  172] <- "Spring"
  datetime$Season[datetime$yday >= 172 & datetime$yday <  264] <- "Summer"
  datetime$Season[datetime$yday >= 264 & datetime$yday <  355] <- "Autumn"
  datetime$Season[datetime$yday >= 355 | datetime$yday <  80] <- "Winter"
  return(datetime$Season)
}

format_data <- function(df){
  # Format the dataset to remove/include columns
  
  # Only take Date, Time and Global_active_power
  df <- df[, c(1:6)]
  df$Datetime <- paste(df$Date, df$Time)
  df$Datetime <- as.POSIXlt(df$Datetime, format='%d/%m/%Y %H:%M:%S')
  df$Date <- as.POSIXlt(df$Date, format='%d/%m/%Y')
  df$Time <- format(df$Time, format = "%H:%M:%S")
  # TRUE if the day is a weekday, False if the day is a weekend
  df$Weekday <- ifelse(df$Datetime$wday >= 1 & df$Datetime$wday <=5, TRUE, FALSE)
  df$Period <- get_period(df$Datetime)
  df$Season <- get_season(df$Datetime)
  
  df <- na.omit(df)
  return(df)
}

train_data = read.table("data/Train Data.txt", 
                        header=TRUE, sep=',')
train_data <- format_data(train_data)

test1_data = read.table("data/test1.txt", 
                        header=TRUE, sep=',')
test1_data <- format_data(test1_data)

test2_data = read.table("data/test2.txt", 
                        header=TRUE, sep=',')
test2_data <- format_data(test2_data)

test3_data = read.table("data/test3.txt", 
                        header=TRUE, sep=',')
test3_data <- format_data(test3_data)

test4_data = read.table("data/test4.txt", 
                        header=TRUE, sep=',')
test4_data <- format_data(test4_data)

test5_data = read.table("data/test5.txt", 
                        header=TRUE, sep=',')
test5_data <- format_data(test5_data)

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
moving_average <- function(df, threshold, col_num) {
  anomalies <- data.frame()
  anomalies <- df[0, ]
  for(i in c(7:nrow(df))) {
    window <- na.omit(df[c((i - 7) : i), col_num])
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

anoms <- moving_average(head(test1_data, 10000), 1, 4)
