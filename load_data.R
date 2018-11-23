library(lubridate)
library(ggplot2)

get_period <- function(datetime){
  # From the Datetime datatype, create a column to represent what time period it is
  # 6am-9am, 9am-6pm, 6pm-9pm, 9pm-6am not inclusive
  period_end <- c(6, 9, 18, 21)
  
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
  df$Day_of_week <- weekdays(as.Date(df$Datetime))
  df$Month <- months(as.Date(df$Datetime))
  df$Period <- get_period(df$Datetime)
  df$Season <- get_season(df$Datetime)
  

  # df <- na.omit(df)
  return(df)
}

#dir = "C:/Users/10yen/Desktop/318/R/Project/CMPT-318-Critical-Infrastructure-Protection"
#setwd(dir)

train_data = read.table("data/Train Data.txt", 
                        header=TRUE, sep=',')
train_data <- format_data(train_data)

test1_data <- read.table("data/test1.txt", 
                         header=TRUE, sep=',')
test1_data <- format_data(test1_data)

test2_data <- read.table("data/test2.txt", 
                         header=TRUE, sep=',')
test2_data <- format_data(test2_data)

test3_data <- read.table("data/test3.txt", 
                         header=TRUE, sep=',')
test3_data <- format_data(test3_data)

test4_data <- read.table("data/test4.txt", 
                         header=TRUE, sep=',')
test4_data <- format_data(test4_data)

test5_data <- read.table("data/test5.txt", 
                         header=TRUE, sep=',')
test5_data <- format_data(test5_data)


winter_train_data <- subset(train_data, Season == "Winter")
winter_test1_data <- subset(test1_data, Season == "Winter")

#Takes the min and max of train_df, and compares it to a given df. Any points that fall above the max or
#below the min are deemed anomalous
out_of_range <- function(df, train_df, col_num) {
  anomalies <- data.frame()
  anomalies <- df[0, c(1:col_num)]

  maximum <- max(train_df[, col_num], na.rm = TRUE)
  minimum <- min(train_df[, col_num], na.rm = TRUE)
  for(i in 1:nrow(df)) {
    if(df[i, col_num] > maximum | df[i, col_num] < minimum ) {
      anomalies <- rbind(anomalies, df[i, ])
    }
  }
  return(anomalies)
}

oor <- out_of_range(winter_test1_data, winter_train_data, 3)

#Plot of the the out of range function when asked to compare the global active power of train_data and
#test1_data's global active power during the winter. Dotted lines are used to denote the max and min of
#the train data.
oorplot <- ggplot() +
  layer(data = winter_test1_data,
        mapping = aes(x=as.POSIXct(winter_test1_data$Date), y=winter_test1_data$Global_active_power, color = "Normal"), 
        geom = "point",
        stat = "identity", 
        position = position_identity()) + 
  layer(data = oor,
        mapping = aes(x=as.POSIXct(oor$Date), y=oor$Global_active_power, color = "Out of Range"), 
        geom = "point",
        stat = "identity", 
        position = position_identity()) +
  labs(x = "Date", y = 'Global Active Power') +
  geom_hline(yintercept = max(winter_train_data$Global_active_power), linetype ="dashed", color = "red") +
  geom_hline(yintercept = min(winter_train_data$Global_active_power), linetype ="dashed", color = "red")
print(oorplot)


#Moving average of 7 observations' data in a given column. If the first observation and the average's
#difference is past a threshold, it will be added to the list of anomalies.
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

anoms <- moving_average(test1_data, 1, 3)
