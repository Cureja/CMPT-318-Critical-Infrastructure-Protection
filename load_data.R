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
test1_data <- format_data(train_data)

