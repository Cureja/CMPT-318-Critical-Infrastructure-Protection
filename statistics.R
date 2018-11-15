get_period_average <- function(input_df, feature) {
  # Get the average value for a feature by period
  # finds the statistics by season, period and day type
  column_names = c("Season", "Period", "Weekday")
  df_mean <- setNames(aggregate(input_df[[feature]] ~ Season+Period+Weekday, input_df, FUN=mean), c(column_names, "mean"))
  df_sd <- setNames(aggregate(input_df[[feature]] ~ Season+Period+Weekday, input_df, FUN=sd), c(column_names, "sd"))
  df_quantile <- aggregate(input_df[[feature]] ~ Season+Period+Weekday, input_df, 
                           function(x) quantile(x, probs = seq(0,1, 0.25)))
  df_quantile <- lapply(df_quantile, unlist)
  df_quantile <- setNames(data.frame(df_quantile), c(column_names, 'min', '25%', '50%', '75%', 'max'))

  df = Reduce(function(x,y) merge(x,y, by=column_names), list(df_mean, df_sd, df_quantile))
  return(df)
}

get_hourly_average <- function(input_df, feature) {
  # Get the average for a featyre per hour
  # finds the statistics by season, period and day type
  input_df$hour <- cut(as.POSIXct(input_df$Time, format="%H:%M:%S"), breaks="hour")
  column_names = c("Season", "hour", "Weekday")
  df_mean <- setNames(aggregate(input_df[[feature]] ~ Season+hour+Weekday, input_df, FUN=mean, na.rm=TRUE), c(column_names, "mean"))
  df_sd <- setNames(aggregate(input_df[[feature]] ~ Season+hour+Weekday, input_df, FUN=sd, na.rm=TRUE), c(column_names, "sd"))
  df_quantile <- aggregate(input_df[[feature]] ~ Season+hour+Weekday, input_df, 
                           function(x) quantile(x, probs = seq(0,1, 0.25)))
  df_quantile <- lapply(df_quantile, unlist)
  df_quantile <- setNames(data.frame(df_quantile), c(column_names, 'min', '25%', '50%', '75%', 'max'))
  
  df = Reduce(function(x,y) merge(x,y, by=column_names), list(df_mean, df_sd, df_quantile))
  df$hour <- format(strptime(df$hour, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
  return(df)
}

train_hourly <- get_hourly_average(train_data, 'Global_active_power')
train_period <- get_period_average(train_data, 'Global_active_power')

test1_hourly <- get_hourly_average(test1_data, 'Global_active_power')
test1_period <- get_period_average(test1_data, 'Global_active_power')
