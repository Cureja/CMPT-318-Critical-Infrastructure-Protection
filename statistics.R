get_stats <- function(input_df) {
  # Takes 1 year of data and finds the statistics by season, period and day type
  # Work with 1 year at a time to improve speed
  column_names = c("Season", "Period", "Weekday")
  df_mean <- setNames(aggregate(Global_active_power ~ Season+Period+Weekday, input_df, FUN=mean), c(column_names, "mean"))
  df_sd <- setNames(aggregate(Global_active_power ~ Season+Period+Weekday, input_df, FUN=sd), c(column_names, "sd"))
  df_quantile <- aggregate(Global_active_power ~ Season+Period+Weekday, input_df, 
                           function(x) quantile(x, probs = seq(0,1, 0.25)))
  df_quantile <- lapply(df_quantile, unlist)
  df_quantile <- setNames(data.frame(df_quantile), c(column_names, 'min', '25%', '50%', '75%', 'max'))

  df = Reduce(function(x,y) merge(x,y, by=column_names), list(df_mean, df_sd, df_quantile))
  return(df)
}

get_hourly_average <- function(input_df) {
  # Get the average global active power per hour
  input_df$hour <- cut(as.POSIXct(input_df$Time, format="%H:%M:%S"), breaks="hour")
  df <- aggregate(Global_active_power ~ Season+Weekday+hour, input_df, FUN=mean)
  df$hour <- format(strptime(df$hour, "%Y-%m-%d %H:%M:%S"), "%H")
  return(df)
}

train_hourly <- get_hourly_average(train_data)
train_stats <- get_stats(train_data)

test1_hourly <- get_hourly_average(test1_data)
test1_stats <- get_stats(test1_data)