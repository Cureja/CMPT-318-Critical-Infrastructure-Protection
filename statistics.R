get_season <- function(datetime) {
  # Creates a column to show which season the datapoint is from
  
  year = unique(datetime$year) + 1900 
  datetime$Season[datetime >= paste(year, '03', '21', sep='-') & datetime <  paste(year, '06', '21', sep='-')] <- "Spring"
  datetime$Season[datetime >= paste(year, '06', '21', sep='-') & datetime <  paste(year, '09', '21', sep='-')] <- "Summer"
  datetime$Season[datetime >= paste(year, '09', '21', sep='-') & datetime <  paste(year, '12', '21', sep='-')] <- "Fall"
  datetime$Season[datetime >= paste(year, '12', '21', sep='-') | datetime <  paste(year, '03', '21', sep='-')] <- "Winter"
  return(datetime$Season)
}

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

year2007 <- subset(train_data, Datetime$year == 107)
year2007$Season <- get_season(year2007$Datetime)


stats2007 <- get_stats(year2007)

