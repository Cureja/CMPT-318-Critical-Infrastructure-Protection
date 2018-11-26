get_stats_by_period <- function(input_df, feature, day) {
  # Get the mean, sd, max and min value for a feature by season and period for a certain day
  input_df <- subset(input_df, Day_of_week == day)
  column_names = c("Season", "Period")
  df_mean <- setNames(aggregate(input_df[[feature]] ~ Season+Period, input_df, FUN=mean), c(column_names, "mean"))
  df_sd <- setNames(aggregate(input_df[[feature]] ~ Season+Period, input_df, FUN=sd), c(column_names, "sd"))
  df_quantile <- aggregate(input_df[[feature]] ~ Season+Period, input_df, 
                           function(x) quantile(x, probs = seq(0,1, 0.25)))
  df_quantile <- lapply(df_quantile, unlist)
  df_quantile <- setNames(data.frame(df_quantile), c(column_names, 'min', '25%', '50%', '75%', 'max'))

  df = Reduce(function(x,y) merge(x,y, by=column_names), list(df_mean, df_sd, df_quantile))
  return(df)
}

get_stats_by_scale <- function(input_df, feature, day, scale) {
  # Get the mean, sd, max and min value for a feature by season and by time scale (min or hour) for a certain day
  input_df$Time <- cut(as.POSIXct(input_df$Time, format="%H:%M:%S"), breaks=scale)
  column_names = c("Season", "Time")
  df_mean <- setNames(aggregate(input_df[[feature]] ~ Season+Time, input_df, FUN=mean, na.rm=TRUE), c(column_names, "mean"))
  df_sd <- setNames(aggregate(input_df[[feature]] ~ Season+Time, input_df, FUN=sd, na.rm=TRUE), c(column_names, "sd"))
  df_quantile <- aggregate(input_df[[feature]] ~ Season+Time, input_df, 
                           function(x) quantile(x, probs = seq(0,1, 0.25)))
  df_quantile <- lapply(df_quantile, unlist)
  df_quantile <- setNames(data.frame(df_quantile), c(column_names, 'min', '25%', '50%', '75%', 'max'))
  
  df = Reduce(function(x,y) merge(x,y, by=column_names), list(df_mean, df_sd, df_quantile))
  df$Time <- format(strptime(df$Time, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
  return(df)
}

train_hourly <- get_average_by_scale(train_data, 'Global_active_power', 'hour')
train_minutely <- get_stats_by_scale(train, 'Global_active_power', 'Tuesday', 'min')

###########################################################################
# Calculate Monthly Mean and SD of train and test
###########################################################################

train <- subset(train_data, Day_of_week == "Tuesday" & Period == "Morning")
t1 <- subset(test1_data, Day_of_week == "Tuesday" & Period == "Morning")
t2 <- subset(test2_data, Day_of_week == "Tuesday" & Period == "Morning")
t3 <- subset(test3_data, Day_of_week == "Tuesday" & Period == "Morning")
t4 <- subset(test4_data, Day_of_week == "Tuesday" & Period == "Morning")
t5 <- subset(test5_data, Day_of_week == "Tuesday" & Period == "Morning")

train_month_mean <- aggregate(Global_active_power ~ Month, train, FUN=mean)
t1_month_mean <- aggregate(Global_active_power ~ Month, t1, FUN=mean)
t2_month_mean <- aggregate(Global_active_power ~ Month, t2, FUN=mean)
t3_month_mean <- aggregate(Global_active_power ~ Month, t3, FUN=mean)
t4_month_mean <- aggregate(Global_active_power ~ Month, t4, FUN=mean)
t5_month_mean <- aggregate(Global_active_power ~ Month, t5, FUN=mean)

monthly_means <- train_month_mean
names(monthly_means) <- c("Month", "Train") 
monthly_means$Test1 <- t1_month_mean$Global_active_power
monthly_means$Test2 <- t2_month_mean$Global_active_power
monthly_means$Test3 <- t3_month_mean$Global_active_power
monthly_means$Test4 <- t4_month_mean$Global_active_power
monthly_means$Test5 <- t5_month_mean$Global_active_power

train_month_sd <- aggregate(Global_active_power ~ Month, train, FUN=sd)
t1_month_sd <- aggregate(Global_active_power ~ Month, t1, FUN=sd)
t2_month_sd <- aggregate(Global_active_power ~ Month, t2, FUN=sd)
t3_month_sd <- aggregate(Global_active_power ~ Month, t3, FUN=sd)
t4_month_sd <- aggregate(Global_active_power ~ Month, t4, FUN=sd)
t5_month_sd <- aggregate(Global_active_power ~ Month, t5, FUN=sd)

monthly_sds <- train_month_sd
names(monthly_sds) <- c("Month", "Train") 
monthly_sds$Test1 <- t1_month_sd$Global_active_power
monthly_sds$Test2 <- t2_month_sd$Global_active_power
monthly_sds$Test3 <- t3_month_sd$Global_active_power
monthly_sds$Test4 <- t4_month_sd$Global_active_power
monthly_sds$Test5 <- t5_month_sd$Global_active_power

month_order <- c("January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December")
monthly_means <- monthly_means[match(month_order, monthly_means$Month),]
monthly_sds   <- monthly_sds[match(month_order, monthly_sds$Month),]
rm(train_month_mean, t1_month_mean, t2_month_mean, 
   t3_month_mean, t4_month_mean, t5_month_mean)
rm(train_month_sd, t1_month_sd, t2_month_sd, 
   t3_month_sd, t4_month_sd, t5_month_sd)

write.csv(monthly_means, file = "monthly_means.csv")
write.csv(monthly_sds,   file = "monthly_sds.csv")

###########################################################################
# Calculate Monthly Mean and SD of train and test
###########################################################################

tuesday_stats_train <- get_stats_by_period(train_data, 'Global_active_power', "Tuesday")
tuesday_stats_test1 <- get_stats_by_period(test1_data, 'Global_active_power', "Tuesday")
tuesday_stats_test2 <- get_stats_by_period(test2_data, 'Global_active_power', "Tuesday")
tuesday_stats_test3 <- get_stats_by_period(test3_data, 'Global_active_power', "Tuesday")
tuesday_stats_test4 <- get_stats_by_period(test4_data, 'Global_active_power', "Tuesday")
tuesday_stats_test5 <- get_stats_by_period(test5_data, 'Global_active_power', "Tuesday")

# Collect all the mean
seasonal_means       <- tuesday_stats_train[tuesday_stats_train$Period == "Morning", c("Season", "mean")]
names(seasonal_means)<- c("Season", "Train") 
seasonal_means$Test1 <- tuesday_stats_test1[tuesday_stats_test1$Period == "Morning", "mean"]
seasonal_means$Test2 <- tuesday_stats_test2[tuesday_stats_test2$Period == "Morning", "mean"]
seasonal_means$Test3 <- tuesday_stats_test3[tuesday_stats_test3$Period == "Morning", "mean"]
seasonal_means$Test4 <- tuesday_stats_test4[tuesday_stats_test4$Period == "Morning", "mean"]
seasonal_means$Test5 <- tuesday_stats_test5[tuesday_stats_test1$Period == "Morning", "mean"]

# Collect all the SD
seasonal_sds <- tuesday_stats_train[tuesday_stats_train$Period == "Morning", c("Season", "sd")]
names(seasonal_sds) <- c("Season", "Train") 
seasonal_sds$Test1 <- tuesday_stats_test1[tuesday_stats_test1$Period == "Morning", "sd"]
seasonal_sds$Test2 <- tuesday_stats_test2[tuesday_stats_test2$Period == "Morning", "sd"]
seasonal_sds$Test3 <- tuesday_stats_test3[tuesday_stats_test3$Period == "Morning", "sd"]
seasonal_sds$Test4 <- tuesday_stats_test4[tuesday_stats_test4$Period == "Morning", "sd"]
seasonal_sds$Test5 <- tuesday_stats_test5[tuesday_stats_test1$Period == "Morning", "sd"]

write.csv(seasonal_means, file = "seasonal_means.csv")
write.csv(seasonal_sds,   file = "seasonal_sds.csv")



