library(lubridate)
library(scales)
library(ggplot2)

data <- train_data 
seasonal_minute <- aggregate(tuesday_morning[[feature]] ~ Season+Minute, tuesday_morning, FUN=mean)
seasonal_minute$Minute<- format(strptime(seasonal_minute$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
names(seasonal_minute) <- c("Season", "Time", "Global_active_power") 
data$Hour <- cut(as.POSIXct(data$Time, format="%H:%M:%S"), breaks='hour')
data$Minute<- cut(as.POSIXct(data$Time, format="%H:%M:%S"), breaks='min')
feature <- 'Global_active_power'

###########################################################################
# Plot average by hour of day of the week
###########################################################################

mean_by_day_hour <- aggregate(data[[feature]] ~ Day_of_week+Hour, data, FUN=mean)
names(mean_by_day_hour) <- c('Day_of_week', 'Hour', 'Global_active_power')
mean_by_day_hour$Hour <- format(strptime(mean_by_day_hour$Hour, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# Reorder the plot, normally plots them alphabetically
day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday")

# peak usage hours +1 hour 
peaks <- c(7, 10, 19, 22)

Day_of_week_plot <- ggplot(mean_by_day_hour, 
                           aes(x=Hour, y=Global_active_power, 
                               group = Day_of_week,
                               color = Day_of_week)) + 
  geom_line() +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3)]) +
  labs(x="Time", y=paste('Average Global Active Power')) +
  guides(color=guide_legend("Day of the Week")) +
  scale_color_discrete(breaks=day_order) +
  geom_rect(data=data.frame(xmin = peaks[1], 
                            xmax = peaks[2],
                            ymin = -Inf,
                            ymax = Inf),
            aes(xmin=xmin, xmax=xmax, 
                ymin=ymin, ymax=ymax,
                fill='Peak Usage'),
            color=NA, alpha=0.25, 
            inherit.aes = FALSE) +
  geom_rect(data=data.frame(xmin = peaks[3], 
                            xmax = peaks[4],
                            ymin = -Inf,
                            ymax = Inf),
            aes(xmin=xmin, xmax=xmax, 
                ymin=ymin, ymax=ymax,
                fill='Peak Usage'),
            color=NA, alpha=0.25, 
            inherit.aes = FALSE) +
  scale_fill_manual('',
                    values = 'pink',  
                    guide = guide_legend(override.aes = list(alpha = 1))) 

Day_of_week_plot

###########################################################################
# Plot average by minute of each month for Tuesday morning
###########################################################################
tuesday_morning <- subset(data, Day_of_week == "Tuesday" & Period == "Morning")
mean_by_month_minute <- aggregate(tuesday_morning[[feature]] ~ Month+Minute, tuesday_morning, FUN=mean)

names(mean_by_month_minute) <- c('Month', 'Minute', 'Global_active_power')
month_order <- c("January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December")


mean_by_month_minute$Minute<- format(strptime(mean_by_month_minute$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

month_plot <- ggplot(mean_by_month_minute, 
                     aes(x=Minute, y=Global_active_power, 
                         group = Month,
                         color = Month)) + 
  geom_line() +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 60)]) +
  labs(x="Time", y=paste('Average Global Active Power')) +
  scale_color_discrete(breaks=month_order)

month_plot

###########################################################################
# Plot average by minute of season for Tuesday morning
###########################################################################

mean_by_season_minute <- aggregate(tuesday_morning[[feature]] ~ Season+Minute, tuesday_morning, FUN=mean)
names(mean_by_season_minute) <- c('Season', 'Minute', 'Global_active_power')
mean_by_season_minute$Minute<- format(strptime(mean_by_season_minute$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

season_plot <- ggplot(mean_by_season_minute, 
                      aes(x=Minute, y=Global_active_power, 
                          group = Season,
                          color = Season)) + 
  geom_line() +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 60)]) +
  labs(x="Time", y=paste('Average Global Active Power'))

season_plot

###########################################################################
# Experiments
###########################################################################

mean_by_minute <- aggregate(tuesday_morning[[feature]] ~ Minute, tuesday_morning, FUN=mean)
mean_by_minute$Minute<- format(strptime(mean_by_minute$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
names(mean_by_minute) <- c("Time", "Global_active_power") 

mean_by_minute$Counts <- seq(1,180)
tuesday_plot <- ggplot() + 
  layer(data=mean_by_minute,
        mapping = aes(x=Time, y=Global_active_power))

tuesday_plot <- ggplot() + layer(data = mean_by_minute,
                              mapping = aes(x=Time, y=Global_active_power), 
                              geom = 'point',
                              stat = "identity", 
                              position = position_identity()) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 60)]) +
  stat_smooth(data=mean_by_minute, method="lm", se=TRUE, fill=NA,
                formula=Global_active_power ~ poly(Counts, 3, raw=TRUE), colour="red")

tuesday_plot <- ggplot(mean_by_minute, aes(Counts, Global_active_power)) +
  geom_point() + geom_smooth(method="loess",se=TRUE)

t1data <- subset(test1_data, Day_of_week == "Tuesday" & Period == "Morning")
t1data$Minute<- cut(as.POSIXct(t1data $Time, format="%H:%M:%S"), breaks='min')

t1data <- aggregate(t1data[[feature]] ~ Minute, t1data, FUN=mean)
t1data$Minute<- format(strptime(t1data$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
names(t1data) <- c("Time", "Global_active_power") 

t1data$Counts <- seq(1,180)

fit1 <- lm(Global_active_power ~ poly(Counts, 3, raw = TRUE), data=mean_by_minute)

lmMod <- lm(Global_active_power ~ Counts, data=mean_by_minute)
prediction <- predict(lmMod, t1data)
summary(lmMod)

actuals_preds <- data.frame(cbind(actuals=t1data$Global_active_power, 
                                  predicteds=prediction))
coeff <- lmMod$coefficients
tuesday_plot + lines(mean_by_minute$Counts, predict(fit1, mean_by_minute$Counts))



###########################################################################