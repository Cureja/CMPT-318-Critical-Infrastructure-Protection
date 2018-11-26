library(ggplot2)
library(lubridate)

plot_average <- function(input_df, weekday, feature, scale) {
  # input_df$Time <- as.POSIXct(input_df$Time, format="%H:%M:%S")
  # input_df$hour <- min(strptime(input_df$hour, format = "%H:%M:%S"))
  seasons = c('Spring', 'Summer', 'Autumn', 'Winter')
  plot <- ggplot()
  for (season in seasons) {
    season_df = subset(input_df, Season == season & Weekday == weekday )
    plot <- plot + layer(data = season_df,
                         mapping = aes(x=Time, y=mean, color = Season), 
                         geom = 'point',
                         stat = "identity", 
                         position = position_identity()
    ) + geom_line(data = season_df,
                  aes(Time, 
                      mean, 
                      group = 1,
                      color = Season))
  }
  plot <- plot + labs(x="Time", y=paste('Average', feature))
  title <- paste("Average", feature, "across each Season during the")
  
  if (weekday) {
    title <- paste(title, "Weekdays")
  } else {
    title <- paste(title, "Weekends")
  }
  
  peaks <- c(7, 10, 19, 22)
  xscale = 2
  if (scale == "min") {
    peaks <- peaks*60
    xscale <- xscale*60
  }
  
  plot <- plot + ggtitle(title) + 
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = xscale)])
  

  if (TRUE) {
  plot <- plot + geom_rect(
    data=data.frame(xmin = peaks[1], 
                    xmax = peaks[2], 
                    ymin = -Inf, 
                    ymax = Inf),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    fill='red', alpha=0.05
  )
  
  plot <- plot + geom_rect(
    data=data.frame(xmin = peaks[3], 
                    xmax = peaks[4], 
                    ymin = -Inf, 
                    ymax = Inf),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    fill='red', alpha=0.05
  )
  return(plot)
  }
}

train_weekday_plot <- plot_average(train_hourly, TRUE, 'Global Active Power', 'hour')
train_weekend_plot <- plot_hourly(train_hourly, FALSE, 'Global Active Power')

#test1_weekday_plot <- plot_hourly(test1_hourly, TRUE, 'Global Active Power')
#test1_weekend_plot <- plot_hourly(test1_hourly, FALSE, 'Global Active Power')