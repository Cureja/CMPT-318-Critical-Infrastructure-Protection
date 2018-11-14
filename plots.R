library(ggplot2)

plot_hourly <- function(input_df, weekday) {
  seasons = c('Spring', 'Summer', 'Autumn', 'Winter')
  plot <- ggplot()
  for (season in seasons) {
    season_df = subset(input_df, Season == season & Weekday == weekday )
    plot <- plot + layer(data = season_df,
                         mapping = aes(x=hour, y=Global_active_power, color = Season), 
                         geom = 'point',
                         stat = "identity", 
                         position = position_identity()
    ) + geom_line(data = season_df,
                  aes(hour, 
                      Global_active_power, 
                      group = 1,
                      color = Season))
  }
  plot <- plot + labs(x= "Hour", y='Average Global Active Power')
  title <- "Average Hourly Global Active Power across each Season during the"
  
  if (weekday) {
    title <- paste(title, "Weekdays")
  } else {
    title <- paste(title, "Weekends")
  }
  
  plot <- plot + ggtitle(title)
  
  plot <- plot + geom_rect(
    data=data.frame(xmin = 6, 
                    xmax = 10, 
                    ymin = -Inf, 
                    ymax = Inf),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    fill='red', alpha=0.05
  )
  
  plot <- plot + geom_rect(
    data=data.frame(xmin = 18, 
                    xmax = 22, 
                    ymin = -Inf, 
                    ymax = Inf),
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    fill='red', alpha=0.05
  )
  return(plot)
}

train_weekday_plot <- plot_hourly(train_hourly, TRUE)
train_weekend_plot <- plot_hourly(train_hourly, FALSE)

test1_weekday_plot <- plot_hourly(test1_hourly, TRUE)
test1_weekend_plot <- plot_hourly(test1_hourly, FALSE)
