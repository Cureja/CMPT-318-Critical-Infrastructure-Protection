library(lubridate)
library(scales)
library(ggplot2)
install.packages("Metrics")
library(Metrics)

data <- train_data 
data$Hour <- cut(as.POSIXct(data$Time, format="%H:%M:%S"), breaks='hour')
data$Minute<- cut(as.POSIXct(data$Time, format="%H:%M:%S"), breaks='min')
feature <- 'Global_active_power'

tuesday_morning <- subset(data, Day_of_week == "Tuesday" & Period == "Morning")
seasonal_minute <- aggregate(tuesday_morning[[feature]] ~ Season+Minute, tuesday_morning, FUN=mean)
seasonal_minute$Minute<- format(strptime(seasonal_minute$Minute, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
names(seasonal_minute) <- c("Season", "Time", "Global_active_power") 

summer_tuesday <- subset(seasonal_minute, Season == "Summer")
winter_tuesday <- subset(seasonal_minute, Season == "Winter")
autumn_tuesday <- subset(seasonal_minute, Season == "Autumn")
spring_tuesday <- subset(seasonal_minute, Season == "Spring")

fit_autumn <- lm(autumn_tuesday$Global_active_power~poly(x,3, raw=TRUE))
coef_autumn <- coefficients(fit_autumn)
eqn_autumn = paste("y = ", round(coef_autumn[1], 5),
                   " + ", round(coef_autumn[2], 5), "x",
                   " + ", round(coef_autumn[3], 5), "x^2", 
                   " + ", round(coef_autumn[2], 5), "x^3", 
                   sep = "")
plot(x, autumn_tuesday$Global_active_power,
     xlab='Minute', ylab='Global Active Power') +
  lines(x, predict(fit_autumn , data.frame(x=x)), col='red') +
  text(100, 0.5, eqn_autumn, col='red') +
  title(main = "Autumn Polynomial Regression")

fit_summer <- lm(summer_tuesday$Global_active_power~poly(x,3, raw=TRUE))
coef_summer <- coefficients(fit_summer)
eqn_summer = paste("y = ", round(coef_summer[1], 5),
                   " + ", round(coef_summer[2], 5), "x",
                   " + ", round(coef_summer[3], 5), "x^2", 
                   " + ", round(coef_summer[2], 5), "x^3", 
                   sep = "")
plot(x, summer_tuesday$Global_active_power,
     xlab='Minute', ylab='Global Active Power') +
  lines(x, predict(fit_summer, data.frame(x=x)), col='blue') +
  text(100, 0.45, eqn_summer, col='blue') +
  title(main = "Summer Polynomial Regression")

fit_winter <- lm(winter_tuesday$Global_active_power~poly(x,3, raw=TRUE))
coef_winter <- coefficients(fit_winter)
eqn_winter = paste("y = ", round(coef_winter[1], 5),
                   " + ", round(coef_winter[2], 5), "x",
                   " + ", round(coef_winter[3], 5), "x^2", 
                   " + ", round(coef_winter[2], 5), "x^3", 
                   sep = "")
plot(x, winter_tuesday$Global_active_power,
     xlab='Minute', ylab='Global Active Power') +
  lines(x, predict(fit_winter, data.frame(x=x)), col='purple') +
  text(100, 0.75, eqn_winter, col='purple') +
  title(main = "Winter Polynomial Regression")


fit_spring <- lm(spring_tuesday$Global_active_power~poly(x,3, raw=TRUE))
coef_spring <- coefficients(fit_spring)
eqn_spring = paste("y = ", round(coef_spring[1], 5),
                   " + ", round(coef_spring[2], 5), "x",
                   " + ", round(coef_spring[3], 5), "x^2", 
                   " + ", round(coef_spring[2], 5), "x^3", 
                   sep = "")
plot(x, spring_tuesday$Global_active_power,
     xlab='Minute', ylab='Global Active Power') +
  lines(x, predict(fit_spring, data.frame(x=x)), col='forestgreen') +
  text(100, 0.6, eqn_spring, col='forestgreen') +
  title(main = "Spring Polynomial Regression")

eqn_autumn
eqn_spring
eqn_summer
eqn_winter

predict_spring <- predict(fit_spring, spring_tuesday)
mse_spring <- mse(spring_tuesday$Global_active_power, predict_spring)

hepredict_summer <- predict(fit_summer, summer_tuesday)
mse_summer <- mse(summer_tuesday$Global_active_power, predict_summer)

predict_winter <- predict(fit_winter, winter_tuesday)
mse_winter <- mse(winter_tuesday$Global_active_power, predict_winter)

predict_autumn <- predict(fit_autumn, autumn_tuesday)
mse_autumn <- mse(autumn_tuesday$Global_active_power, predict_autumn)

BIC(fit_autumn)
BIC(fit_summer)
BIC(fit_spring)
BIC(fit_winter)

summary(fit_autumn)
summary(fit_summer)
summary(fit_spring)
summary(fit_winter)