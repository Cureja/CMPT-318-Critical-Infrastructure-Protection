library(depmixS4)
#install.packages("tidyverse")
library(tidyverse)

#tuesday morning individual anomaly detection
tuesMornTest1Date <- vector()
tuesMornTest1LogLike <- vector()

for (row in seq(from = 1, to = 12240, by = 240)) {
  #Strip 'Date' from the row of tuesMorningTest1
  indivDate = tuesMorningTest1[row,]$Date
  indivDate = str_remove(indivDate, " PST")
  
  indivDate
  #add date into vector tuesMornTest1Date to keep track of which index/row associated data will be at
  tuesMornTest1Date <- c(tuesMornTest1Date, indivDate)
  #find the indivial model, fitted model and log likelihood of Global active power at specific individual date
  indivLoop <- depmix(list(Global_active_power~1),
                      data = subset(tuesMorningTest1, Date == indivDate),
                      nstates = 9,
                      family = list(gaussian()))
  indivLoopF <- setpars(indivLoop, getpars(tuesMorningModelF, which="pars"), which="pars")
  indivLoopFLog <- forwardbackward(indivLoopF, return.all = TRUE, useC = TRUE)
  #store the individual log likelihood of this loop iteration within tuesMornTest1LogLike vector
  tuesMornTest1LogLike <- c(tuesMornTest1LogLike, indivLoopFLog$logLike)
}

#vectors to store anomal date and log likelihood value
tuesMornTest1AnomalDate <- vector()
tuesMornTest1AnomalLogLike <- vector()

#sort through data to mark as normal or anomal depending on anomaly threshold
for (i in 1:length(tuesMornTest1LogLike)) {
  #If the log likelihood value is smaller than the average log likelihood value, ignore
  if (tuesMornTest1LogLike[i] > mean(tuesMornTest1LogLike)) {
    next
  }
  #If the log likelihood value is in the top nth percentile, count as anomaly
  if (tuesMornTest1LogLike[i] <= quantile(tuesMornTest1LogLike, c(.20))) {
    tuesMornTest1AnomalDate <- c(tuesMornTest1AnomalDate, tuesMornTest1Date[i])
    tuesMornTest1AnomalLogLike <- c(tuesMornTest1AnomalLogLike, tuesMornTest1LogLike[i])
  }
}

#plot data, with anomalies marked in red
plot(as.Date(tuesMornTest1Date), tuesMornTest1LogLike)
points(as.Date(tuesMornTest1AnomalDate), tuesMornTest1AnomalLogLike, col = "red")

min(tuesMornTest1Date)
max(tuesMornTest1Date)

mean(tuesMornTest1LogLike)
min(tuesMornTest1LogLike)
max(tuesMornTest1LogLike)

mean(tuesMornTest1AnomalLogLike)
min(tuesMornTest1AnomalLogLike)
max(tuesMornTest1AnomalLogLike)




#saturday night individual anomaly detection
satNightTest1Date <- vector()
satNightTest1LogLike <- vector()

for (row in seq(from = 1, to = 12240, by = 240)) {
  #strip 'Date' from the row of satNightTest1
  indivDate = satNightTest1[row,]$Date
  indivDate = str_remove(indivDate, " PST")
  
  indivDate
  #add date into vector satNightTest1Date to keep track of which index/row associated data will be at
  satNightTest1Date <- c(satNightTest1Date, indivDate)
  #find the indivial model, fitted model and log likelihood of Global active power at specific individual date
  indivLoop <- depmix(list(Global_active_power~1),
                      data = subset(satNightTest1, Date == indivDate),
                      nstates = 18,
                      family = list(gaussian()))
  indivLoopF <- setpars(indivLoop, getpars(satNightModelF, which="pars"), which="pars")
  indivLoopFLog <- forwardbackward(indivLoopF, return.all = TRUE, useC = TRUE)
  #store the individual log likelihood of this loop iteration within satNightTest1LogLike vector
  satNightTest1LogLike <- c(satNightTest1LogLike, indivLoopFLog$logLike)
}

#vectors to store anomal date and log likelihood value
satNightTest1AnomalDate <- vector()
satNightTest1AnomalLogLike <- vector()

#sort through data to mark as normal or anomal depending on anomaly threshold
for (i in 1:length(satNightTest1LogLike)) {
  #If the log likelihood value is smaller than the average log likelihood value, ignore
  if (satNightTest1LogLike[i] > mean(satNightTest1LogLike)) {
    next
  }
  #If the log likelihood value is in the top nth percentile, count as anomaly
  if (satNightTest1LogLike[i] <= quantile(satNightTest1LogLike, c(.15))) {
    satNightTest1AnomalDate <- c(satNightTest1AnomalDate, satNightTest1Date[i])
    satNightTest1AnomalLogLike <- c(satNightTest1AnomalLogLike, satNightTest1LogLike[i])
  }
}

#plot data, with anomalies marked in red
plot(as.Date(satNightTest1Date), satNightTest1LogLike)
points(as.Date(satNightTest1AnomalDate), satNightTest1AnomalLogLike, col = "red")

mean(satNightTest1LogLike)

min(satNightTest1Date)
max(satNightTest1Date)

mean(satNightTest1LogLike)
min(satNightTest1LogLike)
max(satNightTest1LogLike)

mean(satNightTest1AnomalLogLike)
min(satNightTest1AnomalLogLike)
max(satNightTest1AnomalLogLike)



#saturday night multivariate individual anomaly detection
satNightMultiTest1Date <- vector()
satNightMultiTest1LogLike <- vector()

for (row in seq(from = 1, to = 12240, by = 240)) {
  #strip 'Date' from the row of satNightTest1
  indivDate = satNightTest1[row,]$Date
  indivDate = str_remove(indivDate, " PST")
  
  indivDate
  #add date into vector satNightMultiTest1Date to keep track of which index/row associated data will be at
  satNightMultiTest1Date <- c(satNightMultiTest1Date, indivDate)
  #find the indivial model, fitted model and log likelihood of Global active power and Global reactive power at specific individual date
  indivLoop <- depmix(list(Global_active_power~1, Global_reactive_power~1),
                      data = subset(satNightTest1, Date == indivDate),
                      nstates = 4,
                      family = list(gaussian(), gaussian()))
  indivLoopF <- setpars(indivLoop, getpars(satNightModelMultinomialF, which="pars"), which="pars")
  indivLoopFLog <- forwardbackward(indivLoopF, return.all = TRUE, useC = TRUE)
  #store the individual log likelihood of this loop iteration within satNightMultiTest1LogLike vector
  satNightMultiTest1LogLike <- c(satNightMultiTest1LogLike, indivLoopFLog$logLike)
}

#vectors to store anomal date and log likelihood value
satNightMultiTest1AnomalDate <- vector()
satNightMultiTest1AnomalLogLike <- vector()

#sort through data to mark as normal or anomal depending on anomaly threshold
for (i in 1:length(satNightMultiTest1LogLike)) {
  #If the log likelihood value is smaller than the average log likelihood value, ignore
  if (satNightMultiTest1LogLike[i] > mean(satNightMultiTest1LogLike)) {
    next
  }
  #If the log likelihood value is in the top nth percentile, count as anomaly
  if (satNightMultiTest1LogLike[i] <= quantile(satNightMultiTest1LogLike, c(.15))) {
    satNightMultiTest1AnomalDate <- c(satNightMultiTest1AnomalDate, satNightMultiTest1Date[i])
    satNightMultiTest1AnomalLogLike <- c(satNightMultiTest1AnomalLogLike, satNightMultiTest1LogLike[i])
  }
}

#plot data, with anomalies marked in red
plot(as.Date(satNightMultiTest1Date), satNightMultiTest1LogLike)
points(as.Date(satNightMultiTest1AnomalDate), satNightMultiTest1AnomalLogLike, col = "red")

mean(satNightMultiTest1LogLike)

min(satNightTest1Date)
max(satNightTest1Date)

mean(satNightMultiTest1LogLike)
min(satNightMultiTest1LogLike)
max(satNightMultiTest1LogLike)

mean(satNightMultiTest1AnomalLogLike)
min(satNightMultiTest1AnomalLogLike)
max(satNightMultiTest1AnomalLogLike)

