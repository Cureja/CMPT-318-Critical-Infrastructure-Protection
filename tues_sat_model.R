library(depmixS4)
#install.packages("tidyverse")
library(tidyverse)

tuesMorning <- subset(train_data, Datetime$wday == 2 &
                                 strptime(Time, "%H:%M:%S") >= strptime("05:00", "%H:%M") & 
                                 strptime(Time, "%H:%M:%S") < strptime("9:00", "%H:%M"))
tuesMorningTest1 <- subset(test1_data, Datetime$wday == 2 &
                        strptime(Time, "%H:%M:%S") >= strptime("5:00", "%H:%M") & 
                        strptime(Time, "%H:%M:%S") < strptime("9:00", "%H:%M"))
tuesMorningTest4 <- subset(test4_data, Datetime$wday == 2 &
                             strptime(Time, "%H:%M:%S") >= strptime("5:00", "%H:%M") & 
                             strptime(Time, "%H:%M:%S") < strptime("9:00", "%H:%M"))

set.seed(1)
tuesMorningModel <- depmix(list(Global_active_power~1),
                           data=tuesMorning,
                           nstates=9,
                           family=list(gaussian()),
                           ntimes=c(rep(240,155)))
tuesMorningModelF <- fit(tuesMorningModel)
tuesMorningModelF

set.seed(1)
tuesMorningModelTest1 <- depmix(list(Global_active_power~1),
                           data=tuesMorningTest1,
                           nstates=9,
                           family=list(gaussian()),
                           ntimes=c(rep(240,51)))
tuesMorningModelTest1F <- setpars(tuesMorningModelTest1, getpars(tuesMorningModelF))
ans <- forwardbackward(tuesMorningModelTest1F, return.all=TRUE, useC=TRUE)
ans  

set.seed(1)
tuesMorningModelTest4 <- depmix(list(Global_active_power~1),
                                data=tuesMorningTest4,
                                nstates=9,
                                family=list(gaussian()),
                                ntimes=c(rep(240, 51)))
tuesMorningModelTest4F <- setpars(tuesMorningModelTest4, getpars(tuesMorningModelF))
ans4 <- forwardbackward(tuesMorningModelTest4F, return.all=TRUE, useC = TRUE)
ans4
                        
satNight <- subset(train_data, Datetime$wday == 6 &
                        strptime(Time, "%H:%M:%S") >= strptime("17:00", "%H:%M") & 
                        strptime(Time, "%H:%M:%S") < strptime("21:00", "%H:%M"))
satNightTest1 <- subset(test1_data, Datetime$wday == 6 &
                             strptime(Time, "%H:%M:%S") >= strptime("17:00", "%H:%M") & 
                             strptime(Time, "%H:%M:%S") < strptime("21:00", "%H:%M"))
table(factor(format(satNight$Date,"%D")))

set.seed(1)
satNightModel <- depmix(list(Global_active_power~1),
                           data=satNight,
                           nstates=18,
                           family=list(gaussian()),
                           ntimes=c(rep(240,147),216,rep(240,7)))
satNightModelF <- fit(satNightModel)
satNightModelF

set.seed(1)
satNightModelTest1 <- depmix(list(Global_active_power~1),
                                data=satNightTest1,
                                nstates=18,
                                family=list(gaussian()),
                                ntimes=c(rep(240,51)))
satNightModelTest1F <- setpars(satNightModelTest1, getpars(satNightModelF))
ans <- forwardbackward(satNightModelTest1F, return.all=TRUE, useC=TRUE)
ans  


set.seed(2)
satNightModelMultinomial <- depmix(list(Global_active_power~1,Global_reactive_power~1),
                        data=satNight,
                        nstates=4,
                        family=list(gaussian(),gaussian()),
                        ntimes=c(rep(240,147),216,rep(240,7)))
satNightModelMultinomialF <- fit(satNightModelMultinomial)
satNightModelMultinomialF

set.seed(2)
satNightModelMultinomialTest1 <- depmix(list(Global_active_power~1,Global_reactive_power~1),
                                        data=satNightTest1,
                                        nstates=4,
                                        family=list(gaussian(),gaussian()),
                                        ntimes=c(rep(240,51)))
satNightModelMultinomialTest1F <- setpars(satNightModelMultinomialTest1, getpars(satNightModelMultinomialF))
ans <- forwardbackward(satNightModelTest1F, return.all=TRUE, useC=TRUE)
ans
