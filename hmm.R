library(depmixS4)
#install.packages("tidyverse")
library(tidyverse)

firstDateTest <- subset(train_data, Date=="2008-01-06")

projectsundaymorning <- subset(train_data, Weekday == 0 &
                                 strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                 strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))

projectsundaymorningtest1 <- subset(test1_data, Weekday == 0 &
                                      strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                      strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))

projectsundaymorningtest2 <- subset(test2_data, Weekday == 0 &
                                      strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                      strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))

projectsundaymorningtest3 <- subset(test3_data, Weekday == 0 & 
                                      strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                      strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))

projectsundaymorningtest4 <- subset(test4_data, Weekday == 0 & 
                                      strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                      strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))

projectsundaymorningtest5 <- subset(test5_data, Weekday == 0 & 
                                      strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                                      strptime(Time, "%H:%M:%S") < strptime("11:00", "%H:%M"))


death <- table(factor(format(projectsundaymorning$Date,"%D")))

table(factor(format(projectsundaymorningtest1$Date,"%D")))


set.seed(1)
modTrain <- depmix(list(Global_active_power~1),
                   data=projectsundaymorning,
                   nstates=15,
                   family=list(gaussian()),
                   ntimes=c(rep(180,309)))
fmTrain <- fit(modTrain)

modTest1 <- depmix(list(Global_active_power~1),
                   data=projectsundaymorningtest1,
                   nstates=15,
                   family=list(gaussian()),
                   ntimes=c(rep(180,102)))
modTest1f <- setpars(modTest1, getpars(fmTrain, which="pars"), which="pars")
ans <- forwardbackward(modTest1f, return.all=TRUE, useC=TRUE)

trainMaxArr=c(0,0,0,0,0)

trainDate <- vector()
trainLog <- vector()
trainLogSat <- vector()
trainLogSun <- vector()

isSun = T

for(row in seq(from=1, to=55620, by=180)){
  
  dDate = projectsundaymorning[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  trainDate <- c(trainDate, dDate)
  
  dLoopTrain <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorning, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTrainf <- setpars(dLoopTrain, getpars(fmTrain, which="pars"), which="pars")
  dLoopTrainLog <- forwardbackward(dLoopTrainf, return.all=TRUE, useC=TRUE)
  
  trainLog <- c(trainLog, dLoopTrainLog$logLike)
  
  if (isSun == T) {
    trainLogSun <- c(trainLogSun, dLoopTrainLog$logLike)
    isSun = F
  } else {
    trainLogSat <- c(trainLogSat, dLoopTrainLog$logLike)
    isSun = T
  }
  
  checkedMax = F
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[1]) {
    trainMaxArr[1] = dLoopTrainLog$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[2]) {
    trainMaxArr[2] = dLoopTrainLog$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[3]) {
    trainMaxArr[3] = dLoopTrainLog$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[4]) {
    trainMaxArr[4] = dLoopTrainLog$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[5]) {
    trainMaxArr[5] = dLoopTrainLog$logLike
    checkedMax = T
  }
}

trainMaxArr

length(trainDate)
length(trainLog)
min(trainDate)
max(trainDate)
min(trainLog)
max(trainLog)

trainLog

trainLogSat

length(trainLogSat)
min(trainLogSat)
max(trainLogSat)
mean(trainLogSat)

testMaxArr=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

test1Date <- vector()
test1Log <- vector()
test1LogSat <- vector()
test1LogSun <- vector()

test1DateSat <- vector()
test1DateSun <- vector()

isSun = F

for(row in seq(from=1, to=18360, by=180)){
  dDate = projectsundaymorningtest1[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  test1Date <- c(test1Date, dDate)
  
  dLoopTest1 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest1, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest1f <- setpars(dLoopTest1, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest1Log <- forwardbackward(dLoopTest1f, return.all=TRUE, useC=TRUE)
  
  dLoopTest1Log
  
  test1Log <- c(test1Log, dLoopTest1Log$logLike)
  
  if (isSun == T) {
    test1LogSun <- c(test1LogSun, dLoopTest1Log$logLike)
    test1DateSun <- c(test1DateSun, dDate)
    isSun = F
  } else {
    test1LogSat <- c(test1LogSat, dLoopTest1Log$logLike)
    test1DateSat <- c(test1DateSat, dDate)
    isSun = T
  }
  
  checkedMax = F
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[1]) {
    testMaxArr[1] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[2]) {
    testMaxArr[2] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[3]) {
    testMaxArr[3] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[4]) {
    testMaxArr[4] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[5]) {
    testMaxArr[5] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[6]) {
    testMaxArr[6] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[7]) {
    testMaxArr[7] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[8]) {
    testMaxArr[8] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[9]) {
    testMaxArr[9] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[10]) {
    testMaxArr[10] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[11]) {
    testMaxArr[11] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[12]) {
    testMaxArr[12] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[13]) {
    testMaxArr[13] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[14]) {
    testMaxArr[14] = dLoopTest1Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F && dLoopTest1Log$logLike < testMaxArr[15]) {
    testMaxArr[15] = dLoopTest1Log$logLike
    checkedMax = T
  }
}

testMaxArr

test1Log

length(test1Date)
length(test1Log)
min(test1Log)
max(test1Log)

test1Log

test1DateSat
test1LogSat

length(test1LogSat)
min(test1LogSat)
max(test1LogSat)
mean(test1LogSat)

length(test1DateSat)
min(test1DateSat)
max(test1DateSat)

quantile(test1LogSat, c(.10))
test1Anomaly <- vector()
test1AnomDate <- vector()

for (i in 1:length(test1LogSat)) {
  if (test1LogSat[i] > mean(test1LogSat)) {
    next
  }
  if (test1LogSat[i] <= quantile(test1LogSat, c(.15))) {
    test1Anomaly <- c(test1Anomaly, test1LogSat[i])
    test1AnomDate <- c(test1AnomDate, test1DateSat[i])
  }
}

test1Anomaly
test1AnomDate

plot(as.Date(test1DateSat), test1LogSat)
points( as.Date(test1AnomDate), test1Anomaly, col="red")

test2MaxArr=c(0,0,0,0,0)

for(row in seq(from=1, to=18360, by=180)){
  
  dDate = projectsundaymorningtest2[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  dLoopTest2 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest2, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest2f <- setpars(dLoopTest2, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest2Log <- forwardbackward(dLoopTest2f, return.all=TRUE, useC=TRUE)
  
  checkedMax = F
  
  if (checkedMax == F &&  dLoopTest2Log$logLike < test2MaxArr[1]) {
    test2MaxArr[1] =  dLoopTest2Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest2Log$logLike < test2MaxArr[2]) {
    test2MaxArr[2] =  dLoopTest2Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest2Log$logLike < test2MaxArr[3]) {
    test2MaxArr[3] =  dLoopTest2Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest2Log$logLike < test2MaxArr[4]) {
    test2MaxArr[4] =  dLoopTest2Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest2Log$logLike < test2MaxArr[5]) {
    test2MaxArr[5] =  dLoopTest2Log$logLike
    checkedMax = T
  }
  
  test2MaxArr
}

test2MaxArr

test3MaxArr=c(0,0,0,0,0)

for(row in seq(from=1, to=18360, by=180)){
  
  dDate = projectsundaymorningtest3[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  dLoopTest3 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest3, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest3f <- setpars(dLoopTest3, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest3Log <- forwardbackward(dLoopTest3f, return.all=TRUE, useC=TRUE)
  
  checkedMax = F
  
  if (checkedMax == F &&  dLoopTest3Log$logLike < test3MaxArr[1]) {
    test3MaxArr[1] =  dLoopTest3Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest3Log$logLike < test3MaxArr[2]) {
    test3MaxArr[2] =  dLoopTest3Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest3Log$logLike < test3MaxArr[3]) {
    test3MaxArr[3] =  dLoopTest3Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest3Log$logLike < test3MaxArr[4]) {
    test3MaxArr[4] =  dLoopTest3Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest3Log$logLike < test3MaxArr[5]) {
    test3MaxArr[5] =  dLoopTest3Log$logLike
    checkedMax = T
  }
  
  test3MaxArr
}

test3MaxArr

test4MaxArr=c(0,0,0,0,0)
test4MinArr = c(0,0,0,0,0)

for(row in seq(from=1, to=18360, by=180)){
  
  dDate = projectsundaymorningtest4[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  dLoopTest4 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest4, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest4f <- setpars(dLoopTest4, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest4Log <- forwardbackward(dLoopTest4f, return.all=TRUE, useC=TRUE)
  
  checkedMax = F
  
  if (checkedMax == F &&  dLoopTest4Log$logLike < test4MaxArr[1]) {
    test4MaxArr[1] =  dLoopTest4Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest4Log$logLike < test4MaxArr[2]) {
    test4MaxArr[2] =  dLoopTest4Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest4Log$logLike < test4MaxArr[3]) {
    test4MaxArr[3] =  dLoopTest4Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest4Log$logLike < test4MaxArr[4]) {
    test4MaxArr[4] =  dLoopTest4Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest4Log$logLike < test4MaxArr[5]) {
    test4MaxArr[5] =  dLoopTest4Log$logLike
    checkedMax = T
  }
  
  
  checkedMin = F
  
  if (checkedMin == F && test4MinArr[1] == 0) {
    test4MinArr[1] = dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && dLoopTest4Log$logLike > test4MinArr[1]) {
    test4MinArr[1] =  dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test4MinArr[2] == 0) {
    test4MinArr[2] = dLoopTest4Log$logLike
    checkedMin = T
  }
  
  test4MinArr
  
  if (checkedMin == F &&  dLoopTest4Log$logLike > test4MinArr[2]) {
    test4MinArr[2] =  dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test4MinArr[3] == 0) {
    test4MinArr[3] = dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest4Log$logLike > test4MinArr[3]) {
    test4MinArr[3] =  dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test4MinArr[4] == 0) {
    test4MinArr[4] = dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest4Log$logLike > test4MinArr[4]) {
    test4MinArr[4] =  dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test4MinArr[5] == 0) {
    test4MinArr[5] = dLoopTest4Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest4Log$logLike > test4MinArr[5]) {
    test4MinArr[5] =  dLoopTest4Log$logLike
    checkedMin = T
  }
  
  test4MaxArr
  
  
}

test4MaxArr
test4MinArr


test5MaxArr=c(0,0,0,0,0)
test5MinArr = c(0,0,0,0,0)

for(row in seq(from=1, to=18360, by=180)){
  
  dDate = projectsundaymorningtest5[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  dLoopTest5 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest5, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest5f <- setpars(dLoopTest5, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest5Log <- forwardbackward(dLoopTest5f, return.all=TRUE, useC=TRUE)
  
  checkedMax = F
  
  if (checkedMax == F &&  dLoopTest5Log$logLike < test5MaxArr[1]) {
    test5MaxArr[1] =  dLoopTest5Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest5Log$logLike < test5MaxArr[2]) {
    test5MaxArr[2] =  dLoopTest5Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest5Log$logLike < test5MaxArr[3]) {
    test5MaxArr[3] =  dLoopTest5Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest5Log$logLike < test5MaxArr[4]) {
    test5MaxArr[4] =  dLoopTest5Log$logLike
    checkedMax = T
  }
  
  if (checkedMax == F &&  dLoopTest5Log$logLike < test5MaxArr[5]) {
    test5MaxArr[5] =  dLoopTest5Log$logLike
    checkedMax = T
  }
  
  
  checkedMin = F
  
  if (checkedMin == F && test5MinArr[1] == 0) {
    test5MinArr[1] = dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && dLoopTest5Log$logLike > test5MinArr[1]) {
    test5MinArr[1] =  dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test5MinArr[2] == 0) {
    test5MinArr[2] = dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest5Log$logLike > test5MinArr[2]) {
    test5MinArr[2] =  dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test5MinArr[3] == 0) {
    test5MinArr[3] = dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest5Log$logLike > test5MinArr[3]) {
    test5MinArr[3] =  dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test5MinArr[4] == 0) {
    test5MinArr[4] = dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest5Log$logLike > test5MinArr[4]) {
    test5MinArr[4] =  dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F && test5MinArr[5] == 0) {
    test5MinArr[5] = dLoopTest5Log$logLike
    checkedMin = T
  }
  
  if (checkedMin == F &&  dLoopTest5Log$logLike > test5MinArr[5]) {
    test5MinArr[5] =  dLoopTest5Log$logLike
    checkedMin = T
  }
  
}

test5MaxArr
test5MinArr

d12 <- depmix(list(Global_active_power~1),
              data=subset(projectsundaymorningtest1, Date=="2009-12-12"),
              nstates=15,
              family=list(gaussian()))
d12f <- setpars(d12, getpars(fmTrain, which="pars"), which="pars")
d12log <- forwardbackward(d12f, return.all=TRUE, useC=TRUE)


d17train <- depmix(list(Global_active_power~1),
                   data=subset(projectsundaymorning, Date=="2006-12-17"),
                   nstates=15,
                   family=list(gaussian()))
d17trainf <- setpars(d17train, getpars(fmTrain, which="pars"), which="pars")
d17log <- forwardbackward(d17trainf, return.all=TRUE, useC=TRUE)

summary(fmTrain)
fmTrain
d17log

d12log


