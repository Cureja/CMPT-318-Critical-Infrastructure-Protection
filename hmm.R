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

nrow(projectsundaymorning)
for(row in seq(from=1, to=55620, by=180)){
  
  dDate = projectsundaymorning[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  
  dLoopTrain <- depmix(list(Global_active_power~1),
                          data=subset(projectsundaymorning, Date==dDate),
                          nstates=15,
                          family=list(gaussian()))
  dLoopTrainf <- setpars(dLoopTrain, getpars(fmTrain, which="pars"), which="pars")
  dLoopTrainLog <- forwardbackward(dLoopTrainf, return.all=TRUE, useC=TRUE)
  
  checkedMax = F
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[1]) {
    trainMaxArr[1] = dLoopTrainLog$logLike
    checkedMax = T
  } else {
    checkedMax = F
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[2]) {
    trainMaxArr[2] = dLoopTrainLog$logLike
    checkedMax = T
  } else {
    checkedMax = F
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[3]) {
    trainMaxArr[3] = dLoopTrainLog$logLike
    checkedMax = T
  } else {
    checkedMax = F
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[4]) {
    trainMaxArr[4] = dLoopTrainLog$logLike
    checkedMax = T
  } else {
    checkedMax = F
  }
  
  if (checkedMax == F && dLoopTrainLog$logLike < trainMaxArr[5]) {
    trainMaxArr[5] = dLoopTrainLog$logLike
    checkedMax = T
  } else {
    checkedMax = F
  }
}

trainMaxArr

testMaxArr=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

for(row in seq(from=1, to=18360, by=180)){
  dDate = projectsundaymorningtest1[row,]$Date
  dDate = str_remove(dDate, " PST")
  dDate
  dLoopTest1 <- depmix(list(Global_active_power~1),
                       data=subset(projectsundaymorningtest1, Date==dDate),
                       nstates=15,
                       family=list(gaussian()))
  dLoopTest1f <- setpars(dLoopTest1, getpars(fmTrain, which="pars"), which="pars")
  dLoopTest1Log <- forwardbackward(dLoopTest1f, return.all=TRUE, useC=TRUE)
  
  dLoopTest1Log
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


