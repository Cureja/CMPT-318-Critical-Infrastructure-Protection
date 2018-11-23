dir = "C:/Users/10yen/Desktop/318/R/Project/CMPT-318-Critical-Infrastructure-Protection/past_assignments"
setwd(dir)

#install.packages("chron")
#install.packages("depmixS4")
#install.packages("ggplot2")
#install.packages("HMM")
library(chron)
library(depmixS4)
library(ggplot2)
library(HMM)

set.seed(1) 
data = read.table("../../../Assignment2/Dataset1.txt", 
                  header=TRUE, sep=',')
# Convert to datetime format
data$Date <- as.POSIXlt(data$Date, format="%d/%m/%Y")

# Get day of the week, day of the year and month
data$wday <- data$Date$wday
data$yday <- data$Date$yday
data$month <- data$Date$mon

# Get Sunday Morning and Sunday Night datapoints
sundaymorning <- subset(data, wday == 0 &
                        strptime(Time, "%H:%M:%S") >= strptime("08:00", "%H:%M") & 
                        strptime(Time, "%H:%M:%S") <= strptime("11:00", "%H:%M"))
sundaynight <- subset(data, wday== 0 & 
                        strptime(Time, "%H:%M:%S") >= strptime("21:00", "%H:%M") & 
                        strptime(Time, "%H:%M:%S") <= strptime("24:00", "%H:%M"))

# Average per minute feature 3 (global active power )
minute_average_morning = aggregate(sundaymorning[, 3], list(sundaymorning$Time), mean)
minute_average_night = aggregate(sundaynight[, 3], list(sundaynight$Time), mean)

#Data for sundays, separated by seasons
springmorning <- subset(sundaymorning, Date >= "2007-03-21" & Date < "2007-06-21")
summermorning <- subset(sundaymorning, Date >= "2007-06-21" & Date < "2007-09-21")
autumnmorning <- subset(sundaymorning, Date >= "2007-09-21" & Date < "2007-12-21")
wintermorning <- subset(sundaymorning, Date >= "2007-12-21" | Date < "2007-03-21")

springnight <- subset(sundaynight, Date >= "2007-03-21" & Date < "2007-06-21")
summernight <- subset(sundaynight, Date >= "2007-06-21" & Date < "2007-09-21")
autumnnight <- subset(sundaynight, Date >= "2007-09-21" & Date < "2007-12-21")
winternight <- subset(sundaynight, Date >= "2007-12-21" | Date < "2007-03-21")

set.seed(1) 
mod <- depmix(list(Global_active_power~1), 
              data=sundaymorning, 
              nstates=15, 
              family=list(gaussian()),
              ntimes=c(rep(181, 52))
              )
fm <- fit(mod)
summary(fm)
print(fm)

set.seed(1) 
mod2 <- depmix(list(Global_active_power~1), 
              data=sundaynight, 
              nstates=14, 
              family=list(gaussian()),
              ntimes=c(rep(180, 52))
)
fm2 <- fit(mod2)
summary(fm2)
print(fm2)

save(fm2, file = "fm_model.rda")

getmodel(fm2,which="prior",state=1,number=1)

fmPrior <- getmodel(fm2,which="prior",state=1,number=1)@parameters$coefficients

arr <- matrix(nrow=14,ncol=14,byrow=TRUE)
for(n in 1:14){
  arr[n,] <- (fm2)@transition[[n]]@parameters$coefficients
}

respFirstParam <- (fm2)@response[[1]][[1]]@parameters$coefficients
respFirstParam

arrRespCoef <- matrix(nrow=14,ncol=1,byrow=TRUE)
arrRespSd <- matrix(nrow=14,ncol=1,byrow=TRUE)
for(n in 1:14){
  arrRespCoef[n,] <- (fm2)@response[[n]][[1]]@parameters$coefficients
  arrRespSd[n,] <- (fm2)@response[[n]][[1]]@parameters$sd
}

arrResp <- cbind(arrRespCoef, arrRespSd)

aCheck <- depmix(list(Global_active_power~1),
                 data=sundaymorning,
                 nstates=14,
                 respstart=arrResp,
                 instart=fmPrior,
                 trstart=arr,
                 family=list(gaussian()),
                 ntimes=c(rep(181, 52)))

summary(aCheck)
print(aCheck)

fmPrior

arrACheck <- matrix(nrow=14,ncol=14,byrow=TRUE)
for(n in 1:14){
  arrACheck[n,] <- (aCheck)@transition[[n]]@parameters$coefficients
}

(aCheck)@response[[1]][[1]]@parameters$coefficients

arrRespACheck <- matrix(nrow=14,ncol=2,byrow=TRUE)
for(n in 1:14){
  arrRespACheck[n,1] <- (aCheck)@response[[n]][[1]]@parameters$coefficients
  arrRespACheck[n,2] <- (aCheck)@response[[n]][[1]]@parameters$sd
}

arrRespACheck

#hmm = initHMM(States=c(1:14),
#              Symbols=c(1:14),
#              transProbs=arrACheck,
#              emissionProbs=arrRespACheck)
#print(hmm)

load("fm_model.rda")
newthing <- fit(fm2, newdata = aCheck)
summary(aCheck)
print(aCheck)
print(fm2)
summary(newthing)
print(newthing)

# Convert time to timeformat 
minute_average_morning$Time <- as.POSIXct(minute_average_morning$Group.1, 
                                          format="%H:%M:%S")

minute_average_night$Time <- as.POSIXct(minute_average_night$Group.1, 
                                        format="%H:%M:%S")

# Plot average per minute
morning_plot <- ggplot() +
                  layer(data = minute_average_morning, 
                  mapping = aes(x=Time, y=x), 
                  geom = "point",
                  stat = "identity", 
                  position = position_identity()
                  )
morning_plot <- morning_plot + labs(x = "Time", y = 'Average Global Active Power')
morning_plot <- morning_plot + ggtitle("Average Global Active Power during Sunday Mornings")

night_plot <- ggplot()+
        layer(data = minute_average_night,
        mapping = aes(x=Time, y=x), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
night_plot <- night_plot + labs(x = "Time", y = 'Average Global Active Power')
night_plot <- night_plot + ggtitle("Average Global Active Power during Sunday Nights")

#Plot of the whole year's morning data, colour-coded by season
morning_year_plot <-  ggplot() +
        layer(data = springmorning,
        mapping = aes(x=as.POSIXct(springmorning$Date), y=springmorning$Global_active_power, color = "Spring"), 
        geom = "point",
        stat = "identity", 
        position = position_identity()) 
morning_year_plot <- morning_year_plot + layer(data = summermorning,
        mapping = aes(x=as.POSIXct(summermorning$Date), y=summermorning$Global_active_power, color = "Summer"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
morning_year_plot <- morning_year_plot + layer(data = autumnmorning,
        mapping = aes(x=as.POSIXct(autumnmorning$Date), y=autumnmorning$Global_active_power, color = "Autumn"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
morning_year_plot <- morning_year_plot + layer(data = wintermorning,
        mapping = aes(x=as.POSIXct(wintermorning$Date), y=wintermorning$Global_active_power, color = "Winter"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
morning_year_plot <- morning_year_plot + labs(x = "Date", y = 'Global Active Power')
morning_year_plot <- morning_year_plot + ggtitle("Global Active Power during Sunday Mornings")

#Plot of the whole year's morning data, colour-coded by season
night_year_plot <-  ggplot() +
  layer(data = springnight,
        mapping = aes(x=as.POSIXct(springnight$Date), y=springnight$Global_active_power, color = "Spring"), 
        geom = "point",
        stat = "identity", 
        position = position_identity()) 
night_year_plot <- night_year_plot + layer(data = summernight,
        mapping = aes(x=as.POSIXct(summernight$Date), y=summernight$Global_active_power, color = "Summer"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
night_year_plot <- night_year_plot + layer(data = autumnnight,
        mapping = aes(x=as.POSIXct(autumnnight$Date), y=autumnnight$Global_active_power, color = "Autumn"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
night_year_plot <- night_year_plot + layer(data = winternight,
        mapping = aes(x=as.POSIXct(winternight$Date), y=winternight$Global_active_power, color = "Winter"), 
        geom = "point",
        stat = "identity", 
        position = position_identity())
night_year_plot <- night_year_plot + labs(x = "Date", y = 'Global Active Power')
night_year_plot <- night_year_plot + ggtitle("Global Active Power during Sunday Nights")

wmmean <- mean(wintermorning$Global_active_power)
spmmean <- mean(springmorning$Global_active_power)
smmean <- mean(summermorning$Global_active_power)
ammean <- mean(autumnmorning$Global_active_power)

wmmin <- min(wintermorning$Global_active_power)
spmmin <- min(springmorning$Global_active_power)
smmin <- min(summermorning$Global_active_power)
ammean <- min(autumnmorning$Global_active_power)

wmmax <- max(wintermorning$Global_active_power)
spmmax <- max(springmorning$Global_active_power)
smmax <- max(summermorning$Global_active_power)
ammin <- max(autumnmorning$Global_active_power)

wnmean <- mean(winternight$Global_active_power)
spnmean <- mean(springnight$Global_active_power)
snmean <- mean(summernight$Global_active_power)
anmean <- mean(autumnnight$Global_active_power)

wnmin <- min(winternight$Global_active_power)
spnmin <- min(springnight$Global_active_power)
snmin <- min(summernight$Global_active_power)
anmin <- min(autumnnight$Global_active_power)

wnmax <- max(winternight$Global_active_power)
spnmax <- max(springnight$Global_active_power)
snmax <- max(summernight$Global_active_power)
anmax <- max(autumnnight$Global_active_power)

