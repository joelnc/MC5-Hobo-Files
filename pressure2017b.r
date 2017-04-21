## Clear stuff
rm(list=ls())
graphics.off()
library("zoo")
library("broom")

## Just in case
##setwd("c:/Users/95218/Documents/R/MC5")


## 1) Make a regulaurly spaced airFrame, with gaps greater than 5 minutes
## 2) Make a regualrly spaced cDoug, down to 5 minutes
## 3) Fill in gaps in airFrame using cDoug thing
## 4) do some diagnostic plots along the way
##

#############################################################################
#############################################################################

## Define a function to run through a list of dfs, name cols, covert dates
readHoboInterp <- function(dataFile) {
    ## Read in file, rename resulting df columns
    data <- read.csv(paste0(
        "c:/Users/95218.CHARLOTTE/Documents/R/MC5-Hobo-Files/", dataFile),
        sep=",", stringsAsFactors=FALSE, skip=1, header=TRUE)
    data <- data[ ,c(2,3,4)]
    names(data) <- c("dt", "hoboPSI", "hoboF")

    ## Format dates from excel files
    data$dt <- strptime(data$dt, format="%m/%d/%y %I:%M:%S %p",
                        tz="America/Panama")
    data$dt <- as.POSIXct(data$dt)

    ## Create regulalry spaced 5 minute time series overlapping data file
    regTS <- data.frame(dt=seq.POSIXt(to=tail(data$dt,6)[6], by=300,
                      from=round(data$dt[1],"mins")-
                          (round(data$dt[1],"mins")$min%%5)*60),kp=1)

    ## Merge even spaced dt with raw data frame
    dataMerged <- merge(data, regTS, by="dt", all=TRUE)

    ## Interpolate the psi series to the even 5 min time stamps
    zooObj <- zoo(dataMerged[2], dataMerged$dt)
    spline1 <- na.spline(zooObj, na.rm=FALSE)
    dataMerged$MC5ModlPsi <- coredata(spline1)

    ## Interpolate the temperature series to the even 5 min time stamps
    zooObj2 <- zoo(dataMerged[3], dataMerged$dt)
    spline2 <- na.spline(zooObj2, na.rm=FALSE)
    dataMerged$MC5ModlTempF <- coredata(spline2)

    ## drop all but dt, interp temp, interp psi
    dataMerged <- dataMerged[which(dataMerged$kp==1),c(1,5,6)]

    return(dataMerged)
}

#############################################################################
#############################################################################

## Read in and format the directories of files that CB sent
dataAir <- as.list(list.files(
    path="c:/Users/95218.CHARLOTTE/Documents/R/MC5-Hobo-Files",
    pattern="Air.*\\.csv", recursive=TRUE))
names(dataAir) <- paste0("file", seq(1:length(dataAir)))

dataWater <- as.list(list.files(
    path="c:/Users/95218.CHARLOTTE/Documents/R/MC5-Hobo-Files",
    pattern="Water.*\\.csv", recursive=TRUE))
names(dataAir) <- paste0("file",seq(1:length(dataAir)))

## Pass df of water press. file names to f(), rbind results, sort
waterList <- lapply(dataWater, readHoboInterp)
waterFrame <- do.call("rbind", waterList)
waterFrame <- waterFrame[order(waterFrame$dt), ]

dimnames(waterFrame$MC5ModlPsi) <- NULL
dimnames(waterFrame$MC5ModlTempF) <- NULL

## Some unit conversions
waterFrame$MC5ModlHPa <- waterFrame$MC5ModlPsi*68.94757
waterFrame$MC5ModlTempC <- (waterFrame[,"MC5ModlTempF"]-32)/1.8


## Pass df of air press. file names to f(), rbind results, sort
airList <- lapply(dataAir, readHoboInterp)
airFrame <- do.call("rbind", airList)
airFrame <- airFrame[order(airFrame$dt), ]

## Fill gaps with NAs, tempertature regression and pressure convr. from CLT
gapFill <- data.frame(dt=seq.POSIXt(to=tail(airFrame$dt,6)[6], by=300,
                      from=airFrame$dt[1]))
airFrame <- merge(airFrame, gapFill, by="dt", all=TRUE)
dimnames(airFrame$MC5ModlPsi) <- NULL
dimnames(airFrame$MC5ModlTempF) <- NULL

## Some unit conversions
airFrame$MC5ModlHPa <- airFrame$MC5ModlPsi*68.94757
airFrame$MC5ModlTempC <- (airFrame[,"MC5ModlTempF"]-32)/1.8

rm(dataAir, dataWater, readHoboInterp, gapFill, waterList, airList)

##############################################################################
##############################################################################

## Plot spline fits of raw pressure files
dateTicks <- seq.POSIXt(from=min(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      to=max(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      by="month")

graphics.off()
##pdf(file="MC5 Pressure.pdf", width=10, height=7.5)
dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1,1.5,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4))
plot(waterFrame$dt, waterFrame$MC5ModlPsi, pch=16,
     axes=FALSE, xlab="", ylab="", ylim=c(13,18), cex=.2)
points(airFrame$dt, airFrame$MC5ModlPsi, pch=16, col="red", cex=.2)
box()
axis.POSIXct(side=1, at=dateTicks, x=dateTicks, format="%b-%y")
axis(2, las=2)
mtext(side=2, "Pressure (psi)", line=3.5, cex=1.5, font=2)
legend("topleft", legend=c("Water", "Air"), pch=16, pt.cex=.75,
       col=c("black", "red"), bty="n", cex=1.5)
##dev.off()


##############################################################################
##############################################################################

## Load in the Airport data, format and clean up NAs
cDoug <- read.csv("CLTMet.csv", header=TRUE, sep=",",
                  stringsAsFactors=FALSE)
cDoug <- cDoug[ ,c("DATE","HOURLYSeaLevelPressure",
                   "HOURLYStationPressure", "HOURLYDRYBULBTEMPC")]
cDoug$dt <- as.POSIXct(cDoug$DATE, format="%m/%d/%Y %H:%M",
                       tz="America/Panama")
names(cDoug) <- c("date", "CLTSlpHgIn", "CLTStpHgIn", "CLTTempC", "dt")

cDoug$CLTSlpHgIn <- as.numeric(cDoug$CLTSlpHgIn)
cDoug$CLTStpHgIn <- as.numeric(cDoug$CLTStpHgIn)
cDoug$CLTSlpHPa <- cDoug$CLTSlpHgIn*33.8639

cDoug$CLTTempC <- as.numeric(cDoug$CLTTempC)


##############################################################################
## Plot CLT data
dateTicks <- seq.POSIXt(from=min(cDoug$dt, na.rm=TRUE),
                      to=max(cDoug$dt, na.rm=TRUE),
                      by="month")

graphics.off()
##pdf(file="MC5 Pressure.pdf", width=10, height=7.5)
dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1,1.5,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4))
plot(cDoug$dt, cDoug$CLTSlpHgIn, pch=16,
     axes=FALSE, xlab="", ylab="", ylim=c(28,32), cex=.2)
points(cDoug$dt, cDoug$CLTStpHgIn, pch=16, col="red", cex=.2)
box()
axis.POSIXct(side=1, at=dateTicks, x=dateTicks, format="%b-%y")
axis(2, las=2)
mtext(side=2, "Pressure (HgIn)", line=3.5, cex=1.5, font=2)
legend("topleft", legend=c("Sea Level Pressure", "Station Pressure"),
       pch=16, pt.cex=.75, col=c("black", "red"), bty="n", cex=1.5)
##dev.off()

##############################################################################
##############################################################################
## Want to use cDoug pressure at MC5, but need to convert base units, and then
## convert from

##############################################################################
## Loop over cDoug records and convert cDoug SLP to station pressure
## at MC5 using MC5 temperature...
for (j in 1:nrow(cDoug)) {
    currentMatch <- j
    currentIndex <- which(abs(airFrame$dt-cDoug$dt[j])==
                     min(abs(airFrame$dt-cDoug$dt[j])))

    tempA <- airFrame$MC5ModlTempC[currentIndex]
    ## tempA <- cDoug$CLTTempC[currentMatch]
    minus12 <- which(abs(airFrame$dt-(cDoug$dt[j]-(60*60*12)))==
                     min(abs(airFrame$dt-(cDoug$dt[j]-(60*60*12)))))
    tempB <- airFrame$MC5ModlTempC[minus12]
    T <- (sum(tempA,tempB,na.rm=TRUE)/2) +273
    cDoug$MC5StpHpa[j] <-
        cDoug$CLTSlpHPa[j]*exp(-((668)/3.2808)/(T*29.263))
    rm(tempA,tempB,T)
}

## Merge the MC5 File with the cDoug file to see how well actual MC5 HPa
## corresponds with CLT estimated MC5 HPa
tempData <- merge(cDoug, airFrame, by="dt", all=FALSE)

dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1.25,1.25,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4), cex=1.3)
plot(x=tempData$MC5StpHpa, y=tempData$MC5ModlHPa, pch=16,
     cex=.2, xlim=c(970,1020), ylim=c(970,1020),
     xlab="Computed Pressure at MC5 (HPa)",
     ylab="Measured Pressure at MC5 (HPa)")
box()
lines(x=c(970,1020), y=c(970,1020), col="red", lwd=2, lty=2)

dataModel <- lm(tempData$MC5ModlHPa~tempData$MC5StpHpa)
tidy1 <- tidy(dataModel)
lines(x=c(974,1017),y=c(tidy1$estimate[2]*974+tidy1$estimate[1],
                     tidy1$estimate[2]*1017+tidy1$estimate[1]),
      col="blue",lwd=2)
glance(dataModel)

legend("topleft", legend=c("1:1", "Regression"),
       pch=NA, lty=c(2,1), lwd=2, col=c("red", "blue"),
       bty="n", cex=1.3)
##dev.off()

##############################################################################
##############################################################################
##  Okay, but where we need to back fill air pressure at MC5 we don't
##  have MC5 tempertature.  So, compute MC5 pressure using CLT temperature,
## and see how well that corresponds..

for (j in 1:nrow(cDoug)) {
    currentMatch <- j
    currentIndex <- which(abs(cDoug$dt-cDoug$dt[j])==
                     min(abs(cDoug$dt-cDoug$dt[j])))

    tempA <- cDoug$CLTTempC[currentIndex]
    ## tempA <- cDoug$CLTTempC[currentMatch]
    minus12 <- which(abs(cDoug$dt-(cDoug$dt[j]-(60*60*12)))==
                     min(abs(cDoug$dt-(cDoug$dt[j]-(60*60*12)))))
    tempB <- cDoug$CLTTempC[minus12]
    T <- (sum(tempA,tempB,na.rm=TRUE)/2) +273
    cDoug$MC5StpHpaViaClt[j] <-
        cDoug$CLTSlpHPa[j]*exp(-((668)/3.2808)/(T*29.263))
    rm(tempA,tempB,T)
}


## Merge the MC5 File with the cDoug file to see how well actual MC5 HPa
## corresponds with CLT estimated MC5 HPa that is now estimated via
## Airport tempraturte records
tempData2 <- merge(cDoug, airFrame, by="dt", all=FALSE)

dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1.25,1.25,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4), cex=1.3)
plot(x=tempData2$MC5StpHpaViaClt, y=tempData2$MC5ModlHPa, pch=16,
     cex=.2, xlim=c(970,1020), ylim=c(970,1020),
     xlab="Computed Pressure at MC5 (HPa)",
     ylab="Measured Pressure at MC5 (HPa)")
box()
lines(x=c(970,1020), y=c(970,1020), col="red", lwd=2, lty=2)

dataModel2 <- lm(tempData2$MC5ModlHPa~tempData2$MC5StpHpaViaClt)
tidy2 <- tidy(dataModel2)
lines(x=c(974,1017),y=c(tidy2$estimate[2]*974+tidy2$estimate[1],
                     tidy2$estimate[2]*1017+tidy2$estimate[1]),
      col="blue",lwd=2)

legend("topleft", legend=c("1:1", "Regression"),
       pch=NA, lty=c(2,1), lwd=2, col=c("red", "blue"),
       bty="n", cex=1.3)
##dev.off()

glance(dataModel2)
tidy2

##############################################################################
##  Next up, confident that airport can be used to extrapolate, lets do so, and
## interpolate to even 5 minute intervals to match the water pressure records

regTS <- data.frame(dt=seq.POSIXt(to=tail(cDoug$dt,6)[6], by=300,
                                  from=round(cDoug$dt[1],"mins")-
                                      (round(cDoug$dt[1],"mins")$min%%5)*60),
                    kp=1)

cDoug5Min <- merge(cDoug, regTS, by="dt", all=TRUE)


## Interpolate the temperature series to the even 5 min time stamps
zooObj1 <- zoo(cDoug5Min$CLTTempC, cDoug5Min$dt)
spline1 <- na.spline(zooObj1, na.rm=FALSE)
cDoug5Min$CLTModlTempC <- coredata(spline1)

## Interpolate the SLP HPa series to the even 5 min time stamps
zooObj2 <- zoo(cDoug5Min$CLTSlpHPa, cDoug5Min$dt)
spline2 <- na.spline(zooObj2, na.rm=FALSE)
cDoug5Min$CLTModlSlpHPa <- coredata(spline2)

## Slow....
for (j in 1:nrow(cDoug5Min)) {
    currentMatch <- j
    currentIndex <- which(abs(cDoug5Min$dt-cDoug5Min$dt[j])==
                     min(abs(cDoug5Min$dt-cDoug5Min$dt[j])))

    tempA <- cDoug5Min$CLTModlTempC[currentIndex]
    ## tempA <- cDoug5Min$CLTTempC[currentMatch]
    minus12 <- which(abs(cDoug5Min$dt-(cDoug5Min$dt[j]-(60*60*12)))==
                     min(abs(cDoug5Min$dt-(cDoug5Min$dt[j]-(60*60*12)))))
    tempB <- cDoug5Min$CLTModlTempC[minus12]
    T <- (sum(tempA,tempB,na.rm=TRUE)/2) +273
    cDoug5Min$MC5StpHPaViaClt[j] <-
        cDoug5Min$CLTModlSlpHPa[j]*exp(-((668)/3.2808)/(T*29.263))
    rm(tempA,tempB,T)
}

## Some plotlying
library(plotly)
plotlyAirFrame <- airFrame
plotlyAirFrame$MC5ModlPsi <- plotlyAirFrame$MC5ModlPsi[,1]
plotlyAirFrame$MC5ModlTempF <- plotlyAirFrame$MC5ModlTempF[,1]
plotlyAirFrame$MC5ModlHPa <- plotlyAirFrame$MC5ModlHPa[,1]
plotlyAirFrame$MC5ModlTempC <- plotlyAirFrame$MC5ModlTempC[,1]

p <- plot_ly(data=cDoug5Min,
             x=~dt, y=~MC5StpHPaViaClt,
             type="scatter", mode="markers") %>%
    add_trace(data=plotlyAirFrame,
              x=~dt, y=~MC5ModlHPa)
p

## Static plot...
dev.new()
par(xaxs="i", yaxs="i")
plot(cDoug5Min$dt, cDoug5Min$MC5StpHPaViaClt, pch=16, cex=.8,
     col="black")
points(airFrame$dt, airFrame$MC5ModlHPa, pch=16, cex=0.3,
       col="red")



## Merge stuff back together

## 14941
for (j in 1:nrow(airFrame)) {
    if (is.na(airFrame$MC5ModlHPa[j])) {
        airFrame$MC5ModlHPa[j] <-
            cDoug5Min$MC5StpHPaViaClt[which(cDoug5Min$dt==airFrame$dt[j])]
    }
}


## Plot spline fits of raw pressure files
dateTicks <- seq.POSIXt(from=min(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      to=max(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      by="month")

graphics.off()
##pdf(file="MC5 Pressure.pdf", width=10, height=7.5)
dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1,1.5,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4))
plot(waterFrame$dt, waterFrame$MC5ModlHPa, pch=16,
     axes=FALSE, xlab="", ylab="", ylim=c(950,1150), cex=.2)
points(airFrame$dt, airFrame$MC5ModlHPa, pch=16, col="red", cex=.2)
box()
axis.POSIXct(side=1, at=dateTicks, x=dateTicks, format="%b-%y")
axis(2, las=2)
mtext(side=2, "Pressure (psi)", line=3.5, cex=1.5, font=2)
legend("topleft", legend=c("Water", "Air"), pch=16, pt.cex=.75,
       col=c("black", "red"), bty="n", cex=1.5)
##dev.off()
























## Back fill air sensor data from Airport data

##############################################################################
## Make a plot and fit a regression between air sensor temp and CLT temp
## Merge subset of CLT and MC5 that naturally co-occur in time
compareTemps <- merge(cDoug, airFrame, by="dt", all=FALSE)

## Do plotting
dev.new()
par(xaxs="i", yaxs="i")
plot(compareTemps$CLTTempC,(compareTemps$MC5ModlTempF-32)/1.8, pch=16,
     ylim=c(-14,42), xlim=c(-14,42), cex=.3)
lines(x=c(-14,42), y=c(-14,42), col="red", lwd=2)

## Do regresion fit and more plotting
dataModel <- lm(((compareTemps$MC5ModlTempF-32)/1.8)~compareTemps$CLTTempC)
tidy1 <- tidy(dataModel)
lines(x=c(-9,38),y=c(tidy1$estimate[2]*-9+tidy1$estimate[1],
                     tidy1$estimate[2]*38+tidy1$estimate[1]),
      col="blue",lwd=2)
glance(dataModel)

##############################################################################
## Temperature regression, interpolate 5 min CLT temp

regTS <- data.frame(dt=seq.POSIXt(to=tail(cDoug$dt,6)[6], by=300,
                                  from=round(cDoug$dt[1],"mins")-
                                      (round(cDoug$dt[1],"mins")$min%%5)*60),
                    kp=1)

cDougReg <- merge(cDoug, regTS, by="dt", all=TRUE)
## Apparently unnecessary, previously incorrect (used sum)....
## ... could be used to average duplicate time stamp data, but there are none....
##cDoug2  <- aggregate(cDoug[ ,c(3,4,5,6)], by=list(cDoug$dt), mean)

## Interpolate the temperature series to the even 5 min time stamps
zooObj1 <- zoo(cDougReg$CLTTempC, cDougReg$dt)
spline1 <- na.spline(zooObj1, na.rm=FALSE)
cDougReg$CLTModlTempC <- coredata(spline1)

## Interpolate the hpa series to the even 5 min time stamps
zooObj2 <- zoo(cDougReg$CLTSlpHgIn, cDougReg$dt)
spline2 <- na.spline(zooObj2, na.rm=FALSE)
cDougReg$CLTModlSlpHgIn <- coredata(spline2)

## Merge and compare again... very similar model
compareTemps2 <- merge(cDougReg, airFrame, by.x="dt", by.y="dt",
                       all=FALSE)
dev.new()
plot(compareTemps2$CLTModlTempC,(compareTemps2$MC5ModlTempF-32)/1.8, pch=16,
     ylim=c(-14,42), xlim=c(-14,42), cex=.3)
lines(x=c(-14,42), y=c(-14,42),col="red", lwd=2)

modelModel <- lm(((compareTemps2$MC5ModlTempF-32)/1.8)~
                     compareTemps2$CLTModlTempC)
tidy2 <- tidy(modelModel)
lines(x=c(-9,38), y=c(tidy2$estimate[2]*-9+tidy2$estimate[1],
                     tidy2$estimate[2]*38+tidy2$estimate[1]),
      col="blue", lwd=2)
glance(modelModel)


##

## Do regression back fill from dataModel (not modelModel)
af3 <- merge(airFrame, cDougReg, by.x="dt", by.y="dt", all=FALSE)
af3[,"MC5ModlTempC"] <- (af3[,"MC5ModlTempF"]-32)/1.8

af3[which(is.na(af3[,"MC5ModlTempC"])),"MC5ModlTempC"] <-
    tidy1$estimate[2]*af3[which(is.na(af3[,"MC5ModlTempC"])),"CLTModlTempC"]+
    tidy1$estimate[1]

## Compute pressure
af3$mc5HPa <- af3$modPsi*68.9475729
af3[which(is.na(af3[,"mc5HPa"])),"mc5HPa"] <-


## Alt, convert sea level press direct to Reedy station pressure
for (j in 1:nrow(af3)) {
    currentMatch <- j
    tempA <- af3$mc5tempC[currentMatch]
    minus12 <- which(abs(af3$dt-(af3$dt[j]-(60*60*12)))==
                     min(abs(af3$dt-(af3$dt[j]-(60*60*12)))))
    tempB <- af3$mc5tempC[minus12]
    T <- (sum(tempA,tempB,na.rm=TRUE)/2) +273
    af3$mc5HPaCLT[j] <-
        af3$modHPa[j]*exp(-((668)/3.2808)/(T*29.263))
    rm(tempA,tempB,T)
}

af3$mc5HPa[which(is.na(af3[,"mc5HPa"]))] <-
    af3$mc5HPaCLT[which(is.na(af3[,"mc5HPa"]))]

dev.new()
plot(af3[,10], af3[,11], pch=16,cex=.5)






points(cDoug$dt, cDoug$my13BPressB,pch=16,cex=.25,col="navy")

plot(cDoug$my13BPress,cDoug$my13BPressB)
summary(lm(cDoug$my13BPress~cDoug$my13BPressB))














plot(cDoug$dt,cDoug$tempC, pch=16, cex=.5)
points(airFrame$dt,(airFrame[,3]-32)/1.8,col="blue")




## Alt, convert sea level press direct to Reedy station pressure
for (i in 1:nrow(cDoug)) {
    currentMatch <-
       which(abs(my13Batm$dt-cDoug$dt[j])==min(abs(my13Batm$dt-cDoug$dt[j])))
    tempA <- my13Batm$temp[currentMatch]
    minus12 <- which(abs(my13Batm$dt-(cDoug$dt[j]-(60*60*12)))==min(abs(my13Batm$dt-(cDoug$dt[j]-(60*60*12)))))
    tempB <- my13Batm$temp[minus12]
    T <- (sum(tempA,tempB,na.rm=TRUE)/2) +273
    cDoug$my13BPressB[j] <-
        cDoug$slpHPa[j]*exp(-((638)/3.2808)/(T*29.263))
    rm(tempA,tempB,T)
}
points(cDoug$dt, cDoug$my13BPressB,pch=16,cex=.25,col="navy")

plot(cDoug$my13BPress,cDoug$my13BPressB)
summary(lm(cDoug$my13BPress~cDoug$my13BPressB))






## Just to see
dev.new()
plot(cDoug$dt,cDoug$slpHPa, pch=16, cex=.5, ylim=c(960,1040))
