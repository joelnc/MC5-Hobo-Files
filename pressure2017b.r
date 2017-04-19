## Clear stuff
rm(list=ls())
graphics.off()
library("zoo")
library("broom")

## Just in case
setwd("c:/Users/95218/Documents/R/MC5")

#############################################################################
#############################################################################

## Define a function to run through a list of dfs, name cols, covert dates
readHoboInterp <- function(dataFile) {
    ## Read in file, rename resulting df columns
    data <- read.csv(paste0("c:/Users/95218/Documents/R/MC5/", dataFile),
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
    dataMerged$modPsi <- coredata(spline1)

    ## Interpolate the temperature series to the even 5 min time stamps
    zooObj2 <- zoo(dataMerged[3], dataMerged$dt)
    spline2 <- na.spline(zooObj2, na.rm=FALSE)
    dataMerged$modTemp <- coredata(spline2)

    ## drop all but dt, interp temp, interp psi
    dataMerged <- dataMerged[which(dataMerged$kp==1),c(1,5,6)]

    return(dataMerged)
}

#############################################################################
#############################################################################

## Read the directories of files that CB sent
dataAir <- as.list(list.files(path="c:/Users/95218/Documents/R/MC5",
                      pattern="Air.*\\.csv", recursive=TRUE))
names(dataAir) <- paste0("file", seq(1:length(dataAir)))

dataWater <- as.list(list.files(path="c:/Users/95218/Documents/R/MC5",
                    pattern="Water.*\\.csv", recursive=TRUE))
names(dataAir) <- paste0("file",seq(1:length(dataAir)))

## Pass df of water press. file names to f(), rbind results, sort
waterList <- lapply(dataWater, readHoboInterp)
waterFrame <- do.call("rbind", waterList)
waterFrame <- waterFrame[order(waterFrame$dt), ]

## Pass df of air press. file names to f(), rbind results, sort
airList <- lapply(dataAir, readHoboInterp)
airFrame <- do.call("rbind", airList)
airFrame <- airFrame[order(airFrame$dt), ]

## Fill gaps with NAs, tempertature regression and pressure convr. from CLT
gapFill <- data.frame(dt=seq.POSIXt(to=tail(airFrame$dt,6)[6], by=300,
                      from=airFrame$dt[1]))
airFrame <- merge(airFrame, gapFill, by="dt", all=TRUE)
dimnames(airFrame$modPsi) <- NULL
dimnames(airFrame$modTemp) <- NULL

rm(dataAir, dataWater, readHoboInterp, gapFill, waterList, airList)

##############################################################################
##############################################################################

## Plot spline fits from raw pressure files
dateTicks <- seq.POSIXt(from=min(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      to=max(waterFrame$dt,airFrame$dt,na.rm=TRUE),
                      by="month")

graphics.off()
##pdf(file="MC5 Pressure.pdf", width=10, height=7.5)
dev.new(width=10,height=7.5, xpos=1930,ypos=65)
par(xaxs="i", yaxs="i", mai=c(1,1.5,.5,.5), font=2,
    cex.axis=1.2, family="serif", omi=rep(0,4))
plot(waterFrame$dt, waterFrame$modPsi, pch=16,
     axes=FALSE, xlab="", ylab="", ylim=c(13,18), cex=.2)
points(airFrame$dt, airFrame$modPsi, pch=16, col="red", cex=.2)
box()
axis.POSIXct(side=1, at=dateTicks, x=dateTicks, format="%b-%y")
axis(2, las=2)
mtext(side=2, "Pressure (psi)", line=3.5, cex=1.5, font=2)

##dev.off()

##############################################################################
##############################################################################

## Back fill air sensor data from Airport data

##############################################################################
## Load in the Airport data, format and clean up NAs
cDoug <- read.csv("CLTMet.csv", header=TRUE, sep=",",
                  stringsAsFactors=FALSE)
cDoug <- cDoug[ ,c("DATE","HOURLYSeaLevelPressure",
                   "HOURLYStationPressure", "HOURLYDRYBULBTEMPC")]
cDoug$dt <- as.POSIXct(cDoug$DATE, format="%m/%d/%Y %H:%M",
                       tz="America/Panama")
names(cDoug) <- c("date", "slpHgIn", "stpHgIn", "tempC", "dt")

cDoug$slpHgIn <- as.numeric(cDoug$slpHgIn)
cDoug$stpHgIn <- as.numeric(cDoug$stpHgIn)
cDoug$tempC <- as.numeric(cDoug$tempC)

## for (i in 1:nrow(cDoug)) {
##     if (nchar(cDoug$V4[i])==3) {
##         cDoug$V4[i] <- paste0("0",cDoug$V4[i])
##     }
##     if (nchar(cDoug$V4[i])==2) {
##         cDoug$V4[i] <- paste0("00",cDoug$V4[i])
##     }
##     if (nchar(cDoug$V4[i])==1) {
##         cDoug$V4[i] <- paste0("000",cDoug$V4[i])
##     }
##     if (!is.na(cDoug$V25[i]) & cDoug$V25[i]>9000) {
##         cDoug$V25[i] <- NA
##     }
##     if (!is.na(cDoug$V21[i]) & cDoug$V21[i]>800) {
##         cDoug$V21[i] <- NA
##     }
## }

## ## Date and time are in UTC, stored numeric.  Do some base R gymnastics to
## ## convert to a EST posix.ct value.
## cDoug$dt <- strptime(x=paste(cDoug$V3, cDoug$V4, sep=""),
##                      format="%Y%m%d %H%M", tz="UTC") ## make date time obj
## cDoug$dt <- as.POSIXct(cDoug$dt) ## convert from lt to ct
## ## Format to an EST character array
## cDoug$dt <- format(cDoug$dt, tz="America/Panama", usetz=TRUE)
## ## as.POSIXct it again away from chr...
## cDoug$dt <- as.POSIXct(cDoug$dt, format="%Y-%m-%d %T", tz="America/Panama")

## names(cDoug) <- c("date", "time", "tempC", "slpHPa", "dt")

##############################################################################
## Make a plot and fit a regression between air sensor temp and CLT temp
## Merge subset of CLT and MC5 that naturally co-occur in time
compareTemps <- merge(cDoug, airFrame, by="dt", all=FALSE)

## Do plotting
dev.new()
par(xaxs="i", yaxs="i")
plot(compareTemps$tempC,(compareTemps$modTemp-32)/1.8, pch=16,
     ylim=c(-14,42), xlim=c(-14,42), cex=.5)
lines(x=c(-14,42), y=c(-14,42), col="red", lwd=2)

## Do regresion fit and more plotting
dataModel <- lm(((compareTemps$modTemp-32)/1.8)~compareTemps$tempC)
tidy1 <- tidy(dataModel)
lines(x=c(-9,38),y=c(tidy1$estimate[2]*-9+tidy1$estimate[1],
                     tidy1$estimate[2]*38+tidy1$estimate[1]),
      col="blue",lwd=2)
glance(dataModel)

##############################################################################
## Temperature regression, interpolate 5 min CLT temp

regTS <- data.frame(dt=seq.POSIXt(to=tail(cDoug$dt,6)[6], by=300,
                                    from=cDoug$dt[1]), kp=1)

cDoug <- merge(cDoug, regTS, by="dt", all=TRUE)
## Apparently unnecessary, previously incorrect (used sum)....
## ... could be used to average duplicate time stamp data, but there are none....
cDoug  <- aggregate(cDoug[ ,c(3,4,5,6)], by=list(cDoug$dt), mean)

## Interpolate the temperature series to the even 5 min time stamps
zooObj <- zoo(cDoug$tempC, cDoug$Group.1)
spline1 <- na.spline(zooObj, na.rm=FALSE)
cDoug$modTemp <- coredata(spline1)

## Interpolate the hpa series to the even 5 min time stamps
zooObj2 <- zoo(cDoug$slpHgIn,cDoug$Group.1)
spline2 <- na.spline(zooObj2,na.rm=FALSE)
cDoug$modHgIn <- coredata(spline2)

## Merge and compare again... very similar model
compareTemps2 <- merge(cDoug, airFrame, by.x="Group.1", by.y="dt",
                       all=FALSE)
dev.new()
plot(compareTemps2$modTemp.x,(compareTemps2$modTemp.y-32)/1.8, pch=16,
     ylim=c(-14,42), xlim=c(-14,42), cex=.3)
lines(x=c(-14,42), y=c(-14,42),col="red", lwd=2)

modelModel <- lm(((compareTemps2$modTemp.y-32)/1.8)~compareTemps2$modTemp.x)
tidy2 <- tidy(modelModel)
lines(x=c(-9,38),y=c(tidy2$estimate[2]*-9+tidy2$estimate[1],
                     tidy2$estimate[2]*38+tidy2$estimate[1]),
      col="blue",lwd=2)
glance(modelModel)

## Do regression back fill from dataModel (not modelModel)
af3 <- merge(airFrame, cDoug, by.x="dt", by.y="Group.1", all=FALSE)
af3[,"mc5tempC"] <- (af3[,3]-32)/1.8
af3[which(is.na(af3[,"mc5tempC"])),"mc5tempC"] <-
    tidy1$estimate[2]*af3[which(is.na(af3[,"mc5tempC"])),"modTemp.y"]+
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
