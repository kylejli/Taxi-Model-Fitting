library(regtools)
library(dplyr)
library(stringr)
library(geosphere)

# data cleaning and setting up additional data sets
trainingData <- read.csv("train.csv")
trainingData2<- trainingData[! trainingData$MISSING_DATA %in% "True", ]
trainingData3<- trainingData2[! trainingData2$POLYLINE %in% "[]", ]

len <- length(trainingData3$POLYLINE)
dist <- vector()

for(i in 1:len) {
  temp1 <- gsub("\\[|\\]", "", trainingData3$POLYLINE[i])
  temp <- str_split(temp1, ",")
  tempLen <- length(temp[[1]])
  # finding distance between first and last pair of coordinates
  dist[i] <- distm(c(as.double(temp[[1]][1]), as.double(temp[[1]][2])), c(as.double(temp[[1]][tempLen-1]), as.double(temp[[1]][tempLen])), fun = distHaversine)
}

# adding a column of trip times and trip distances to data set
trainingData4 <- cbind(trainingData3[1:9], TRIP_TIME=(floor(str_count(trainingData3$POLYLINE, ',')/2) + 1) * 15, TRIP_DISTANCE=dist)


timeVect <- trainingData4$TRIP_TIME

# Part B Bullet 1 
# removing outliers
quantileTime <- quantile(timeVect)
timeIQR <- IQR(timeVect)
timeVect <- timeVect[timeVect < quantileTime[4] + timeIQR * 1.5]
timeVect <- timeVect[timeVect > quantileTime[2] - timeIQR * 1.5]
timeVect <- timeVect[timeVect > 15]
hist(timeVect)
quantileTime[2] - timeIQR * 1.5

# generating parameters for gamma distribution
m1 <- mean(timeVect)
newS2 <- mean((timeVect - m1)^2)
rest <- m1^2 / newS2
lambest <- m1 / newS2
hist(timeVect, freq = FALSE)
curve(dgamma(x, rest, lambest), add = TRUE)

# Part B Bullet 2
taxiDrivers <- unique(trainingData$TAXI_ID) # vector of the taxi drivers

# returns vector of trip times for a specific driverID
driverTimes <- function(driverID, dataframe) {
  polylineCol <- dataframe$POLYLINE
  vectPoly <- polylineCol[dataframe$TAXI_ID == driverID]
  retVect <- vector()
  i <- 0
  for(trip in vectPoly){
    i <- i + 1
    retVect[i] <- ((floor(str_count(vectPoly[i], ',')/2) + 1) * 15) 
  }
  retVect
}

# returns proportion driverID is busy
getShift <- function(driverID, dataframe) {
  times <- dataframe$timestamp
  totalTime <- 0
  shiftVect <- vector()
  busyVect <- vector()
  i <- 0
  j <- 0
  for(driver in driverID) {
    vectTimes <- times[dataframe$TAXI_ID == driver] # extract the trip start times that correspond to a specific driver
    tripDurations <- driverTimes(driver, dataframe) # get the durations of the driver's trips
    differenceVect <- diff(vectTimes) # calculate the difference between start times of trips
    for(difference in differenceVect) {
      j <- j + 1
      if(difference > 14400) { # if the difference between start times > 4 hours, assume new shift
        i <- i + 1
        shiftVect[i] <- totalTime + tripDurations[j]
        totalTime <- 0
      }
      else {
        totalTime <- totalTime + difference
      }
    }
    busyTime <- sum(tripDurations) # add up times of all the trips
    shiftTime <- sum(shiftVect) # find the total shift time for the driver
  }
  return(busyTime/shiftTime)
}

# returns vector of proportion each driver is busy
dataAvgBusy <- function(taxiDrivers, dataFrame) {
  retVect <- vector()
  index <- 0
  for(driver in taxiDrivers) {
    index <- index + 1
    avg <- getShift(driver, dataFrame)
    retVect[index] <- avg
  }
  return(retVect)
}

driverTimesBusy <- dataAvgBusy(taxiDrivers, trainingData)

# Removing outliers
driverTimesBusy <- driverTimesBusy[driverTimesBusy < 1]
busyIQR <- IQR(driverTimesBusy)
busyQuantile <- quantile(driverTimesBusy)
driverTimesBusy <- driverTimesBusy[driverTimesBusy < busyQuantile[4] + busyIQR * 1.5]
driverTimesBusy <- driverTimesBusy[driverTimesBusy > busyQuantile[2] - busyIQR * 1.5]

# modeling normal distribution on data
hist(driverTimesBusy, freq = FALSE)
mu <- mean(driverTimesBusy)
sdev <- sqrt(mean((driverTimesBusy - mu) ^ 2))
curve(dnorm(x, mu, sdev), 0, 0.4, add = TRUE)

# Part B Bullet 3
# returns vector of trip times for each call type
callTypeTimes <- function(callType, dataframe) {
  vectTime <- dataframe$TRIP_TIME
  vectTime[dataframe$CALL_TYPE == callType]
}

tripTimesA <- callTypeTimes('A', trainingData4)
tripTimesB <- callTypeTimes('B', trainingData4)
tripTimesC <- callTypeTimes('C', trainingData4)

# finds radius of confidence interval for difference of means between vectors of two call types
findMargError <- function(callType1Times, callType2Times) {
  mean1 <- mean(callType1Times)
  mean2 <- mean(callType2Times)
  sdev1 <- sqrt(mean((callType1Times - mean1) ^ 2))
  sdev2 <- sqrt(mean((callType2Times - mean2) ^ 2))
  n1 <- length(callType1Times)
  n2 <- length(callType2Times)
  retVal <- sqrt((sdev1^2 / n1) + (sdev2^2 / n2))
  return(retVal * qnorm(0.975)) # substitute in qnorm(0.995) for 99%, qnorm(0.9995) for 99.9% confidence intervals
}

margAB <- findMargError(tripTimesA, tripTimesB)
margCB <- findMargError(tripTimesC, tripTimesB)
margCA <- findMargError(tripTimesC, tripTimesA)

diffAB <- mean(tripTimesA) - mean(tripTimesB)
diffCA <- mean(tripTimesC) - mean(tripTimesA)
diffCB <- mean(tripTimesC) - mean(tripTimesB)

# generating confidence intervals
AB_CI <- c(diffAB - margAB, diffAB + margAB)
CB_CI <- c(diffCB - margBC, diffCB + margBC)
CA_CI <- c(diffCA - margAC, diffCA + margAC)
AB_CI
CB_CI
CA_CI