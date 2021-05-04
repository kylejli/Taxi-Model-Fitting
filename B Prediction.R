sampData <- sample_n(trainingData4, 100000)

## Single Predictor: Trip Distance
sampSubset <- data.frame(sampData[10:11])
trainingSubset <- data.frame(trainingData4[10:11])

testAcc10 <- vector()
# qeLin with single predictor: Trip Distance
for (i in 1:10) {
  linout <- qeLin(sampSubset, 'TRIP_TIME')
  testAcc10[i] <- linout$testAcc
}
testAcc10 # 10 values of testAcc for qeLin

linout <- qeLin(trainingSubset, 'TRIP_TIME')
predictx <- function(x) { # calls linout.predict(x)
  x1 <- data.frame('TRIP_DISTANCE' = x)
  predict(linout, x1)
}

plot(trainingSubset$TRIP_DISTANCE, trainingSubset$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qePolyLin with single predictor: Trip Distance
for (i in 1:10) {
  polylinout <- qePolyLin(sampSubset, 'TRIP_TIME')
  testAcc10[i] <- polylinout$testAcc
}
testAcc10 # 10 values of testAcc for qePolyLin

polylinout <- qePolyLin(trainingSubset, 'TRIP_TIME')
predictx <- function(x) { # calls polylinout.predict(x)
  x1 <- data.frame('TRIP_DISTANCE' = x)
  predict(polylinout, x1)
}

plot(trainingSubset$TRIP_DISTANCE, trainingSubset$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qeKNN with single predictor: Trip Distance
for (i in 1:10) {
  knnout <- qeKNN(sampSubset, 'TRIP_TIME')
  testAcc10[i] <- knnout$testAcc
}
testAcc10 # 10 values of testAcc for qeKNN

knnout <- qeKNN(trainingSubset, 'TRIP_TIME')
predictx <- function(x) { # calls knnout.predict(x)
  x1 <- data.frame('TRIP_DISTANCE' = x)
  predict(knnout, x1)
}

plot(trainingSubset$TRIP_DISTANCE, trainingSubset$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qeNeural with single predictor: Trip Distance
for (i in 1:10) {
  neuralout <- qeNeural(sampSubset, 'TRIP_TIME')
  testAcc10[i] <- neuralout$testAcc
}
testAcc10 # 10 values of testAcc for qeNeural

neuralout <- qeNeural(trainingSubset, 'TRIP_TIME')
predictx <- function(x) { # calls neuralout.predict(x)
  x1 <- data.frame('TRIP_DISTANCE' = x)
  predict(neuralout, x1)
}

plot(trainingSubset$TRIP_DISTANCE, trainingSubset$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

## Single Predictor: Origin Call
origcallSubset <- trainingData4[! trainingData4$ORIGIN_CALL %in% NA]
origcallSubset2 <- data.frame(c(origcallSubset[3], origcallSubset[10]))

# qeLin with single predictor: Origin Call
linout <- qeLin(origcallSubset2, 'TRIP_TIME')
linout$testAcc

predictx <- function(x) { # calls linout.predict(x)
  x1 <- data.frame('ORIGIN_CALL' = x)
  predict(linout, x1)
}

plot(origcallSubset2$ORIGIN_CALL, origcallSubset2$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qePolyLin with single predictor: Origin Call
polylinout <- qePolyLin(origcallSubset2, 'TRIP_TIME')
polylinout$testAcc

predictx <- function(x) { # calls polylinout.predict(x)
  x1 <- data.frame('ORIGIN_CALL' = x)
  predict(polylinout, x1)
}

plot(origcallSubset2$ORIGIN_CALL, origcallSubset2$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qeKNN with single predictor: Origin Call
knnout <- qeKNN(origcallSubset2, 'TRIP_TIME')
knnout$testAcc

predictx <- function(x) { # calls knnout.predict(x)
  x1 <- data.frame('ORIGIN_CALL' = x)
  predict(knnout, x1)
}

plot(origcallSubset2$ORIGIN_CALL, origcallSubset2$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

# qeNeural with single predictor: Origin Call
neuralout <- qeNeural(origcallSubset2, 'TRIP_TIME')
neuralout$testAcc

predictx <- function(x) { # calls neuralout.predict(x)
  x1 <- data.frame('ORIGIN_CALL' = x)
  predict(neuralout, x1)
}

plot(origcallSubset2$ORIGIN_CALL, origcallSubset2$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")

## Single Predictor: Call Type

trainingSubset2 <- data.frame(c(trainingData4[2], trainingData4[10]))
#substitute A,B,C with 1,2,3 to plot
temp2 <- gsub("A", 1, trainingData4$CALL_TYPE)
temp3 <- gsub("B", 2, temp2)
x1 <- gsub("C", 3, temp3)

# spread out data 0.8 units to better illustrate density
x1 <- as.numeric(x1) + runif(1704759, min=0, max=0.8)

# qeLin with single predictor: Call Type
linout <- qeLin(trainingSubset2, 'TRIP_TIME')
predictx <- function(x) { # calls knnout.predict(x)
  x1 <- data.frame('CALL_TYPE' = x)
  predict(linout, x1)
}
plot(x1,trainingData4$TRIP_TIME)
curve(predictx("A")+0*x,1,1.8,add=TRUE, col='RED')
curve(predictx("B")+0*x,2,2.8,add=TRUE, col='GREEN')
curve(predictx("C")+0*x,3,3.8,add=TRUE, col='BLUE')

# zoom in to 0-5000 Trip Time to show difference in values of A, B, C
plot(x1,trainingData4$TRIP_TIME,ylim=c(0,5000))
curve(predictx("A")+0*x,1,1.8,add=TRUE, col='RED')
curve(predictx("B")+0*x,1,2.8,add=TRUE, col='GREEN')
curve(predictx("C")+0*x,1,3.8,add=TRUE, col='BLUE')

## Two Predictors: Trip Distance and Call Type
trainingSubset3 <- data.frame(c(trainingData4[2], trainingData4[10:11]))

callTypeTimes <- function(callType, dataframe) {
  vectTime <- dataframe$TRIP_TIME
  vectTime[dataframe$CALL_TYPE == callType]
}

tripTimesA <- callTypeTimes('A', trainingData4)
tripTimesB <- callTypeTimes('B', trainingData4)
tripTimesC <- callTypeTimes('C', trainingData4)

callTypeDists <- function(callType, dataframe) {
  vectTime <- dataframe$TRIP_DISTANCE
  vectTime[dataframe$CALL_TYPE == callType]
}

tripDistsA <- callTypeDists('A', trainingData4)
tripDistsB <- callTypeDists('B', trainingData4)
tripDistsC <- callTypeDists('C', trainingData4)

# qeLin with two predictors: Trip Distance and Call Type
linout <- qeLin(trainingSubset3, 'TRIP_TIME')
linout$testAcc

predicta <- function(x){
  x1 <- data.frame(CALL_TYPE='A', TRIP_DISTANCE=x)
  predict(linout, x1)
}
predictb <- function(x){
  x1 <- data.frame(CALL_TYPE='B', TRIP_DISTANCE=x)
  predict(linout, x1)
}
predictc <- function(x){
  x1 <- data.frame(CALL_TYPE='C', TRIP_DISTANCE=x)
  predict(linout, x1)
}

plot(tripDistsA, tripTimesA)
curve(predicta(x),add=TRUE, col="RED")

plot(tripDistsB, tripTimesB)
curve(predictb(x),add=TRUE, col="RED")

plot(tripDistsC, tripTimesC)
curve(predictc(x),add=TRUE, col="RED")

# qeNeural with two predictors: Trip Distance and Call Type
neuralout <- qeNeural(trainingSubset3, 'TRIP_TIME')
neuralout$testAcc

predicta <- function(x){
  x1 <- data.frame(CALL_TYPE='A', TRIP_DISTANCE=x)
  predict(neuralout, x1)
}
predictb <- function(x){
  x1 <- data.frame(CALL_TYPE='B', TRIP_DISTANCE=x)
  predict(neuralout, x1)
}
predictc <- function(x){
  x1 <- data.frame(CALL_TYPE='C', TRIP_DISTANCE=x)
  predict(neuralout, x1)
}

plot(tripDistsA, tripTimesA)
curve(predicta(x),add=TRUE, col="RED")

plot(tripDistsB, tripTimesB)
curve(predictb(x),add=TRUE, col="RED")

plot(tripDistsC, tripTimesC)
curve(predictc(x),add=TRUE, col="RED")

# qeRF with two predictors: Trip Distance and Call Type
sampSubset2 <- data.frame(c(sampData[2], sampData[10:11]))
rfout <- qeRF(sampSubset2, 'TRIP_TIME')
rfout$testAcc

predicta <- function(x){
  x1 <- data.frame(CALL_TYPE='A', TRIP_DISTANCE=x)
  predict(rfout, x1)
}
predictb <- function(x){
  x1 <- data.frame(CALL_TYPE='B', TRIP_DISTANCE=x)
  predict(rfout, x1)
}
predictc <- function(x){
  x1 <- data.frame(CALL_TYPE='C', TRIP_DISTANCE=x)
  predict(rfout, x1)
}

plot(sampSubsetA$TRIP_DISTANCE, sampSubsetA$TRIP_TIME)
curve(predicta(x),add=TRUE, col="RED")

plot(sampSubsetB$TRIP_DISTANCE, sampSubsetB$TRIP_TIME)
curve(predictb(x),add=TRUE, col="RED")

plot(sampSubsetC$TRIP_DISTANCE, sampSubsetC$TRIP_TIME)
curve(predictc(x),add=TRUE, col="RED")

## Testing Different Hyperparameter Accuracies
testAcc10 <- vector()
for (i in 1:10) {
  xout <- qeNeural(sampSubset2, 'TRIP_TIME', nEpoch = 40) # Can substitute qeNeural with other models such as qeRF, and nEpoch to other parameter or values
  testAcc10[i] <- xout$testAcc
}
testAcc10
#3.4.4
traininital <- read.csv("train.csv")
trainingData2<- traininital[! traininital$MISSING_DATA %in% "True", ]
trainingData3<- trainingData2[! trainingData2$POLYLINE %in% "[]", ]
train <- trainingData3[! trainingData3$POLYLINE %in% NA, ]

tripDuration <- nrow(train)
library(stringr)
for(i in 1:nrow(train))
  tripDuration[i] <- (floor(str_count(train[i,"POLYLINE"],",")+1)/2)*15
train$TRIP_TIME <- tripDuration

time_of_day <- nrow(train)
for(i in 1:nrow(train))
  time_of_day[i] <- train[i,"TIMESTAMP"] %% (3600*24)
train$time_of_day <- time_of_day
trainingSubset <- data.frame(train[10:11])
library(regtools)
polylinout <- qePolyLin(trainingSubset, 'TRIP_TIME')
predictx <- function(x) { # calls polylinout.predict(x)
  x1 <- data.frame('time_of_day' = x)
  predict(polylinout, x1)
}
plot(trainingSubset$time_of_day, trainingSubset$TRIP_TIME)
curve(predictx(x),add=TRUE, col="RED")