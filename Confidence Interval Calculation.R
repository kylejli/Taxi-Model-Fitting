#for prediciting trip time from origin call
trainingSubset <- trainingData4[,c(3,10)] # training subset for origin call and trip time
trainingSubset <- trainingSubset[!is.na(trainingSubset$ORIGIN_CALL),] #excludes incices that have NA
x <- trainingData4$ORIGIN_CALL
x <- na.exclude(x)
maxx <- max(x)
minx <- min(x)
predicttimes <- runif(10000, minx, maxx) #generates random values based on origin call distribution
predictTimes_df <- data.frame(ORIGIN_CALL = predicttimes)
linOut <- qeLin(trainingSubset, 'TRIP_TIME')
x <- predict(linOut, predictTimes_df)
hist(x) #histogram of predicted times
maxx <- max(x)
minx <- min(x)
lowerB <- qunif(0.025, minx, maxx) 
upperB <- qunif(.975, minx, maxx) 
CI <- c(lowerB, upperB) #creates 95% confidence interval
prop <- mean(lowerB <= x & x<= upperB) #find the proportion of predicted times in confidence interval

#for prediciting trip time from timestamp
trainingSubset <- trainingData4[,c(6,10)]  # training subset for timestamp and trip time
x <- trainingData4$TIMESTAMP
maxx <- max(x)
minx <- min(x)
predicttimes <- runif(10000, minx, maxx) #generates random values based on timestamp distribution
predictTimes_df <- data.frame(TIMESTAMP = predicttimes)
linOut <- qeLin(trainingSubset, 'TRIP_TIME')
x <- predict(linOut, predictTimes_df)
hist(x) #histogram of predicted times
maxx <- max(x)
minx <- min(x)
lowerB <- qunif(0.025, minx, maxx) 
upperB <- qunif(.975, minx, maxx) 
CI <- c(lowerB, upperB) #creates 95% confidence interval
prop <- mean(lowerB <= x & x<= upperB) #find the proportion of predicted times in confidence interval

#predicting trip time from distance, call origin, and timestamp
trainingSubset <- trainingData4[,c(3,6,10,11)]  # training subset for distance, call origin, timestamp, and trip time
trainingSubset <- trainingSubset[!is.na(trainingSubset$ORIGIN_CALL),]  #excludes incices that have NA

x <- trainingSubset$TRIP_DISTANCE
xiqr <- IQR(x)
q <- quantile(x)
x <- x[x < q[4] + xiqr * 1.5]
x <- x[x > q[2] - xiqr * 1.5]
m <- mean(x)
s2 <- mean((x-m)^2)
rest <- m^2/s2
lest <- m/s2
predictDistances <- rgamma(10000,  rest, lest) #generates random values based on distance distribution
predictDist_df <- data.frame(TRIP_DISTANCE = predictDistances)

x <- trainingSubset$TIMESTAMP
xiqr <- IQR(x)
maxx <- max(x)
minx <- min(x)
predicttimes <- runif(10000, minx, maxx) #generates random values based on timestamp distribution
predictTimes_df <- data.frame(TIMESTAMP = predicttimes)

x <- trainingData4$ORIGIN_CALL
x <- na.exclude(x)
maxx <- max(x)
minx <- min(x)
predictcall <- runif(10000, minx, maxx) #generates random values based on origin call distribution
predictcall_df <- data.frame(ORIGIN_CALL = predictcall)

linOut <- qeLin(trainingSubset, 'TRIP_TIME') #creates 95% confidence interval
x <- predict(linOut, c(predictcall_df,predictTimes_df,predictDist_df))
hist(x) #histogram of predicted times

m <- mean(x)
s2 <- mean((x-m)^2)
rest <- m^2/s2
lest <- m/s2
lowerB <- qgamma(0.025, rest, lest) 
upperB <- qgamma(.975, rest, lest) 
CI <- c(lowerB, upperB) #creates 95% confidence interval based on gamma
prop <- mean(lowerB <= x & x<= upperB) #find the proportion of predicted times in confidence interval

#creating histograms of taxi id and origin stand
trainingSubset <- trainingData4[,c(5,10)] #5 is taxi id and 4 is origin stand
#trainingSubset <- trainingSubset[!is.na(trainingSubset$ORIGIN_STAND),] #keep commented for taxi id, otherwise uncomment
x <- trainingData4$TAXI_ID # $TAXI_ID is for taxi id and $ORIGIN_STAND is for origin stand
#x <- na.exclude(x) #keep commented for taxi id, otherwise uncomment
hist(x) #see distribution of taxi id or origin stand