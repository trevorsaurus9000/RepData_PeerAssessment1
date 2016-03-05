RepData_PeerAssessment1 <- function () {
     
     setwd("C:/THARRIS/Coursera/Reproducible Data")
     
     ## Unpack and load our data set
     if(!file.exists("./data")){dir.create("./data")}
     if(!file.exists("./data/activity.csv")){
          unzip("./activity.zip",exdir = "./data")}
     activityData <- read.csv("./data/activity.csv")
     
     ## Tidy/transform our data so that its more suitable for analysis
     date <- strptime(activityData$date, format = "%Y-%m-%d")
     date <- format(date, "%m/%d/%Y")
     activityData$date <- date
     steps <- activityData$steps
     activityData <- cbind(activityData[,2:3],steps)  ## Reorder variables
     
     ## Ignoring rows with missing data, what is mean total number of steps taken per day?
     activityDataNoNAs <- na.omit(activityData)
     totalStepsPerDay <- aggregate(activityDataNoNAs$steps, by=list(date = activityDataNoNAs$date), FUN=sum)

     png(file = "./meanStepsPerDay.png", width = 400, height = 400)
     hist(totalStepsPerDay$x, right = FALSE, col="royalblue4", main = "Average Total Steps Per Day", xlab = "Total Steps Taken")
     dev.off()
     
     ## Calculate and report the mean and median of the total number of steps taken per day
     meanStepsPerDay <- mean(totalStepsPerDay$x)
     medianStepsPerDay <- median(totalStepsPerDay$x)
     
     ## Ignoring rows with missing data, what is the average daily activity pattern?
     averageStepsPerInterval <- aggregate(activityDataNoNAs$steps, by=list(interval = activityDataNoNAs$interval), FUN=mean)
     
     png(file = "./averageStepsPerInterval.png", width = 400, height = 400)
     plot(averageStepsPerInterval$interval,averageStepsPerInterval$x, type ="l", col="royalblue4",
          main = "Average Steps Per Interval", xlab = "5 Minute Interval", ylab = "Average Steps Taken")
     dev.off()
     
     ## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
     averageStepsPerInterval <- averageStepsPerInterval[with(averageStepsPerInterval, order(-x)),]
     intervalMax <- averageStepsPerInterval[1,1]
     
     ## Imputing missing values
     totalMissingValues <- nrow(activityData) - sum(complete.cases(activityData$steps))
     
     ## For every row missing a value, use that row's interval to extract the average steps for that interval from our
     ##    averageStepsPerInterval data set
     activityDataFixed <- activityData
     i <- 1
     for (i in 1:nrow(activityDataFixed))
     {
          if(is.na(activityDataFixed[i,3]))
          {
               targetValue <- paste0(paste0("^",activityDataFixed[i,2]),"$")
               targetRowNumber <- which(grepl(targetValue,averageStepsPerInterval$interval))
               activityDataFixed[i,3] <- averageStepsPerInterval[targetRowNumber,2]
          }
     }
     totalStepsPerDayFixed <- aggregate(activityDataFixed$steps, by=list(date = activityDataFixed$date), FUN=sum)
     
     png(file = "./meanStepsPerDayNoNAs.png", width = 400, height = 400)
     hist(totalStepsPerDayFixed$x, right = FALSE, col="royalblue4",  xlab = "Total Steps Taken",
          main = "Average Total Steps Per Day\nNAs populated using interval means")
     dev.off()
     
     ## Calculate and report the mean and median of the total number of steps taken per day
     meanStepsPerDayFixed <- mean(totalStepsPerDayFixed$x)
     medianStepsPerDayFixed <- median(totalStepsPerDayFixed$x)
}