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
     stepsPerDay <- aggregate(activityDataNoNAs$steps, by=list(date = activityDataNoNAs$date), FUN=sum)
     totalStepsPerDay <- sum(stepsPerDay$x)
     medianStepsPerDay <- median(stepsPerDay$x)
     meanStepsPerDay <- mean(stepsPerDay$x)

     png(file = "./meanStepsPerDay.png", width = 480, height = 480)
     hist(stepsPerDay$x, right = FALSE, col="royalblue4", main = "Steps Taken Per Day", xlab = "Steps")
     dev.off()
}