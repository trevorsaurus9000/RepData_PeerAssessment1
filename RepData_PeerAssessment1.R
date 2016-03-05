RepData_PeerAssessment1 <- function () {
     
     setwd("C:/THARRIS/Coursera/Reproducible Data")
     
     ##Loading and preprocessing the data
     if(!file.exists("./data")){dir.create("./data")}
     if(!file.exists("./data/activity.csv")){
          unzip("./activity.zip",exdir = "./data")}
     activityData <- read.csv("./data/activity.csv")

     activityData
}