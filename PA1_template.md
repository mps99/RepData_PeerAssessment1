# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
library(dplyr)
library(ggplot2)
library(scales)

keep_md: true

loadedData <- read.csv("activity.csv",header = TRUE, colClasses = c("numeric", "Date", "numeric") )
str(loadedData)

## What is mean total number of steps taken per day?
meanStepsDay <- mean(actDay$Steps, na.rm = TRUE)
formatC(meanStepsDay, big.mark = ",", format = "f", digits = 0)
## [1] "10,766"


## What is the average daily activity pattern?

actInterval <- loadedData %.% group_by(interval) %.% summarise(meanSteps = mean(steps,na.rm = TRUE))
p2 <- ggplot(data = actInterval, mapping = aes(x = interval, y = meanSteps)) + 
             geom_line() + scale_x_continuous("Day Interval", breaks = seq(min(actInterval$interval), 
             max(actInterval$interval), 100)) + scale_y_continuous("Average Number of Steps") + 
             ggtitle("Average Number of Steps Taken by Interval")
p2

## Saving to file
dev.copy(png, file="plot2.png", height=800, width=800)
dev.on()

## Imputing missing values
sum(is.na(loadedData$steps))
## [1] 2304



## Are there differences in activity patterns between weekdays and weekends?

##During the weekdays the activity is much higher than weekends, and is highest at 8.35am.
##The weekend activity is lower,starting from 9am in the morning and reaching its peak at around 10am, 12noon and 4pm
##with an mid day over all high activity until 6pm and decreasing after 6pm.
