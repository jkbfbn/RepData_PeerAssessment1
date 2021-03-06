rm(list=ls())

library("ggplot2")
#Loading and preprocessing the data
activityData <- read.csv("./activity/activity.csv")
head(activityData)

#What is mean total number of steps taken per day?
#1. Make a histogram of the total number of steps taken each day
stepsPerDay <- aggregate(steps ~ date,activityData,sum)
hist(stepsPerDay$steps,breaks=100)
#2. Calculate and report the mean and median total number of steps taken per day
summary(stepsPerDay$steps)

#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all days (y-axis)

#Only complete cases, sind this data rame is used later to replace NAs in the source data frame
stepsPerInterval <- aggregate(steps ~ interval,activityData[(complete.cases(activityData)),],mean)
with(stepsPerInterval,plot(interval,steps,type="l"))
#Which 5-minute interval, on average across all the days in the dataset, contains 
#the maximum number of steps?
maxSteps <- max(stepsPerInterval$steps)
maxInterval <- stepsPerInterval[stepsPerInterval$steps %in% maxSteps,"interval"]

#Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. 
#the total number of rows with NAs)
#sum(is.na(activityData))
summary(activityData)
sum(!complete.cases(activityData))

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use 
#the mean/median for that day, or the mean for that 5-minute interval, etc.

#see, if theres's a pattern in steps per day
with(stepsPerDay,plot(date,steps))

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
#Strategy: replace NAs by mean of steps per interval
replaceNaByIntervalMean <- function(activityData) {
        for (i in 1:nrow(activityData)) {
                if (is.na(activityData[i,"steps"])) {
                        activityData[i,"steps"] <- stepsPerInterval[stepsPerInterval$interval == activityData[i,"interval"],"steps"] 
                } 
        }
        activityData
}

activityDataCC <- replaceNaByIntervalMean(activityData)

#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? What is the 
#impact of imputing missing data on the estimates of the total daily number of steps?

stepsPerDayCC <- aggregate(steps ~ date,activityDataCC,sum)
hist(stepsPerDayCC$steps,breaks=50)
hist(stepsPerDay$steps,breaks=50)
# -> Mit ggplot nebeneinander setzen

summary(stepsPerDayCC$steps)

#-> The quartile range is broader


#Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels -- "weekday" and 
#"weekend" indicating whether a given date is a weekday or weekend day.
weekdaysOfDates <- weekdays(as.Date(activityDataCC$date))
partOfWeek <- function(days) {
        days[days == "Samstag" | days == "Sonntag"] <- "weekend"
        days[days != "weekend"] <- "weekday"
        days
}
activityDataCC$partOfWeek <- as.factor(partOfWeek(weekdaysOfDates)) 

#Make a panel plot containing a time series plot (i.e. type = "l") of the 
#5-minute interval (x-axis) and the average number of steps taken, averaged 
#across all weekday days or weekend days (y-axis).

stepsPerIntervalCC <- aggregate(steps ~ interval + partOfWeek,activityDataCC,mean)
ggplot(stepsPerIntervalCC,aes(x=interval,y=steps)) + geom_line() + facet_grid(partOfWeek~.)


