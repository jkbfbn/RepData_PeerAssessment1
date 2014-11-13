activityData <- read.csv("./activity/activity.csv")
meanTotalPerDay <- aggregate(steps ~ date,activityData,mean,na.rm=T)
with(activityData,hist(steps,date))