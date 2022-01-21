library(ggplot2)
library(scales)

## Code for reading in the dataset and/or processing the data
activitydata <- read.csv("C:/Project Files/CURSOS/DataScience-Hopkins/ReproducibleResearch_Week 2_Course Project_1/activity.csv")

StepsPerDay <- aggregate(steps~date, activitydata, sum)
hist(StepsPerDay$steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

summary(activitydata)


## Histogram of the total number of steps taken each day
ggplot(StepsPerDay, aes(x=date, y=steps)) + geom_bar(stat = "identity", width=.6, fill="blue") + ylab("Steps") + xlab("Dates") + labs(title = "Total number of steps taken each day") + theme(axis.text.x = element_text(angle=75, vjust=0.1))

## Mean and median number of steps taken each day

rmean <- mean(StepsPerDay$steps)
rmean

rmedian <- median(StepsPerDay$steps)
rmedian

## Time series plot of the average number of steps taken
AverageStepsPerDay <- aggregate(steps~date, activitydata, mean , na.rm=TRUE)
names(AverageStepsPerDay)<-c("interval", "mean")
ggplot(AverageStepsPerDay, aes(x=interval, y = mean , group=1)) + geom_line() + ylab("Average number of steps") + xlab("Interval") + labs(title = "Average number of steps per interval") + theme(axis.text.x = element_text(angle=75, vjust=0.1))

#The 5-minute interval that, on average, contains the maximum number of steps
max_interval <- activitydata[which.max(activitydata$steps),]$interval
max_interval

##Code to describe and show a strategy for imputing missing data
TotalNAs <- sum(!complete.cases(activitydata))
TotalNAs

## Histogram of the total number of steps taken each day after missing values are imputed
StepsAverage <- aggregate(steps ~ interval, data = activitydata, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activitydata)) {
  obs <- activitydata[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

new_dataset_activity <- activitydata
new_dataset_activity$steps <- fillNA

TotalSteps <- aggregate(steps ~ date, data = new_dataset_activity, sum, na.rm = TRUE)
hist(TotalSteps$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

##Create Histogram to show difference. 

hist(StepsPerDay$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T) 
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=10)

### Mean of the total number of steps taken per day
cmean <- mean(TotalSteps$steps)

### Median of the total number of steps taken per day
cmedian <- median(TotalSteps$steps)

# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
weekdays <- c("lunes", "martes", "miercoles", "jueves", "viernes")

new_dataset_activity$dayoftheweek = as.factor(ifelse(is.element(weekdays(as.Date(new_dataset_activity$date)),weekdays), "Weekday", "Weekend"))
TotalStepsU <- aggregate(steps ~ interval + dayoftheweek, new_dataset_activity, mean)

library(lattice)
xyplot(TotalStepsU$steps ~ TotalStepsU$interval|TotalStepsU$dayoftheweek, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

