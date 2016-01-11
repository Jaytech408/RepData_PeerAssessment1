# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
```
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "./Wearableactivity.zip")
unzip("./Wearabledata")
data <- read.csv("./activity.csv", colClasses = c("integer","Date","numeric"))
```
2. Process/transform the data (if necessary) into a format suitable for analysis

```r
#This tranformation is only for the last steps: 
# Load the raw activity data
activity_raw <-  read.csv("./activity.csv", colClasses = c("integer","Date","numeric"))    
# Transform the date attribute to an actual date format
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)

# Compute the day type (weekend or weekday)
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                     activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))

# Create the final data.frame
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)
```
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
```
library(dplyr)
stepsdata <- group_by(data,date)
steps <- summarise(stepsdata, total = sum(steps,na.rm=TRUE))
ggplot(steps,aes(date,total)) + geom_bar(stat = "identity",color = "black", width = 30, fill = "black") + labs(y="Steps", x="Date", title = "Total Number of Steps Taken Each Day")
```
2. Calculate and report the mean and median total number of steps taken per day
```
summary(steps$total)
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```
steps_rm_na<-filter(data,!is.na(steps))
stepsdata2<-group_by(steps_rm_na,interval)
steps2<-summarise(stepsdata2,average=mean(steps))
ggplot(data=steps2,aes(x=interval,y=average))+geom_line()+ labs(x ="Interval", y="Mean number of steps", title = "Average Steps By Interval")
```
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```
steps2[steps2$average == max(steps2$average),]
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```
sum(is.na(data))
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```
data3 <- data 
for (i in 1:nrow(data3)) {
    if (is.na(data3$steps[i])) {
        data3$steps[i] <- steps2[which(data3$interval[i] == steps2$interval), ]$average
    }
}
```
Display the first few rows of the new activity data frame:
```
head(data3)
```
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```
groupSteps3 <- group_by(data3, date)
steps3 <- summarise(groupSteps3,total=sum(steps))
ggplot(steps3,aes(date,total)) + geom_bar(stat="identity",color ="black",fill="black", width = 0.7)+ labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```
Compute the mean and median.
```
mean(data3$steps)
median(data3$steps)
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - “weekdays” and “weekend” indicating whether a given date is a weekday or weekend day.
```
# The new factor variable "daytype" was created in the first step
head(activity)

# Here we will create a new data frame with the daytype and average steps taken each day
# Find the NA positions
na_pos <- which(is.na(activity$steps))

# Create a vector of means
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))

# Replace the NAs by the means
activity[na_pos, "steps"] <- mean_vec
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```
library(lattice)
# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")
```
