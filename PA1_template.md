
#Activity Monitoring Data
##Paloma soler
r opts_chunk$set(cache=TRUE)
Loading and preprocessing the data

```r
#1. Load the data
Activity <- read.csv("activity.csv")

#2. Process/transform the data
Activity$date <- as.Date(Activity$date, "%Y-%m-%d")
ActivitySteps <- na.omit(Activity)
Steps_By_Date <- aggregate(steps ~ date, ActivitySteps, sum)
```

What is mean total number of steps taken per day?


```r
#1. Make a histogram of the total number of steps taken each day
hist(Steps_By_Date$steps, 
     col=1, 
     main="Histogram of total number of steps per day",
     xlab="Steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
#2. Calculate and report the mean and median total number of steps taken per day

# Mean
mean(Steps_By_Date$steps)
```

```
## [1] 10766
```

```r
# Median
median(Steps_By_Date$steps)
```

```
## [1] 10765
```

What is the average daily activity pattern?

```r
#1. Make a time series plot of the 5-minute interval 
Steps_By_Interval <- aggregate(steps ~ interval, Activity, mean)

plot(Steps_By_Interval$interval, 
     Steps_By_Interval$steps, 
     type='l', col=1,
     main="Average across all days", 
     xlab="Interval", 
     ylab="Average number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# The Interval with maximum number of steps
max_interval <- which.max(Steps_By_Interval$steps)

Steps_By_Interval [max_interval, ]
```

```
##     interval steps
## 104      835 206.2
```

Imputing missing values



```r
#1. Calculate and report the total number of missing values in the dataset

MissingActivity <- Activity[is.na(Activity)]

length(MissingActivity)
```

```
## [1] 2304
```

```r
#2. Devise a strategy for filling in all of the missing values in the dataset. 

MedianSteps_By_Interval <- aggregate(steps ~ interval, Activity, mean)

for (i in 1:nrow(Activity)){
        if(is.na(Activity$steps[i])){
                interval_value <- Activity$interval[i]
                id <- which(MedianSteps_By_Interval$interval == interval_value)
                median_steps <- MedianSteps_By_Interval$steps[id]
                Activity$steps[i] <- median_steps
        }
}

Steps_By_Date_Complete <- aggregate(steps ~ date, Activity, sum)

hist(Steps_By_Date_Complete$steps, col=1, main="Histogram of total number of steps per day(complete by median)", xlab="Total number of steps in a day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
# Mean
mean(Steps_By_Date$steps)
```

```
## [1] 10766
```

```r
mean(Steps_By_Date_Complete$steps)
```

```
## [1] 10766
```

```r
# Median
median(Steps_By_Date$steps)
```

```
## [1] 10765
```

```r
median(Steps_By_Date_Complete$steps)
```

```
## [1] 10766
```

Are there differences in activity patterns between weekdays and weekends?


```r
# add a new column to indicate the day of the week 
Activity$day <- weekdays(Activity$date)

#weekdays(Sys.Date()+0:6) -- to test the output for weekdays function

# add a new column to indicate if is weekday or weekend
Activity$day_type <- c("weekday")

Activity$date_type <- c("weekday")
for (i in 1:nrow(Activity)){        
        if(Activity$day[i] == "sábado" || Activity$day[i] == "domingo"){
    Activity$day_type[i] <- "weekend"
  }   
}
Activity_Steps_TypeofDay <- aggregate(steps ~ interval+day_type, Activity, mean)


Activity$day_type <- as.factor(Activity$day_type)

library(ggplot2)

p <- qplot(interval, steps, data=Activity_Steps_TypeofDay, geom="line", xlab="Interval",ylab="Steps Number", main="") + facet_wrap(~ day_type, ncol=1)

# Plot Format

p + theme_bw() + theme(strip.background = element_rect( fill = "pink")) + geom_line(colour="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
.
