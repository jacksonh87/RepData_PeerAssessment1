# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# Check whether the raw data has been unzipped and unzip if necessary
data_local <- "activity.zip"
data_unzipped <- "activity"
if(!dir.exists(data_unzipped)){
  dir.create(data_unzipped)
  unzip(data_local, exdir = data_unzipped)
}
# Read the data
data <- read.csv("activity/activity.csv")
```


## What is mean total number of steps taken per day?

```r
daily_total <- aggregate(steps~date, data = data, sum, na.rm = TRUE)
hist(daily_total$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
print(paste('The mean total number of steps taken per day is', round(mean(daily_total$steps),0)))
```

```
## [1] "The mean total number of steps taken per day is 10766"
```

```r
print(paste('The median total number of steps taken per day is', round(median(daily_total$steps),0)))
```

```
## [1] "The median total number of steps taken per day is 10765"
```


## What is the average daily activity pattern?

```r
interval_mean <- aggregate(steps~interval, data = data, mean, na.rm = TRUE)
plot(steps~interval, data = interval_mean, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
print(paste('The highest number of steps, on average, occurs in interval',interval_mean$interval[which.max(interval_mean$steps)]))
```

```
## [1] "The highest number of steps, on average, occurs in interval 835"
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?