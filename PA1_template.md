# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data_local <- "activity.zip"
data_unzipped <- "activity"
if(!dir.exists(data_unzipped)){
  dir.create(data_unzipped)
  unzip(data_local, exdir = data_unzipped)
}
```


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
