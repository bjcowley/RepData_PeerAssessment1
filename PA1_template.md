---
title: "Reproducible Research - Project 1"
output: 
  html_document: 
    keep_md: yes
---

# Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The raw dataset is at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

# Loading and preprocessing the data

```r
ActivityData <- read.csv("activity.csv")
```

# What is mean total number of steps taken per day?
- Calculate the total number of steps taken per day. 
- Make a histogram of the total number of steps taken each day.

```r
DailySteps <- tapply(ActivityData$steps, ActivityData$date, sum)

hist(DailySteps,
     col = "green",
     xlab = "Number of Steps",
     main = "Number of Steps / Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

- Calculate and report the mean and median of the total number of steps taken per day.

```r
mean(DailySteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(DailySteps, na.rm = TRUE)
```

```
## [1] 10765
```

# What is the average daily activity pattern?
- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
IntervalData <-
  tapply(ActivityData$steps, ActivityData$interval, mean, na.rm = TRUE)

plot(
  as.numeric(names(IntervalData)),
  IntervalData,
  xlab = "Interval",
  ylab = "Steps",
  main = "Average Daily Steps",
  type = "l"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(sort(IntervalData, decreasing = TRUE)[1])
```

```
## [1] "835"
```

# Imputing missing values
- Calculate and report the total number of missing values in the dataset.

```r
nrow(ActivityData[is.na(ActivityData$steps),])
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset.
  - I will replace the missing values with the mean of the valid entries in that interval.

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
IntervalData <-
  tapply(ActivityData$steps, ActivityData$interval, mean, na.rm = TRUE)

IntervalDataSplit <- split(ActivityData, ActivityData$interval)
for (lp in 1:length(IntervalDataSplit)) {
  IntervalDataSplit[[lp]]$steps[is.na(IntervalDataSplit[[lp]]$steps)] <-
    IntervalData[lp]
}

ActivityDataImputed <- do.call("rbind", IntervalDataSplit)
ActivityDataImputed <-
  ActivityDataImputed[order(ActivityDataImputed$date), ]
```

- Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
DailyStepsImputed <-
  tapply(ActivityDataImputed$steps, ActivityDataImputed$date, sum)

hist(DailyStepsImputed,
     col = "red",
     xlab = "Number of Steps",
     main = "Number of Steps / Day (Imputed)")

hist(
  DailySteps,
  col = "green",
  xlab = "Number of Steps",
  main = "Number of Steps / Day (Imputed)",
  add = TRUE
)

legend("topright",
       c("Imputed Data", "Original Date"),
       fill = c("red", "green"))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(DailyStepsImputed, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(DailyStepsImputed, na.rm = TRUE)
```

```
## [1] 10766.19
```

We can see that by replacing the missing values with the mean for that interval, the median increases slightly, but the mean stays the same. 

# Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
ActivityDataImputed$DOW <-
  ifelse(
    weekdays(as.Date(ActivityDataImputed$date)) == "Saturday" |
      weekdays(as.Date(ActivityDataImputed$date)) == "Sunday",
    "weekend",
    "weekday"
  )

IntervalDataWeekday <-
  tapply(ActivityDataImputed[ActivityDataImputed$DOW == "weekday", ]$steps,
         ActivityDataImputed[ActivityDataImputed$DOW == "weekday", ]$interval,
         mean,
         na.rm = TRUE)

IntervalDataWeekend <-
  tapply(ActivityDataImputed[ActivityDataImputed$DOW == "weekend", ]$steps,
         ActivityDataImputed[ActivityDataImputed$DOW == "weekend", ]$interval,
         mean,
         na.rm = TRUE)
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow = c(1, 2))

plot(
  as.numeric(names(IntervalDataWeekday)),
  IntervalDataWeekday,
  xlab = "Interval",
  ylab = "Steps",
  main = "Average Weekday Steps",
  type = "l"
)

plot(
  as.numeric(names(IntervalDataWeekend)),
  IntervalDataWeekend,
  xlab = "Interval",
  ylab = "Steps",
  main = "Average Weekend Steps",
  type = "l"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

