---
title: "Course Project 1 Rmd"
author: "Zhi Sun"
date: "2021/1/8"
output: html_document
---

This R Markdown file is the Course Project 1 for Reproducible Research, Week 2.

## Loading and preprocessing the data

First Load the data, save it into `RowData`.


```r
RowData <- read.csv("Data/activity.csv")
head(RowData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.


```r
TotalStepPerDay <- tapply(RowData$steps, RowData$date, sum, na.rm = TRUE)
head(TotalStepPerDay)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

2. Make a histogram of the total number of steps taken each day.


```r
setwd("Figure")
png("the total number of steps taken each day.png")
hist(TotalStepPerDay, main = "the total number of steps taken each day", xlab = "the total steps")
dev.off()
```

```
## png 
##   2
```

3. Calculate the mean and median of the total number of steps taken per day.


```r
Mean <- mean(TotalStepPerDay); 
print(Mean)
```

```
## [1] 9354.23
```


```r
Median <- median(TotalStepPerDay)
print(Median)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.


```r
AverageDailySteps <- tapply(RowData$steps, RowData$interval, mean, na.rm = TRUE)
setwd("Figure")
png("average daily steps.png")
plot(names(AverageDailySteps), AverageDailySteps, type = "l", xlab = "Time Series of 5-minute Interval", ylab = "Average Daily Steps")
dev.off()
```

```
## png 
##   2
```

2. Which 5-minute interval contains the maximum number of steps?


```r
print(names(sort(AverageDailySteps, decreasing = TRUE)[1]))
```

```
## [1] "835"
```

So the maximum number of average steps appears during the No. 835 5-minute interval.

## Imputing missing values

1. Calculate and the total number of missing values in the dataset.


```r
TotalNAData <- sapply(RowData$steps, is.na)
print(sum(TotalNAData))
```

```
## [1] 2304
```

So there is 2304 NA values.

2. Filling in all of the missing values using the mean for that 5-minute interval, then create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
FilledData <- as.array(RowData$steps)
for (i in 1:length(FilledData)) {
  if (is.na(FilledData[i])) {
    FilledData <- replace(FilledData, i, AverageDailySteps[as.character(RowData$interval[i])])
  }
}
names(FilledData) <- RowData$interval
head(FilledData)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
setwd("Figure")
png("the total number of filled steps taken each day.png")
TotalStepPerDay_Filled <- tapply(FilledData, RowData$date, sum)
hist(TotalStepPerDay_Filled, main = "the total number of filled steps taken each day", xlab = "the total filled steps")
dev.off()
```

```
## png 
##   2
```


```r
Mean_Filled <- mean(TotalStepPerDay_Filled); 
print(Mean_Filled)
```

```
## [1] 10766.19
```


```r
Median_Filled <- median(TotalStepPerDay_Filled)
print(Median_Filled)
```

```
## [1] 10766.19
```

From the filled data above we could find that the mean and median values both higher than row data, and once the NA values were filled with the average steps of that 5-minute interval, the frequency of lower steps (which were 0~5000 steps) went down, and makes the whole picture of the daily steps more like a normal distribution.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
RowData$weekdays <- factor(weekdays(as.Date(RowData$date)), levels = c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日"), labels = c("weekdays", "weekdays", "weekdays", "weekdays", "weekdays", "weekends", "weekends"))
levels(RowData$weekdays)
```

```
## [1] "weekdays" "weekends"
```

2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
WeekdaySteps <- RowData[RowData$weekdays == "weekdays", ]
WeekendSteps <- RowData[RowData$weekdays == "weekends", ]

AverageWeekdaySteps <- tapply(WeekdaySteps$steps, WeekdaySteps$interval, mean, na.rm = TRUE)
AverageWeekendSteps <- tapply(WeekendSteps$steps, WeekendSteps$interval, mean, na.rm = TRUE)

setwd("Figure")
png("average weekdays and weekends steps.png")
par(mfrow = c(2, 1))
plot(names(AverageWeekdaySteps), AverageWeekdaySteps, type = "l", xlab = "Time Series of 5-minute Interval in Weekdays", ylab = "Average Steps")
plot(names(AverageWeekendSteps), AverageWeekendSteps, type = "l", xlab = "Time Series of 5-minute Interval in Weekends", ylab = "Average Steps")
dev.off()
```

```
## png 
##   2
```
