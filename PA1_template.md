---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## 1. Loading and preprocessing the data
### Load the library

```r
library(ggplot2)
library(dplyr)
```

### Read dataset and process data

```r
if (!file.exists("activity.csv")) {
	unzip("repdata_data_activity.zip")
}
data <- read.csv("activity.csv")
```

### Basic information

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
dim(data)
```

```
## [1] 17568     3
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(data)
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

## 2. What is mean total number of steps taken per day?
### Total number of steps taken per day

```r
total_steps <- data %>%
    group_by(date) %>%
    summarize(steps_by_date = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

### A histogram of the total number of steps taken each day

```r
g2 <- ggplot(total_steps, aes(x = steps_by_date))
g2 <- g2 + geom_histogram(fill = "green", binwidth = 1000) + xlab("Total steps taken each day") + ylab("Frequency") + ggtitle("The total number of steps taken each day")
print(g2)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png, file = "plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### Mean and median of the total number of steps taken per day

```r
mean <- mean(total_steps$steps_by_date)
median <- median(total_steps$steps_by_date)
```
#### Mean and median of the total number of steps taken per day are 9354.2295082 and 10395, respectively.

## 3. What is the average daily activity pattern?
### Time series plot (5-minute interval)

```r
intervals_5min <- data %>%
    group_by(interval) %>%
    summarise(steps_by_5min = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
g3 <- ggplot(intervals_5min, aes(x = interval, y = steps_by_5min))
g3 <- g3 + geom_line(color = "blue") + xlab("Intervals every 5 min") + ylab("Average steps taken") + ggtitle("Time series plot of average steps taken for every 5 min interval")
print(g3)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
dev.copy(png, file = "plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### Which 5-minute interval contains the maximum number of steps?

```r
max_interval <- intervals_5min$interval[which.max(intervals_5min$steps_by_5min)]
```
#### The 835th interval contains the maximum number of steps

## 4. Imputing missing values
### Total number of missing values in the dataset (rows)

```r
num_missing <- sum(is.na(data$steps))
```
#### 2304 rows of data are missing.

### Filling in all of the missing values in the dataset
#### Filling the missing value using the mean for that 5-minute interval
### A new dataset (dataNoNA) with filled missing data

```r
dataNoNA <- data.frame(data)
dataNoNA$steps_by_5min <- intervals_5min$steps_by_5min
dataNoNA[is.na(dataNoNA$steps),]$steps <- dataNoNA[is.na(dataNoNA$steps),]$steps_by_5min
```

### Total number of steps taken per day using filled in missing data dataset (dataNoNA) and the histogram

```r
total_steps_noNA <- dataNoNA %>%
    group_by(date) %>%
    summarize(steps_by_date = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
g4 <- ggplot(total_steps_noNA, aes(x = steps_by_date))
g4 <- g4 + geom_histogram(fill = "red", binwidth = 1000) + xlab("Total steps taken each day") + ylab("Frequency") + ggtitle("The total number of steps taken each day, with filled in missing data")
print(g4)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
dev.copy(png, file = "plot3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```

### Mean and median of the total number of steps taken per day, with the new dataset (dataNoNA)

```r
mean_noNA <- mean(total_steps_noNA$steps_by_date)
median_noNA <- median(total_steps_noNA$steps_by_date)
```
#### Mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 1.0766189\times 10^{4}, respectively.
#### For the previous dataset, with removed missing data, the mean and median of the total number of steps taken per day are 9354.2295082 and 10395, respectively.
#### And the differences between the mean and median are 1411.959171 and 371.1886792, respectively.

## 5. Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels indicating whether a given date is a weekday or weekend day.

```r
dataNoNA$date <- as.Date(strptime(dataNoNA$date, format="%Y-%m-%d"))
dataNoNA$day <- weekdays(dataNoNA$date)
dataNoNA[dataNoNA$day %in% c("Saturday", "Sunday"),]$day <- "weekend"
dataNoNA[dataNoNA$day != "weekend",]$day <- "weekday"
dataNoNA$day <- as.factor(dataNoNA$day)
```

### Time series plot (5-minute interval) of weekday and weekend

```r
intervals_5min_wdwk <- dataNoNA %>%
    select(-date) %>%
    group_by(interval, day) %>%
    summarise(steps_by_5min = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
g5 <- ggplot(intervals_5min_wdwk, aes(x = interval, y = steps_by_5min))
g5 <- g5 + geom_line(color = "blue") + facet_wrap(~day, nrow = 2) + xlab("Intervals every 5 min") + ylab("Average steps taken") + ggtitle("Time series plot of average steps taken for every 5 min interval")
print(g5)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
dev.copy(png, file = "plot4.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
