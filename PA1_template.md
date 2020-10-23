---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data

```r
#Unzip the activity data
unzip("activity.zip")
#Load packages
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
#Load the .csv activity data
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#Change date class into Date
activity$date <- as.Date(as.character(activity$date), format = "%Y-%m-%d")
#Change steps clas into numeric
activity$steps <- as.numeric(activity$steps)
```

## What is mean total number of steps taken per day?

```r
#Histogram of the total number of steps taken each day
StepsByDay <- activity %>% 
  group_by(date) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByDay, aes(x = total)) +
  geom_histogram(bins = 25) +
  xlab("Number of steps taken each day") +
  ylab("Frequency") +
  labs(title = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Mean and median number of steps taken each day
MeanStepsByDay <- StepsByDay$total %>% mean(na.rm = TRUE)
MeanStepsByDay
```

```
## [1] 9354.23
```

```r
MedianStepsByDay <- StepsByDay$total %>% median(na.rm = TRUE)
MedianStepsByDay
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
#Time series plot of the average number of steps taken
StepsByInterval <- activity %>% 
  group_by(interval) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByInterval, aes(x = interval, y = total)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average Daily Steps Taken each interval") +
  labs(title = "Average Number of Steps Taken in a interval each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#The 5-minute interval that, on average, contains the maximum number of steps
StepsByInterval$interval[which.max(StepsByInterval$total)]
```

```
## [1] 835
```

## Imputing missing values

```r
#Number of NA rows
MissingRows <- activity$steps %>% is.na() %>% sum

#Imputing the missing steps value using the mean value by interval
library(plyr)
```

```
## ------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity1 <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
detach("package:plyr", unload = TRUE)

#Histogram of the total number of steps taken each day
library(dplyr)
StepsByDay1 <- activity1 %>% 
  group_by(date) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByDay1, aes(x = total)) +
  geom_histogram(bins = 25) +
  xlab("Number of steps taken each day") +
  ylab("Frequency") +
  labs(title = "Histogram of the total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?


```r
activity1$day <- weekdays(activity1$date)
#Define a character vector for the weekdays in italian, my system language
weekdays <- c("Lunedì", "Martedì", "Mercoledì", "Giovedì", "Venerdì")
for (i in 1:dim(activity)[1]) {
  if(activity1$day[i] %in% weekdays) activity1$weekday[i] <- "weekday"
  else activity1$weekday[i] <- "weekend"
} 
activity1$weekday <- factor(activity1$weekday)

#Time series plot of the average number of steps taken in each interval of a weekday
library(lattice)
StepsByIntervalWeekdayWeekend <- activity1 %>% 
  group_by(weekday, interval) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
xyplot(total ~ interval | weekday,
       data = StepsByIntervalWeekdayWeekend,
       type = "l",
       layout = c(1,2),
       ylab = "Number of steps",
       xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

