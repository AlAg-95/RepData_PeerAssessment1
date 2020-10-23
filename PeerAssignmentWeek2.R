#Loading and preprocessing the data
#Unzip the activity data
unzip("activity.zip")
#Load packages
library(dplyr)
library(ggplot2)
#Load the .csv activity data
activity <- read.csv("activity.csv")
str(activity)
#Change date class into Date
activity$date <- as.Date(as.character(activity$date), format = "%Y-%m-%d")
activity$steps <- as.numeric(activity$steps)

#What is mean total number of steps taken per day?
#Histogram of the total number of steps taken each day
StepsByDay <- activity %>% 
  group_by(date) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByDay, aes(x = total)) +
  geom_histogram(bins = 25) +
  xlab("Number of steps taken each day") +
  ylab("Frequency") +
  labs(title = "Histogram of the total number of steps taken each day")

#Mean and median number of steps taken each day
MeanStepsByDay <- StepsByDay$total %>% mean(na.rm = TRUE)
MedianStepsByDay <- StepsByDay$total %>% median(na.rm = TRUE)

#What is the average daily activity pattern?
#Time series plot of the average number of steps taken
StepsByInterval <- activity %>% 
  group_by(interval) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByInterval, aes(x = interval, y = total)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average Daily Steps Taken each interval") +
  labs(title = "Average Number of Steps Taken in a interval each day")

#The 5-minute interval that, on average, contains the maximum number of steps
StepsByInterval$interval[which.max(StepsByInterval$total)]

#Imputing missing values
#Number of NA rows
MissingRows <- activity$steps %>% is.na() %>% sum

#Imputing the missing steps value using the mean value by interval
library(plyr)
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

#Are there differences in activity patterns between weekdays and weekends?
activity1$day <- weekdays(activity1$date)
weekdays <- c("Lunedì", "Martedì", "Mercoledì", "Giovedì", "Venerdì")
weekenddays <- c("Sabato", "Domenica")
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



xyplot(pm25 ~ latitude | region, data = pollution)
ggplot(data = StepsByIntervalWeekday, aes(x = interval, y = total)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average Daily Steps Taken each interval") +
  labs(title = "Average Number of Steps Taken in each interval of a weekday")

#Time series plot of the average number of steps taken in each interval of a weekday
StepsByIntervalWeekend <- activity1 %>% 
  filter(weekday == "weekend") %>% 
  group_by(interval) %>% 
  summarise(total = sum(steps, na.rm = TRUE))
ggplot(data = StepsByIntervalWeekend, aes(x = interval, y = total)) +
  geom_line() +
  xlab("Interval") +
  ylab("Average Daily Steps Taken each interval") +
  labs(title = "Average Number of Steps Taken in each interval of a weekend")
