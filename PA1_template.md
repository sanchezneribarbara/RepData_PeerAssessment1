---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
```{r}
personalActivity = read.csv("activity.csv", header = TRUE, sep =",")
```
mean total of steps taken per day, ignore NA's

calculate total of steps per day
```{r}
totalSteps <- tapply(personalActivity$steps, personalActivity$date, FUN=sum, na.rm=TRUE)
```

Histogram of number of steps per day
```{r}
library(ggplot2)
qplot(totalSteps, binwidth=1000, xlab="Total number of steps")

echo=TRUE
```
Calculate mean and  median of the total number of steps taken per day
```{r}
mean (totalSteps,na.rm=TRUE)
median(totalSteps,na.rm=TRUE)
echo=TRUE
```

## What is the average daily activity pattern?
```{r}
interval <-aggregate(personalActivity$steps, by=list(personalActivity$interval), data=personalActivity, FUN='mean', na.rm=TRUE)

p <- ggplot(data=interval, aes(x = interval$Group.1, y = interval$x))
p <- p + geom_line() + labs(title = "Average daily activity pattern", x="Interval",y="Number of Steps")
p
echo = TRUE
```
which 5 minute interval  max number of steps
```{r}
maxSteps <- aggregate(interval$Group.1, by=list(interval$Group.1), FUN=max)
maxStep <- max(interval)
echo = TRUE
```

#imputing missing values

Calculate and report missing NA values
```{r}
missingValues = sum(is.na(personalActivity))
```
##create a strategy to fill in missing values
###Since there are days with no data for all the intervals I will use the median obtained for each of the intervals for the missing days

create new dataset with the missing values all filled in
```{r}
df1 = transform(personalActivity, steps = ifelse(is.na(steps), median(steps, na.rm=TRUE), steps))
echo = TRUE
```

Histogram of number of steps per day with no missing values
```{r}
totalStepsNo <- tapply(df1$steps, df1$date, FUN=sum, na.rm=TRUE)
library(ggplot2)
qplot(totalStepsNo, binwidth=1000, xlab="Total number of steps")
echo = TRUE
```
Calculate mean and  median of the total number of steps taken per day
```{r}
mean (totalStepsNo,na.rm=TRUE)
median(totalStepsNo, na.rm=TRUE)
echo =TRUE
```

## Are there differences in activity patterns between weekdays and weekends?
```{r}
library(timeDate)

df1$day <- as.factor(isWeekend(df1$date))

levels(df1$day) <- list(weekday="FALSE", weekend="TRUE")

echo = TRUE
```
make a panel plot 
```{r}

library(lattice)

xyplot(df1$steps~interval | day, data = df1, type = 'l',xlab = 'Interval',ylab = 'Number of Steps', layout = c(1,2))
```


















