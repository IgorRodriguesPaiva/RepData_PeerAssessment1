---
title: "Reproducible Research - Project 1"
author: "Igor Paiva"
date: "11/12/2019"
output: html_document
---

1 - Code for reading in the dataset and/or processing the data:
===============================================================

First we download and save the data:
```{r , echo=TRUE}
setwd("C:/Users/igorp/Documents/Courses/DataScience Course - Coursera/05 - Reproducible Research/Week 2 - Project 1")
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
downloadFile <- "repdata_data_activity.zip"
activityFile <- "repdata_data_activity.csv"

if (!file.exists(activityFile)) {
    download.file(URL, downloadFile, method = "curl")
    unzip(downloadFile, overwrite = TRUE, exdir = "C:/Users/igorp/Documents/Courses/DataScience Course - Coursera/05 - Reproducible Research/Week 2 - Project 1")
}
```

Reading and saving data into a data frame:
```{r, echo=TRUE}
ActivityData <- read.csv(file = "activity.csv", header = TRUE, sep = ",")
```

2 - Histogram of the total number of steps taken each day:
==========================================================

Here I used the aggregate function to create a data frame with the total steps per day:
```{r}
StepsPerDay <- aggregate(steps ~ date, ActivityData, sum)
```

Now with the ggplot2 library, we create the histogram:
```{r, echo=TRUE}
library(ggplot2)
colnames(StepsPerDay) <- c("Date","TotalSteps")
ggplot(StepsPerDay, aes(x = Date, y = TotalSteps)) +
        geom_histogram(stat = "identity", fill = "#00AFBB") +
        ggtitle("Total Steps per Day") +
        xlab("Day") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

3 - Mean and median number of steps taken each day:
===================================================

Calculating the mean of the total number of steps taken per day:
```{r, echo=TRUE}
TotalStepsDay_mean <- mean(StepsPerDay$TotalSteps)
TotalStepsDay_mean
```

The mean of total number of steps taken per day is: `r format(TotalStepsDay_mean, digits = 2, nsmall = 2)`.


Calculating the median of the total number of steps taken per day:
```{r, echo=TRUE}
TotalStepsDay_median <- median(StepsPerDay$TotalSteps)
TotalStepsDay_median
```

The median of total number of steps taken per day is: `r format(TotalStepsDay_median, digits = 2, nsmall = 2)`.

4 - Time series plot of the average number of steps taken:
==========================================================

Here I used the aggregate function to create a data frame with the average steps per day: 
```{r}
StepsPerInterval_Average <- aggregate(steps ~ interval, ActivityData, mean)
colnames(StepsPerInterval_Average) <- c("Interval","AverageSteps")
```

Ploting the time series graph with ggplot2:
```{r, echo=TRUE}
h <- ggplot(StepsPerInterval_Average, mapping = aes(x = Interval, y = AverageSteps))
h +     geom_line(color = "steelblue") +
        geom_point(stroke = 0.5) +
        xlab("Intervals") +
        ggtitle("Average number of steps taken per Interval") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ylim(0,210)
```

5 - The 5-minute interval that, on average, contains the maximum number of steps:
=================================================================================

```{r, echo=TRUE}
MaxStepsInterval <- StepsPerInterval_Average$Interval[max(StepsPerInterval_Average$AverageSteps)]
MaxStepsInterval
```

The 5-minute interval that, on average, contains the maximum number of steps is :
`r MaxStepsInterval`

6 - Code to describe and show a strategy for imputing missing data:
===================================================================

```{r}
countNA <- sum(is.na(ActivityData$steps))
countNA
```

There is a total of `r countNA` NA values in the data.

Now we're going to fill all the missing values in the data set with the mean for that 5-minute interval.
```{r}
CleanActivityData = ActivityData
for(i in 1:length(ActivityData$steps)) {
        if (is.na(ActivityData$steps[i])){
                CleanActivityData$steps[i] <-                             StepsPerInterval_Average$AverageSteps[StepsPerInterval_Average$Interval == ActivityData$interval[i]]
        }
}

CleanActivityData$steps <- as.numeric(format(CleanActivityData$steps, digits = 2))
```

As we can see above, the values for steps that once was NA, now are filled with the average value for that interval.
```{r} 
head(CleanActivityData)
```

7 - Histogram of the total number of steps taken each day after missing values are imputed:
===============================================================================

First I used the aggregate function to sum the total steps that was taken in each day:
```{r, echo=TRUE}
CleanStepsPerDay_Sum <- aggregate(steps ~ date, CleanActivityData, sum)
```

Ploting the histogram with ggplot2:
```{r, echo=TRUE}
library(ggplot2)
ggplot(CleanStepsPerDay_Sum, aes(x = date, y = steps)) +
        geom_histogram(stat = "identity", fill = "#00AFBB") +
        ggtitle("Total Steps per Day") +
        xlab("Day") +
        ylab("Total Steps") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

8 - Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
================================================================================

In ordert to get the weekdays in english we need to set the Sys.setlocale:
```{r}
Sys.setlocale("LC_TIME", "C")
```

Transforming the class of the field date from factor to the date format.
```{r, echo=TRUE}
CleanActivityData$date <- as.Date(CleanActivityData$date, format = "%Y-%m-%d")
```

Creating a new column witch indicates if the day is a weekday or a weekend day.
```{r, echo=TRUE}
ActivityWeekday <- weekdays(CleanActivityData$date)
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
for (i in 1:length(CleanActivityData$date)){
        if(ActivityWeekday[i] %in% weekday){
                ActivityWeekday[i] <- "weekday"
        }
        else{
              ActivityWeekday[i] <- "weekend"  
        }
}
```

Merging the weekday column in the CleanActivityData data frame.
```{r, echo=TRUE}
ActivityWeekDay <- cbind(CleanActivityData, ActivityWeekday)
head(ActivityWeekDay)
```

Here we can see a table indicating the total days in our data that are weekdays or weekend days.
```{r, echo=TRUE}
table(ActivityWeekday)
```

```{r, echo=TRUE}
StepsPerIntervalPerWeekday_Average <- aggregate(steps ~ interval + ActivityWeekday, ActivityWeekDay, mean)

require(lattice)
with(StepsPerIntervalPerWeekday_Average, 
     xyplot(steps ~ interval | ActivityWeekday, type = "l", 
            xlab = "Interval", ylab = "Number of Steps", layout = c(1,2)))
```
