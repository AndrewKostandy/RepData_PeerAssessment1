---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html document: PA1_template.html
    keep_md: true
---


## Loading and preprocessing the data

Let's start by reading the data first.  
The raw data file activity.csv should be in your working directory.


```r
thedata<-read.csv("activity.csv")
```

Then I'll convert the date column into a variable of type date

```r
thedata$date<-as.Date(thedata$date,format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

First, I'll calculate the total number of steps taken per day


```r
daysum<-aggregate(steps ~ date, data=thedata, FUN=sum)
```

Then, I'll plot a histogram showing the total number of steps taken each day.


```r
## Draws the plot
library(ggplot2)
plot1<-qplot(date, data=daysum, geom="histogram", binwidth = 1,
      weight=steps, fill="red", main="Total Number of Steps by Date",
      xlab="Date", ylab = "Total Number os Steps")
plot1<-plot1+theme(legend.position = "none")
print(plot1)
```

![plot of chunk Plot1](figure/Plot1-1.png) 

Then, I'll calculate and report the mean and median of the total number of steps taken per day


```r
datamean<-mean(daysum$steps) ## Calculates the Mean
datamedian<-median(daysum$steps) ## Calculates the Median
```

- The mean total number of steps taken per day is **10766.19**.  
- The median total number of steps taken per day is **10765**.

## What is the average daily activity pattern?

First, I'll calculate the mean of steps by interval and plot it with the 5-minute interval on the x-axis and the average number of steps taken, averaged across all days on the y-axis.


```r
## Calculating Mean of Steps by Interval
intervalmeans<-aggregate(steps ~ interval, data=thedata, FUN=mean)
## Draws the plot
library(lattice)
plot2<-xyplot (steps ~ interval, data=intervalmeans, type="l",
               main="Average Number of Steps Taken in 5-minute Intervals \nAveraged Across All Days",
               xlab="Interval", ylab="Number of Steps")
print(plot2)
```

![plot of chunk plot2](figure/plot2-1.png) 

Then, I'll find out the 5 minute interval with the maximum number of steps on average across all days.


```r
## Gives the Interval with the highest Avg Number of Steps
highestinterval<-intervalmeans[which.max(intervalmeans$steps),1]
## Gives the Highest Average Number of Steps in a 5-minute Interval
highestnumsteps<-intervalmeans[which.max(intervalmeans$steps),2]
```

The interval with the maximum number of steps on average across all days was **835** and the average number of steps in that interval was about **206.17**.

## Imputing missing values

I'll start by calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missingvalues<-sum(is.na(thedata$steps))
```
The total number of missing values in the dataset is **2304**.

As a strategy to fill in all of the missing values, I have decided to take the mean for a 5 minute interval to fill in the Missing Values in that 5 minute interval. This in my view, would be a reasonable expectaion of what the data would have looked like had it not been missing.

Then, I'll create a new dataset that is equal to the original dataset and based on my strategy, I'll use the interval means data generated earlier to fill in the missing values based on those interval means.


```r
imputeddata<-thedata
for (i in 1:nrow(imputeddata)){
        if (is.na(imputeddata[i,1])){
                imputeddata[i,1]<-intervalmeans$steps[which(intervalmeans$interval==imputeddata[i,3])]
        }
}
```

Now, I'll plot a histogram of the total number of steps taken each day after imputing the missing values.


```r
impdaysum<-aggregate(steps ~ date, data=imputeddata, FUN=sum) ## Calculating the sum of steps by date
## Draws the plot
plot3<-qplot(date, data=impdaysum, geom="histogram", binwidth = 1,
      weight=steps, fill="red", main="Total Number of Steps by Date",
      xlab="Date", ylab = "Total Number os Steps")
plot3<-plot3+theme(legend.position = "none")
print(plot3)
```

![plot of chunk plot3](figure/plot3-1.png) 

Then, I'll calculate the mean and median total number of steps taken per day after imputing the missing values.


```r
impdatamean<-mean(impdaysum$steps) ## Calculating Mean
impdatamedian<-median(impdaysum$steps) ## Calculating Median
```

**After** imputing the missing data values:
- The mean total number of steps taken per day is about **10766.19**
- The median total number of steps taken per day is about **10766.19**

**Before** imputing the missing data values:
- The mean total number of steps taken per day was about **10766.19**
- The median total number of steps taken per day was **10765**

As you can see, the mean value did not change at all after imputing the missing values and the median value has only slightly increased.

This means that the impact of imputing missing values on the estimates and central tendencies of the total daily number of steps was small.

## Are there differences in activity patterns between weekdays and weekends?

First, I'll create a new variable and set it to weekday or weekend appropriately and make it a factor variable


```r
imputeddata["daytype"] <- NA ## creating a new variable in the dataset
## Going through the rows and setting the variable to weekday or weekend as per the date
for (i in 1:nrow(imputeddata)){
        if (weekdays(imputeddata[i,2])=="Saturday" || weekdays(imputeddata[i,2])=="Sunday"){
                imputeddata[i,4]<-"weekend"
        }
        else{imputeddata[i,4]<-"weekday" }
}
imputeddata$daytype<-as.factor(imputeddata$daytype) ## Making daytype a factor as instructed
```

Then, I'll make a panel plot containing a time series plot (of type="l") of the 5-minute interval on the x-axis and the average number of steps taken, averaged across all weekdays or weekend days on the y-axis and see if there is anything interesting.


```r
## Calculating the mean of steps by interval and day type
impintervalmeans<-aggregate(steps ~ interval + daytype, data=imputeddata, FUN=mean) 
## Draws the plot
plot4<-xyplot (steps ~ interval | daytype, data=impintervalmeans, type="l", layout=c(1,2),
        main="Average Number of Steps Taken in 5-minute Intervals \nAveraged Across All Days by Day Type",
        xlab="Interval", ylab="Number of Steps")
print(plot4)
```

![plot of chunk plot4](figure/plot4-1.png) 

I have seen differences in activity patterns between weekdays and weekends and below is a list of my observations.

**Observations:**

- On weekdays the number of steps appear to start increasing earlier in the day compared to weekend days.  
- On weekdays there appears to be a large peak in the number of steps between the intervals 750 and 1000. It is quite higher than the same interval on weekend days.  
- Between intervals 1000 and 1750 there seems to be higher averages of steps on weekends compared to weekdays.
- Towards the end of the day, the averages of the number of steps at the weekend spike at around interval 2000 and then start 
their gradual final decline. On weekdays however, the spike happens at an earlier interval of the day and then start their gradual final decline.
