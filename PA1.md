# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data


The variables included in this dataset are:  

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken


The data is loaded from activity.csv and stored in activity. 


```r
activity <- read.csv("activity.csv") 
```

## What is mean total number of steps taken per day?


###1. The total number of steps taken per day###

The total number of steps taken each day is calculated using the code below. The steps taken on each day in the data, where a valid reading is recorded (not NA) are summed and recorded in days.Steps. 


```r
library(plyr)
# Sum steps for each day where the value is recorded for the interval
days.Steps <- ddply(subset(activity[c("date","steps")], !(is.na(steps)) ), "date", colwise(sum))
```

The following cade prints the table containing the calculated steps taken on each day (contents of days.Steps). The output shows the date and the total steps taken on that date.   


```r
library(knitr)
kable(days.Steps[,1:2], align="c")
```

    date       steps 
------------  -------
 2012-10-02     126  
 2012-10-03    11352 
 2012-10-04    12116 
 2012-10-05    13294 
 2012-10-06    15420 
 2012-10-07    11015 
 2012-10-09    12811 
 2012-10-10    9900  
 2012-10-11    10304 
 2012-10-12    17382 
 2012-10-13    12426 
 2012-10-14    15098 
 2012-10-15    10139 
 2012-10-16    15084 
 2012-10-17    13452 
 2012-10-18    10056 
 2012-10-19    11829 
 2012-10-20    10395 
 2012-10-21    8821  
 2012-10-22    13460 
 2012-10-23    8918  
 2012-10-24    8355  
 2012-10-25    2492  
 2012-10-26    6778  
 2012-10-27    10119 
 2012-10-28    11458 
 2012-10-29    5018  
 2012-10-30    9819  
 2012-10-31    15414 
 2012-11-02    10600 
 2012-11-03    10571 
 2012-11-05    10439 
 2012-11-06    8334  
 2012-11-07    12883 
 2012-11-08    3219  
 2012-11-11    12608 
 2012-11-12    10765 
 2012-11-13    7336  
 2012-11-15     41   
 2012-11-16    5441  
 2012-11-17    14339 
 2012-11-18    15110 
 2012-11-19    8841  
 2012-11-20    4472  
 2012-11-21    12787 
 2012-11-22    20427 
 2012-11-23    21194 
 2012-11-24    14478 
 2012-11-25    11834 
 2012-11-26    11162 
 2012-11-27    13646 
 2012-11-28    10183 
 2012-11-29    7047  
   
###2. The data in days.Steps is used to create a histogram of the total number of steps taken each day ###   

The code below breaks the range of steps in to 5 and plots a histogram of the frequency of each of the 5 range steps in terms of days.


```r
hist(days.Steps$steps, breaks=5, main="Steps Taken Each Day", xlab = "Steps taken", ylab="Number of days (frequency)")
```

![](PA1_files/figure-html/plotstepshist-1.png) 


###3. The mean and median of the total number of step taken per day (in days.Steps) are then calcuated###

The code below calculates the mean and median steps per day.   
**Note**: where no measurements were available in the activity data for an entire day, the day is excluded from the calculation of the mean and median.   


```r
mean.Steps<-apply(days.Steps[2], 2, mean)
median.Steps<-apply(days.Steps[2], 2, median)
```


Using the calculated mean and median values above: 

- The **mean** total number of steps taken per day (mean.Steps) is calculated to be **10766.1886792**.   
- The **median** total number of steps taken per day (median.Steps) is calculated to be **10765**.   

## What is the average daily activity pattern?

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

To answer this question, first the maximum steps on each day are calculated from the activity data.


```r
days.Max <- ddply(subset(activity, !(is.na(steps)) ), "date", colwise(max,"steps"))
```

Using the days.Max derived above, the following code selects the the rows from the activity data where the steps are equal to the maximam reading of steps on that day. This gives all the rows, (date, steps, interval) that contain the maximum steps in an interval on a given day. *Note*: there may be multiple intervals on the same day that have the same maximum steps.   


```r
intervals.Max <- data.frame()
for ( i in  1:nrow(days.Max)) {
    index.day<-toString(days.Max[i,1])
    index.steps<-days.Max[i,2]
    current.row <- subset(activity, date == index.day & steps == index.steps)
    intervals.Max <- rbind(intervals.Max,current.row)
}
```

Given the intervals.Max we can work out a median interval in this set, which will be the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps.   

The following code works out the median of intervals:  


```r
median.ActiveInterval<-median(as.vector(intervals.Max[,3]))
```

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is the **median** of most active intervals (median.ActiveInterval) and is calculated to be  **1025**.   

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
