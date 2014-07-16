# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day. The data
consists of two months of data from an anonymous individual collected during
the months of October and November, 2012 and include the number of steps
taken in 5 minute intervals each day.

## Loading and preprocessing the data
The data file (activity.zip) was unzipped into the default directory.  The data file (activity.csv) was loaded with read.csv with no parameters.  

summary() revieled 2304 NAs.  

str() shows the dataset includes the following columns:  steps (int), date (Factor) and interval(int). 

For immediate analysis, NAs were removed, creating activeComplete. A day of the week was derived from the date using weekdays() to facilitate additional subsetting and analysis.



```r
setwd("C:\\Users\\srobin\\Documents\\GitHub\\RepData_PeerAssessment1")
unzip(paste0(getwd(),"\\","activity.zip"))
active <- read.csv(paste0(getwd(),"\\","activity.csv"))

summary(active)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
str(active)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activeComplete <- active[complete.cases(active),]
activeComplete$dow <- weekdays(as.Date(activeComplete$date))
```


## What is mean total number of steps taken per day?

Complete cases was aggregated by date to derive 53 days of observations and summed for each day.  A histogram shows the frequency of the steps per day was around 10,000, closely matching a summary of this data, with a mean of 10,766 and a median of 10,765.

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 





### Mean and Median  
- Mean: 1.0766 &times; 10<sup>4</sup>
- Median:  10765

## What is the average daily activity pattern?
Is there a time series plot of the average number of steps taken (averaged across all days) versus the 5-minute intervals?


```r
  daily <- activeComplete[,c(1,3)]
  pattern <- aggregate(.~ interval,data=daily, mean, na.rm=TRUE)
  plot(pattern$interval,pattern$steps, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Does the report give the 5-minute interval that, on average, contains the maximum number of steps?


```r
  maxSteps <- pattern[pattern$steps > 200,]
  maxSteps
```

```
##     interval steps
## 104      835 206.2
```

The average daily activity show increasing activity at Interval 500 (5:00am) and a sharp spike around Interval 800 (8:00am).  Maximum activity occurs at Interval 835.  Activity fluxuates across the day, until around Interval 1900, where it declines quickly and tappers off to 0 around Interval 2400.


## Inputing missing values

Where data was missing steps (NA), steps were estimated by using the mean of the corresponding interval and day of the week of records that had complete data.  A dataset of complete cases was created, along with Weekdays() function to create a 'Day-of-the-Week'column. 

The dataset was grouped by interval + dow, creating a mean of each interval by day. Missing data is replaced day by day through a loop of missing dates, by day of week.



The introduction of Replaced data has slightly raised the mean,   
- from 37.3826  
- to 37.5736

## Are there differences in activity patterns between weekdays and weekends?



```r
## histogram of the total number of steps taken each day after missing values were 
## imputed
perDay_after <- replaceMissing[,1:2]
sum_steps_per_Day_after <- aggregate(.~ date, data=perDay_after,sum)

hist(sum_steps_per_Day_after$steps, plot=TRUE, main="Histogram of Total Steps\n each Day After Missing Values are Input")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
# create a factor for Weekend - Weekend or Weekday
#activeComplete$weekend <- factor(ifelse(activeComplete$dow %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

#dayofweek <- aggregate(activeComplete$steps, 
#                       by=list(inter=activeComplete$interval, 
#                               we=activeComplete$weekend),
#                       mean)

# arrange by day of the week starting Monday
#dayofweek$day <- factor(dayofweek$Group.1, levels= c( "Monday", 
#    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
#dayofweek[order(dayofweek$day), ]


## panel plot comparing the average number of steps taken per 
## 5-minute interval across weekdays and weekends

#library(ggplot2)
#ggplot(dayofweek, aes(day,steps)) + geom_point()
```

