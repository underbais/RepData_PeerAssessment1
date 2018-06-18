---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
# getting data
zipfile = 'activity.zip'
file = 'activity.csv'


if (!file.exists(zipfile)) {
  
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  dat <- read.csv(unz(temp, file), header = TRUE, sep = ',', stringsAsFactors = FALSE)
  unlink(temp)

} else {
  dat <- read.csv(unz(zipfile,file), header = TRUE, sep = ',', stringsAsFactors = FALSE, na.strings = 'NA')
}
# converting to date type
dat$date <- as.Date(dat$date)
```

```
## Warning in strptime(xx, f <- "%Y-%m-%d", tz = "GMT"): unknown timezone
## 'zone/tz/2018c.1.0/zoneinfo/America/New_York'
```


## What is mean total number of steps taken per day?



```r
library(ggplot2)
# aggregating total steps
steps <- aggregate(steps ~ date, dat, sum)

qplot(steps, data = steps, geom = 'histogram', binwidth = 1000, main = 'Histogram of total number of steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#Mean total number of step taken per day

```r
mean(steps$steps)
```

```
## [1] 10766.19
```
#Median total number of step taken per day


```r
median(steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# mean steps per interval
intervals <- aggregate(steps ~ interval, dat, mean)
# plotting steps per interval
ggplot(intervals, aes(interval,steps)) + 
  geom_line() + xlab('Interval') + 
  ylab('Steps') + 
  ggtitle('Time series plot of average steps taken per 5 minute interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervals$interval[which.max(intervals$steps)]
```

```
## [1] 835
```



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# total number of missing values
sum(is.na(dat))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
# imputing values based on average steps for every interval strategy
dat$date<- as.Date(dat$date)
dat.noNA <- dat[!is.na(dat$steps),]
dat.NA <- dat[is.na(dat$steps),]
to.impute <- merge(dat.NA,intervals, by='interval')
to.impute <- to.impute[,c(4,3,1)]
colnames(to.impute)[1] <- 'steps'
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# binding imputed data back to data with no NAs
imputed.dat <- rbind(dat.noNA,to.impute)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# first aggragating imputed data to get total steps per day
steps.imputed <- aggregate(steps ~ date, imputed.dat, sum)
mean(steps.imputed$steps)
```

```
## [1] 10766.19
```

```r
median(steps.imputed$steps)
```

```
## [1] 10766.19
```


```r
# plotting original data vs imputed data
hist(steps.imputed$steps, breaks=5, xlab="steps per day", main = "Steps with imputed values", col="blue")
hist(steps$steps, breaks=5, xlab="steps per day", main = "Steps with NAs", col="red", add=T)
legend("topright", c("Imputed", "Original"), fill=c("blue", "red") )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# adding new factor variable
imputed.dat$wd <- as.factor(ifelse(weekdays(imputed.dat$date) %in% c('Saturday','Sunday'), 'Weekend','Weekday'))
```

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
# aggregating data
imputed.dat.wd <- aggregate(steps ~ interval + wd, imputed.dat, mean)
# plotting lattice plot
xyplot(steps~interval|wd, data=imputed.dat.wd, type="l",  layout = c(1,2),
       main="Steps per Interval by type of day", 
       ylab="Steps", xlab="interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
