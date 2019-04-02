# Reproducible Research: Peer Assessment 1

In the following report we will present analysis of the activity monitoring data made available for this assignment (dataset available [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)).

The code chunks are interspersed with the explanations.

## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

The csv is stored as *activity.csv* in the working directory.


```r
d <- read.csv("activity.csv")
```



2. **(Optional)** Process/transform the data (if necessary) into a format suitable for your analysis

No pre-processing is done initially. Processing is done only on a case-by-case basis as needed. 

## What is mean total number of steps taken per day?

For this part of the assignment, we ignore the missing values in the dataset.

1. Make a histogram of the total number of steps taken each day

We first find the daily sums to generate the histogram:

```r
sums_d <- tapply(d$steps, d$date, sum)
```


Following is the visualization:

```r
hist(sums_d, xlab = "Total number of steps per day", xlim = c(1, 25000), ylim = c(1, 
    30), breaks = 10, col = "grey", main = "Histogram: Original Data")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


2. Calculate and report the mean and median total number of steps taken per day

```r
mean_d <- mean(sums_d, na.rm = T)
median_d <- median(sums_d, na.rm = T)
```


Therefore, the mean and median total number of steps taken per day are 1.0766 &times; 10<sup>4</sup> and 10765, respectively.


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

We derive the interval averages:

```r
ints <- tapply(d$steps, d$interval, function(x) {
    mean(x, na.rm = T)
})
```


The we  use R's built-in ts function to generate the time series graph:

```r
ts.plot(ts(ints), xlab = "Intervals", ylab = "# of Steps", main = "Average Daily Activity Pattern", 
    type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

We simply find the max value of the *ints* array.

```r
which.max(ints)
```

```
## 835 
## 104
```


Therefore, 104 is the maximum number of steps, occurring at the 5-minute starting at the 835th minute. 


## Imputing missing values

There are a number of days/intervals in the original dataset where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(d$steps))
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

For simplicity, we set the missing values to zero. Clearly, this will lead to lower mean estimates that if we had replaced the NAs with interval means, or daily means, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

We create a new dataset *e*:


```r
e <- d
e$steps[is.na(e$steps)] <- 0
```


4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# get statistics by date for new date
sums_e <- tapply(e$steps, e$date, sum)
mean_e <- mean(sums_e, na.rm = T)
median_e <- median(sums_e, na.rm = T)

# plot histogram new data
hist(sums_e, xlab = "Total number of steps per day", xlim = c(1, 25000), ylim = c(1, 
    30), breaks = 10, col = "grey", main = "Histogram: Adjusted Data")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


The mean and median total number of steps taken per day for this new dataset are 9354.2295 and 1.0395 &times; 10<sup>4</sup>, respectively. As expected, the mean is 1411.9592 lower, and the median is also 370 lower. This reflects the skewing effect the additional 0's have on the overall dataset.

## Are there differences in activity patterns between weekdays and weekends?






```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
wd <- e$date
e <- cbind(e, wd)
e$wd <- weekdays(as.Date(wd))
e[(e$wd == "Sunday") | (e$wd == "Saturday"), 4] <- "weekend"
e[(e$wd != "weekend"), 4] <- "weekday"

w1 <- tapply(e[(e$wd == "weekday"), 1], e[(e$wd == "weekday"), 3], mean)
w2 <- tapply(e[(e$wd == "weekend"), 1], e[(e$wd == "weekend"), 3], mean)

w1_df <- as.data.frame(as.vector(w1))
w1_df <- cbind(w1_df, e[1:288, 3])
w1_df <- cbind(w1_df, rep("weekday", each = 288))
colnames(w1_df) <- c("steps", "interval", "wd")

w2_df <- as.data.frame(as.vector(w2))
w2_df <- cbind(w2_df, e[1:288, 3])
w2_df <- cbind(w2_df, rep("weekend", each = 288))
colnames(w2_df) <- c("steps", "interval", "wd")

lat <- rbind(w1_df, w2_df)

library(lattice)
xyplot(steps ~ interval | wd, lat, main = "Activity patterns: weekdays vs. weekends", 
    xlab = "Interval", ylab = "Number of steps", layout = c(1, 2), type = "l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


Clearly, there are noticeable differences in the weekly patterns. E.g. on weekdays there is only 1 spike in the morning that stands out, while there are a number of similarly tall spikes throughout the middle of the day on weekends. However, while the weekend spikes occur more consistently (all between ca. 100-150 steps), they are still shorter than the mid-morning weekday spike (ca. 200 steps).


