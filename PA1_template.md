# PA1_template
Carl Sutton  
April 10, 2016  

#  Reproducible Research Course Project 1 


The first thing is to read the data into R as shown below


```r
#  Preliminary exploratory work
df1 <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
```
The next item is to explore the data to see what it contains


```r
#  do a bit of data exploring
summary(df1); str(df1)
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

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(df1)
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

```r
anyNA(df1$steps); anyNA(df1$date); anyNA(df1$Interval)  #  only steps contains NA's
```

```
## [1] TRUE
```

```
## [1] FALSE
```

```
## [1] FALSE
```

```r
df1$date <- as.Date(df1$date)  #  to allow use of weekdays function
class(df1$date)  #verify it worked
```

```
## [1] "Date"
```

The following code contains some verification steps to ascertain of the code actually worked as intended.

```r
# Verify later calculation 
df2 <- subset(df1,date == "2012-10-02") # to check calculations performed later
sum(df2$steps)  #  verified above, no NA in dates or intervals
```

```
## [1] 126
```

```r
mean(df2$steps)
```

```
## [1] 0.4375
```

```r
median(df2$steps)
```

```
## [1] 0
```
At this time it is important to ascertain the amount of missing data.  That number is included in the data 
exploration shown above, and is also calculated here.


```r
#  determine number of rows with NA's, it is also printed in the summary above
df3 <- na.omit(df1)  # all NA rows omitted
dim(df1); dim(df3)  #  how many NA's?  2,304
```

```
## [1] 17568     3
```

```
## [1] 15264     3
```

```r
numOfNA <- nrow(df1) - nrow(df3)
numOfNA
```

```
## [1] 2304
```

Questionn 1.Calculate the total number of steps taken per day is answered by the following code.  


```r
#  daily steps questions answered
days <- unique(df1$date)
StepsPerDay <- 1:length(days)
StepsPerDay <- aggregate(steps ~ date,df3,sum,na.rm = TRUE)
StepsPerDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

Question 2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

The data is also plotted using ggplot2 and not the antiquadated cumbersom base plotting "method (torture?)". 
I never shot par on a golf course and do not want to remind myself of that using par in plots.  Ugh!


```r
library(ggplot2)

ggplot(StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day - NA's Removed") +xlab("Steps") + ylab("Days")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day.  I am ASSUMING that a plot is part of the required "report".  It certainly is more meaningful to me than a table of numbers.
Those calculations are recorded below.

```r
MeanSteps <- aggregate(steps~date, df3, mean,na.rm = TRUE)
MeanSteps
```

```
##          date      steps
## 1  2012-10-02  0.4375000
## 2  2012-10-03 39.4166667
## 3  2012-10-04 42.0694444
## 4  2012-10-05 46.1597222
## 5  2012-10-06 53.5416667
## 6  2012-10-07 38.2465278
## 7  2012-10-09 44.4826389
## 8  2012-10-10 34.3750000
## 9  2012-10-11 35.7777778
## 10 2012-10-12 60.3541667
## 11 2012-10-13 43.1458333
## 12 2012-10-14 52.4236111
## 13 2012-10-15 35.2048611
## 14 2012-10-16 52.3750000
## 15 2012-10-17 46.7083333
## 16 2012-10-18 34.9166667
## 17 2012-10-19 41.0729167
## 18 2012-10-20 36.0937500
## 19 2012-10-21 30.6284722
## 20 2012-10-22 46.7361111
## 21 2012-10-23 30.9652778
## 22 2012-10-24 29.0104167
## 23 2012-10-25  8.6527778
## 24 2012-10-26 23.5347222
## 25 2012-10-27 35.1354167
## 26 2012-10-28 39.7847222
## 27 2012-10-29 17.4236111
## 28 2012-10-30 34.0937500
## 29 2012-10-31 53.5208333
## 30 2012-11-02 36.8055556
## 31 2012-11-03 36.7048611
## 32 2012-11-05 36.2465278
## 33 2012-11-06 28.9375000
## 34 2012-11-07 44.7326389
## 35 2012-11-08 11.1770833
## 36 2012-11-11 43.7777778
## 37 2012-11-12 37.3784722
## 38 2012-11-13 25.4722222
## 39 2012-11-15  0.1423611
## 40 2012-11-16 18.8923611
## 41 2012-11-17 49.7881944
## 42 2012-11-18 52.4652778
## 43 2012-11-19 30.6979167
## 44 2012-11-20 15.5277778
## 45 2012-11-21 44.3993056
## 46 2012-11-22 70.9270833
## 47 2012-11-23 73.5902778
## 48 2012-11-24 50.2708333
## 49 2012-11-25 41.0902778
## 50 2012-11-26 38.7569444
## 51 2012-11-27 47.3819444
## 52 2012-11-28 35.3576389
## 53 2012-11-29 24.4687500
```

```r
aggregate(steps~date, df3, median, na.rm = TRUE)  # the predominace of 0 steps results in 0 median
```

```
##          date steps
## 1  2012-10-02     0
## 2  2012-10-03     0
## 3  2012-10-04     0
## 4  2012-10-05     0
## 5  2012-10-06     0
## 6  2012-10-07     0
## 7  2012-10-09     0
## 8  2012-10-10     0
## 9  2012-10-11     0
## 10 2012-10-12     0
## 11 2012-10-13     0
## 12 2012-10-14     0
## 13 2012-10-15     0
## 14 2012-10-16     0
## 15 2012-10-17     0
## 16 2012-10-18     0
## 17 2012-10-19     0
## 18 2012-10-20     0
## 19 2012-10-21     0
## 20 2012-10-22     0
## 21 2012-10-23     0
## 22 2012-10-24     0
## 23 2012-10-25     0
## 24 2012-10-26     0
## 25 2012-10-27     0
## 26 2012-10-28     0
## 27 2012-10-29     0
## 28 2012-10-30     0
## 29 2012-10-31     0
## 30 2012-11-02     0
## 31 2012-11-03     0
## 32 2012-11-05     0
## 33 2012-11-06     0
## 34 2012-11-07     0
## 35 2012-11-08     0
## 36 2012-11-11     0
## 37 2012-11-12     0
## 38 2012-11-13     0
## 39 2012-11-15     0
## 40 2012-11-16     0
## 41 2012-11-17     0
## 42 2012-11-18     0
## 43 2012-11-19     0
## 44 2012-11-20     0
## 45 2012-11-21     0
## 46 2012-11-22     0
## 47 2012-11-23     0
## 48 2012-11-24     0
## 49 2012-11-25     0
## 50 2012-11-26     0
## 51 2012-11-27     0
## 52 2012-11-28     0
## 53 2012-11-29     0
```

```r
#  I am calculating a median value based soley on non zero steps, although I will not use this criteria in the analysis
#  just exporing to see what happens
median_steps <- subset(df1,df1$steps >0, na.rm = TRUE)
Median.Steps <- aggregate(steps ~ date, median_steps, median, na.rm = TRUE) # to get median of actual steps
Median.Steps
```

```
##          date steps
## 1  2012-10-02  63.0
## 2  2012-10-03  61.0
## 3  2012-10-04  56.5
## 4  2012-10-05  66.0
## 5  2012-10-06  67.0
## 6  2012-10-07  52.5
## 7  2012-10-09  48.0
## 8  2012-10-10  56.5
## 9  2012-10-11  35.0
## 10 2012-10-12  46.0
## 11 2012-10-13  45.5
## 12 2012-10-14  60.5
## 13 2012-10-15  54.0
## 14 2012-10-16  64.0
## 15 2012-10-17  61.5
## 16 2012-10-18  52.5
## 17 2012-10-19  74.0
## 18 2012-10-20  49.0
## 19 2012-10-21  48.0
## 20 2012-10-22  52.0
## 21 2012-10-23  56.0
## 22 2012-10-24  51.5
## 23 2012-10-25  35.0
## 24 2012-10-26  36.5
## 25 2012-10-27  72.0
## 26 2012-10-28  61.0
## 27 2012-10-29  54.5
## 28 2012-10-30  40.0
## 29 2012-10-31  83.5
## 30 2012-11-02  55.5
## 31 2012-11-03  59.0
## 32 2012-11-05  66.0
## 33 2012-11-06  52.0
## 34 2012-11-07  58.0
## 35 2012-11-08  42.5
## 36 2012-11-11  55.0
## 37 2012-11-12  42.0
## 38 2012-11-13  57.0
## 39 2012-11-15  20.5
## 40 2012-11-16  43.0
## 41 2012-11-17  65.5
## 42 2012-11-18  80.0
## 43 2012-11-19  34.0
## 44 2012-11-20  58.0
## 45 2012-11-21  55.0
## 46 2012-11-22  65.0
## 47 2012-11-23 113.0
## 48 2012-11-24  65.5
## 49 2012-11-25  84.0
## 50 2012-11-26  53.0
## 51 2012-11-27  57.0
## 52 2012-11-28  70.0
## 53 2012-11-29  44.5
```

```r
ggplot(MeanSteps, aes(x = date, y = steps)) + geom_point() + ggtitle("Time Series of Average Steps - NA's Removed") +
        ylab("AverageSteps Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#  average daily activity pattern based on 5 minute intervals
ave.interval.steps <- aggregate(steps ~ interval, df3, mean)  # note use of df3, no NA's'
#  ave.interval.steps
ggplot(ave.interval.steps, aes(x = interval, y = steps)) + geom_point() +
        geom_line() + ggtitle("Average Number of Steps Taken Per 5 Minute Intervals- NA's Removed") +
        xlab("5 Minute Intervals") + ylab("Mean Steps For All Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
This question is answered by the following code.  Note that I have obtained the maximum number of steps and then used that for logical subsetting to obtain the interval.


```r
steps.max <- max(ave.interval.steps$steps)      #get maximum mean steps per interval
print(paste0("Maximum average steps is ",steps.max))
```

```
## [1] "Maximum average steps is 206.169811320755"
```

```r
a <- ave.interval.steps$steps == steps.max
b <- ave.interval.steps$interval[a == 1]
print(paste0("interval with maximum average number of steps is ",b))
```

```
## [1] "interval with maximum average number of steps is 835"
```

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

This question was answered way back at the beginning as part of the exploratory work.  For completeness, I am reproducing it here.

```r
#  determine number of rows with NA's, it is also printed in the summary above
df3 <- na.omit(df1)  # all NA rows omitted
dim(df1); dim(df3)  #  how many NA's?  2,304
```

```
## [1] 17568     3
```

```
## [1] 15264     3
```

```r
numOfNA <- nrow(df1) - nrow(df3)
numOfNA
```

```
## [1] 2304
```

```r
print(paste0("Number of missing values is ",numOfNA))  #  Question 1 answer
```

```
## [1] "Number of missing values is 2304"
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The code for this questions follows.


```r
#  IMPUTING MISSING VALUES
#  explore the steps = NA rows for patterns
df1$weekday <- weekdays(df1$date, abbreviate = TRUE)  #  create new variable weekday for subsetting
df2 <- subset(df1, is.na(df1$steps))  #  subset for steps = NA only
dim(df2)
```

```
## [1] 2304    4
```

```r
unique(df2$weekday)  #  what days need steps imputed?  All but Tuesday
```

```
## [1] "Mon" "Thu" "Sun" "Fri" "Sat" "Wed"
```

```r
dim(df2); class(df2)
```

```
## [1] 2304    4
```

```
## [1] "data.frame"
```

```r
identical(unique(df1$interval),unique(df2$interval))  # if true, then every interval has an NA 
```

```
## [1] TRUE
```

```r
#  now to determine if certain days are more active than others
steps.weekday <- aggregate(steps ~ weekday, df1,mean, na.rm = TRUE)
ggplot(steps.weekday, aes(x = weekday, y = steps, weekday = as.factor(weekday))) + geom_point() +
        ggtitle("Mean Steps per Day - NA's Removed")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#  Strategy for imputing missing values  Question 2
#  Wed, Fri, Sat, and Sun are busy days on average, will use mean to impute on these days
#  will use median value for intervals on Mon, Tues, and Thursday
#  create weekdays variable for dataframe to assiSt impute values for NA's- no NA's on Tuesday
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

There is undoubtedly a more elegant way to accomplish this task, but I have gone "simple brute force" to 
get the task accomplished.


```r
sun <- subset(df1,df1$weekday == "Sun")
mon <- subset(df1,df1$weekday == "Mon")
tue <- subset(df1,df1$weekday == "Tue")
wed <- subset(df1,df1$weekday == "Wed")
thu <- subset(df1,df1$weekday == "Thu")
fri <- subset(df1,df1$weekday == "Fri")
sat <- subset(df1,df1$weekday == "Sat")
#  replace Friday,  Saturday, Sunday, and Wenesday NA's with the interval mean
intv <- NA
impute <- function(weekday) {
  for (i in 1:nrow(weekday)) {
            aa <- is.na(weekday$steps[i])
            if( aa == 1){
                     intv[i] <- weekday$interval[i]
                     weekday$steps[i] <- mean(weekday$steps[weekday$interval[i] == intv[i]], na.rm =TRUE)
                   }
               }
         return(weekday)
}
fri <- impute(fri)
sat <- impute(sat)
sun <- impute(sun)
wed <- impute(wed)

#  replace Monday, Tuesday and Thursday NA's with 0 (which is the median) because these are low activity days
median.impute <- function(weekday) {
        aa <- is.na(weekday$steps)
        weekday$steps[aa== 1] = 0
        return(weekday)
}
mon <- median.impute(mon)
tue <- median.impute(tue)
thu <- median.impute(thu)

#  create new data frame with NA's replace with interval means or 0's
new.df1 <- rbind(sun, mon, tue, wed,thu, fri, sat)  #  Answer, question 3
anyNA(new.df1)  #verify all NA's have been eliminated
```

```
## [1] FALSE
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

To answer the last part of the question first, I have created a histogram with the data set containing NA's.  Then I have created a histogram with the NA values imputed.


```r
#  plot with NA's
days <- unique(df1$date)
StepsPerDay <- 1:length(days)
StepsPerDay <- aggregate(steps ~ date, df3,sum)  #df3 has NA's omittted, not replaced
StepsPerDay  # using the data set with NA's
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

```r
#  plot to answer question 4
ggplot(StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day - NA's removed") +xlab("Steps") + ylab("Days")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#  plot without NA's
days <- unique(new.df1$date)
new.StepsPerDay <- 1:length(days)
new.StepsPerDay <- aggregate(steps ~ date,new.df1,sum,na.rm = TRUE)
new.StepsPerDay  #  steps per day using the imputed values dataset
```

```
##          date    steps
## 1  2012-10-01     0.00
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08     0.00
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01     0.00
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 12277.71
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 12359.71
## 41 2012-11-10 12535.43
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 11790.75
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 12359.71
```

```r
ggplot(new.StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day With NA's Imputed") +xlab("Steps") + ylab("Days")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
#  There is a significant change with NA's imputed
```

Are there differences in activity patterns between weekdays and weekends?  There is a significant change with NA's imputed

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

I have created a new variable labeled new.df1$weekday as a factor.  I have rarely used factors (all the books on R that I have read indicate factors should only be created for specific circumstances), and those times I did it was just in a ggplot2 plot to create legends. 

The code has an error, weekend not found, even though it clearly has been included as a level.  If the lines are uncommented, then the weekday variable changes to weekend or weekday, which messes up my subsetting later on.  I would be concerned about this, but since I have no clue how to create a data set including weekday in the dataset created when using aggregate, it just does not matter.  

I have created two plots showing the difference between weekday and weekend, just not a panel plot showing the two.


```r
#DIFFERENCES IN ACTIVITY PATERNS BETWEEN WEEKDAYS AND WEEKENDS
#new.df1$weekday <- as.factor(new.df1$weekday)
#levels(new.df1$weekday) = list("weekend" = c("Sat", "Sun"),"weekday" = c("Mon", "Tue","Wed","Thu","Fri"))
#aa <- new.df1$weekday ==weekend
#bb <- new.df1$weekday == 1
#sum(bb)
#  Now to create data for weekday and weekends and do a panel plot
#weekend.new.df1 <- subset(new.df1, "weekday" = "weekend")

#ave.interval.steps <- aggregate(steps ~ interval, new.df1, mean)  # #note use of new.df1, no NA's'
#ave.interval.steps
#ggplot(ave.interval.steps, aes(x = interval, y = steps, col = #weekdays)) + geom_point() +
#        geom_line() + ggtitle("Average Number of Steps Taken Per 5 #Minute Intervals") +
#       xlab("5 Minute Intervals") + ylab("Mean Steps For All Days") +
#        facet_grid(weekday~ .)
#  ggplot will work with factors but the factor has to be in one of the levels
#  aggregate only returns interval and steps variables, no weekday variable is returned
#  I am at a loss as to how to include the weekday factor in the data frame passed to ggplot
#  therfore, I will create weekend and weekday data frames and prepare two plots, one for each
#  dataframe. They will not be panel plots, but the information will get presented
#  I am at a loss as to how to include the factor 
```


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

These two plots are not panel plots, but the information sought is clearly revealed.  First I haved created subset datasets for each day.  Then using rbind have created weekday and weekend data sets, which were thenplotted as histograms


```r
sun <- subset(new.df1,new.df1$weekday == "Sun")
mon <- subset(new.df1,new.df1$weekday == "Mon")
tue <- subset(new.df1,new.df1$weekday == "Tue")
wed <- subset(new.df1,new.df1$weekday == "Wed")
thu <- subset(new.df1,new.df1$weekday == "Thu")
fri <- subset(new.df1,new.df1$weekday == "Fri")
sat <- subset(new.df1,new.df1$weekday == "Sat")
Weekends <- rbind(sat, sun)
Weekdays <- rbind(mon,tue,wed,thu,fri)
#  verify subsetting worked
dim(Weekends)
```

```
## [1] 4608    4
```

```r
dim(Weekdays)
```

```
## [1] 12960     4
```

```r
dim(Weekends[1])+dim(Weekdays[1])  #  total rows for both dataframes
```

```
## [1] 17568     2
```

```r
#  preparing data for plots
sat.sun <- aggregate(steps ~ interval, Weekends,mean)
mon.fri <- aggregate(steps ~ interval, Weekdays,mean)
ggplot(sat.sun, aes(x=interval, y = steps)) + geom_point() + xlab("5 Minute Intervals") +
        ylab("Mean Number Of Steps") + ggtitle("Weekend Mean Steps by 5 Min Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
ggplot(mon.fri,aes(x = interval, y = steps)) + geom_point() + xlab("5 Minute Intervals") +
        ylab("Mean Number of Steps") + ggtitle("Weekday Mean Steps by 5 Min Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-2.png)<!-- -->
