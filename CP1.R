#  Reproducible Research Course Project 1 

#  Preliminary exploratory work
df1 <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
#  do a bit of data exploring
summary(df1); str(df1)
head(df1)
anyNA(df1$steps); anyNA(df1$date); anyNA(df1$Interval)  #  only steps contains NA's
df1$date <- as.Date(df1$date)  #  to allow use of weekdays function
class(df1$date)  #verify it worked

# Verify later calculation 
df2 <- subset(df1,date == "2012-10-02") # to check calculations performed later
sum(df2$steps)  #  verified above, no NA in dates or intervals
mean(df2$steps)
median(df2$steps)

#  determine number of rows with NA's, it is also printed in the summary above
df3 <- na.omit(df1)  # all NA rows omitted
dim(df1); dim(df3)  #  how many NA's?  2,304
numOfNA <- nrow(df1) - nrow(df3)
numOfNA

#  daily steps questions answered
days <- unique(df1$date)
StepsPerDay <- 1:length(days)
StepsPerDay <- aggregate(steps ~ date,df3,sum,na.rm = TRUE)

library(ggplot2)

ggplot(StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day - NA's Removed") +xlab("Steps") + ylab("Days")
MeanSteps <- aggregate(steps~date, df3, mean,na.rm = TRUE)
MeanSteps
aggregate(steps~date, df3, median, na.rm = TRUE)  # the predominace of 0 steps results in 0 median
#  I am calculating a median value based soley on non zero steps, although I will not use this criteria in the analysis
#  just exporing to see what happens
median_steps <- subset(df1,df1$steps >0, na.rm = TRUE)
Median.Steps <- aggregate(steps ~ date, median_steps, median, na.rm = TRUE) # to get median of actual steps
Median.Steps
ggplot(MeanSteps, aes(x = date, y = steps)) + geom_point() + ggtitle("Time Series of Average Steps - NA's Removed") +
        ylab("AverageSteps Per Day")

#  average daily activity pattern based on 5 minute intervals
ave.interval.steps <- aggregate(steps ~ interval, df3, mean)  # note use of df3, no NA's'
#  ave.interval.steps
ggplot(ave.interval.steps, aes(x = interval, y = steps)) + geom_point() +
        geom_line() + ggtitle("Average Number of Steps Taken Per 5 Minute Intervals- NA's Removed") +
        xlab("5 Minute Intervals") + ylab("Mean Steps For All Days")
steps.max <- max(ave.interval.steps$steps)      #get maximum mean steps per interval
print(paste0("Maximum average steps is ",steps.max))
a <- ave.interval.steps$steps == steps.max
b <- ave.interval.steps$interval[a == 1]
print(paste0("interval with maximum average number of steps is ",b))

#  IMPUTING MISSING VALUES
#  nUMBER OF MISSING VALUES WAS CALCULATED ON LINE 22 ABOVE and is shown in the summary display
print(paste0("Number of missing values is ",numOfNA))  #  Question 1 answer

#  explore the steps = NA rows for patterns
df1$weekday <- weekdays(df1$date, abbreviate = TRUE)  #  create new variable weekday for subsetting
df2 <- subset(df1, is.na(df1$steps))  #  subset for steps = NA only
dim(df2)
unique(df2$weekday)  #  what days need steps imputed?  All but Tuesday
dim(df2); class(df2)
identical(unique(df1$interval),unique(df2$interval))  # if true, then every interval has an NA 
#  now to determine if certain days are more active than others
steps.weekday <- aggregate(steps ~ weekday, df1,mean, na.rm = TRUE)
ggplot(steps.weekday, aes(x = weekday, y = steps, weekday = as.factor(weekday))) + geom_point() +
        ggtitle("Mean Steps per Day - NA's Removed")

#  Strategy for imputing missing values  Question 2
#  Wed, Fri, Sat, and Sun are busy days on average, will use mean to impute on these days
#  will use median value for intervals on Mon, Tues, and Thursday
#  create weekdays variable for dataframe to assiSt impute values for NA's- no NA's on Tuesday
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
anyNA(new.df1)

#  plot with NA's
days <- unique(df1$date)
StepsPerDay <- 1:length(days)
StepsPerDay <- aggregate(steps ~ date, df3,sum)  #df3 has NA's omittted, not replaced

#  plot to answer question 4
ggplot(StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day - NA's removed") +xlab("Steps") + ylab("Days")
#  plot without NA's
days <- unique(new.df1$date)
new.StepsPerDay <- 1:length(days)
new.StepsPerDay <- aggregate(steps ~ date,new.df1,sum,na.rm = TRUE)
ggplot(new.StepsPerDay, aes(steps)) + geom_histogram() + ggtitle("Steps per day With NA's Imputed") +xlab("Steps") + ylab("Days")
#  There is a significant change with NA's imputed

#DIFFERENCES IN ACTIVITY PATERNS BETWEEN WEEKDAYS AND WEEKENDS
#  new.df1$weekday <- as.factor(new.df1$weekday)
#  levels(new.df1$weekday) = list("weekend" = c("Sat", "Sun"),"weekday" = c("Mon", "Tue","Wed","Thu","Fri"))
#  aa <- new.df1$weekday ==weekend
#  bb <- new.df1$weekday == 1
#  sum(bb)
#  Now to create data for weekday and weekends and do a panel plot
#  weekend.new.df1 <- subset(new.df1, "weekday" = "weekend")

#  ave.interval.steps <- aggregate(steps ~ interval, new.df1, mean)  # note use of new.df1, no NA's'
#  ave.interval.steps
#ggplot(ave.interval.steps, aes(x = interval, y = steps, col = weekdays)) + geom_point() +
#        geom_line() + ggtitle("Average Number of Steps Taken Per 5 Minute Intervals") +
 #       xlab("5 Minute Intervals") + ylab("Mean Steps For All Days") +
#        facet_grid(weekday~ .)
        
#  ggplot will work with factors but the factor has to be in one of the levels
#  aggregate only returns interval and steps variables, no weekday variable is returned
#  I am at a loss as to how to include the weekday factor in the data frame passed to ggplot
#  therfore, I will create weekend and weekday data frames and prepare two plots, one for each
#  dataframe. They will not be panel plots, but the information will get presented
        
#  I am at a loss as to how to include the factor 
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
dim(Weekdays)
dim(Weekends[1])+dim(Weekdays[1])  #  total rows for both dataframes
#  preparing data for plots
sat.sun <- aggregate(steps ~ interval, Weekends,mean)
mon.fri <- aggregate(steps ~ interval, Weekdays,mean)
ggplot(sat.sun, aes(x=interval, y = steps)) + geom_point() + xlab("5 Minute Intervals") +
        ylab("Mean Number Of Steps") + ggtitle("Weekend Mean Steps by 5 Min Intervals")
ggplot(mon.fri,aes(x = interval, y = steps)) + geom_point() + xlab("5 Minute Intervals") +
        ylab("Mean Number of Steps") + ggtitle("Weekday Mean Steps by 5 Min Intervals")

