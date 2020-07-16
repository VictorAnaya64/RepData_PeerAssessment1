install.packages("lubridate")
install.packages("dplyr")

library(lubridate)
library(ggplot2)
library(dplyr)        
unzip("activity.zip")

ActivityDS=read.csv("activity.csv")

ActivityDS$date=as.Date(ActivityDS$date)

#   What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
Totals<-tapply(ActivityDS$steps,as.factor(ActivityDS$date),sum,na.rm=TRUE)

#Make a histogram of the total number of steps taken each day
hist(Totals)

#Calculate and report the mean and median of the total number 
#of steps taken per day
mean(Totals,na.rm=TRUE)
median(Totals,na.rm=TRUE)


#Make a time series plot of the 5-minute interval (x-axis) and the average 
#number of steps taken, averaged across all days (y-axis)
Avg_5m<-tapply(ActivityDS$steps,as.factor(ActivityDS$interval),mean,na.rm=TRUE)

plot(x=names(Avg_5m) , y=Avg_5m,type='l',main="Steps by each interval")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
Avg_5m[Avg_5m==max(Avg_5m)]
as.numeric(names(Avg_5m[Avg_5m==max(Avg_5m)]))/60


###### Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)
sum(is.na(ActivityDS$steps))


# 2. Devise a strategy for filling in all of the missing values in the dataset.
#    The strategy does not need to be sophisticated. For example, you could 
#    use the mean/median for that day, or the mean for that 5-minute interval,
#    etc.

ActivityDS2=ActivityDS
ActivityDS2$steps[is.na(ActivityDS2$steps)] = 0


# 4. Make a histogram of the total number of steps taken each day and 
#    Calculate and report the mean and median total number of steps taken 
#    per day. Do these values differ from the estimates from the first part of 
#    the assignment? What is the impact of imputing missing data on the 
#    estimates of the total daily number of steps?

Totals2<-tapply(ActivityDS2$steps,as.factor(ActivityDS2$date),sum,na.rm=FALSE)

#Make a histogram of the total number of steps taken each day
hist(Totals2)

#Calculate and report the mean and median of the total number 
#of steps taken per day
mean(Totals2,na.rm=TRUE)
median(Totals2,na.rm=TRUE)



#1. factors
ActivityDS2$dayWeek=wday(ActivityDS2$date,week_start = 1)
ActivityDS2$WeekEndFlag[ActivityDS2$dayWeek>=5]="Weekend"
ActivityDS2$WeekEndFlag[ActivityDS2$dayWeek<5]="Weekday"

Totals3<-aggregate(ActivityDS2$steps, list(ActivityDS2$interval,
                                           ActivityDS2$WeekEndFlag), mean)

names(Totals3)=c("interval","WeekFlag","Steps")

ggplot(data = Totals3, aes(interval, Steps)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  labs(title = "Average steps by 5-min interval",
       subtitle = "(groupping by Weekday  /  Weekend)",
       y = "Average steps", x = "") + 
  facet_wrap(~ WeekFlag)


