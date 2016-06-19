
 ## Loading and preprocessing the data
  Make sure to have the activity.csv file downloaded to your home directory.  


```r
activity_data<-read.csv("activity.csv")
```
  
 
 ## What is mean total number of steps taken per day?  


```r
steps_by_day <- aggregate(steps ~ date, activity_data, sum)
hist(steps_by_day$steps, main = "Total Daily Steps Distribution", col="green",xlab="Number of Steps")
```

![plot of chunk daily_sum](figure/daily_sum-1.png)

```r
mean_steps<-mean(steps_by_day$steps)
median_steps<-median(steps_by_day$steps)
```

The mean steps is 1.0766189 &times; 10<sup>4</sup>.    
The median steps is 10765  
  
 ## What is the average daily activity pattern?


```r
steps_by_interval <- aggregate(steps ~ interval, activity_data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps,xlab = "Interval",ylab = "Mean Steps",main="Mean Steps by Interval",type = 'l')
```

![plot of chunk daily_pattern](figure/daily_pattern-1.png)

```r
max_interval<-steps_by_interval[steps_by_interval$steps==max(steps_by_interval$steps),]
```
The max interval is 835  


  ## Imputing missing values

```r
  ##1
na_records <- sum(!complete.cases(activity_data))
```
The number of records with missing data is 2304


```r
full_data <- transform(activity_data, steps = ifelse(is.na(activity_data$steps), steps_by_interval$steps[match(activity_data$interval, steps_by_interval$interval)], activity_data$steps))
```


```r
steps_by_day_f <- aggregate(steps ~ date, full_data, sum)
i_mean_steps<-mean(steps_by_day_f$steps)
i_median_steps<-median(steps_by_day_f$steps)
hist(steps_by_day_f$steps, main = paste("Total Steps Per Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="green", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "green"), lwd=5)
```

![plot of chunk imputing_part4](figure/imputing_part4-1.png)
As you can see the imputed results differ from the original data.

The new mean steps is 1.0766189 &times; 10<sup>4</sup>  
The new median steps is 1.0766189 &times; 10<sup>4</sup>

  ## Are there differences in activity patterns between weekdays and weekends?
  

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
full_data$day_type = as.factor(ifelse(is.element(weekdays(as.Date(full_data$date)),weekdays), "Weekday", "Weekend"))
library(lattice)
steps_by_interval_f <- aggregate(steps ~ interval + day_type, full_data, mean)
xyplot(steps_by_interval_f$steps ~ steps_by_interval_f$interval|steps_by_interval_f$day_type, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk weekday_compare](figure/weekday_compare-1.png)

