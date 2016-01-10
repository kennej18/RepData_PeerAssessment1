#Loading and preprocessing the data
data <- read.csv("activity.csv")

#What is mean total number of steps taken per day?
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)

#What is the average daily activity pattern?
library(ggplot2)
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                     FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps, type = "l")) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

average[which.max(average$steps), ]
#Imputing missing values
missing <- is.na(data$steps)
table(missing)
#5-minute interval
fill.value <- function(steps, interval){
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (average[average$interval==interval, "steps"])
  return(filled)
}
filled.data <- data 
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval) 

total_steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps)
median(total_steps)

#Are there differences in activity patterns between weekdays and weekends?
days <- weekdays(filled.data$date)
filled.data$day <- ifelse(days == "Saturday" | days == "Sunday", 
                           "Weekend", "Weekday")
filled.data <- aggregate(filled.data$steps,
                       by=list(data$interval,
                               data$day),mean)
names(filled.data) <- c("interval","day","steps")
xyplot(steps~interval | day, average,type="l",
       layout=c(1,2),xlab="Interval",ylab = "Number of steps")

tapply(filled.data$steps,filled.data$day,
       function (x) { c(MINIMUM=min(x),MEAN=mean(x),
                        MEDIAN=median(x),MAXIMUM=max(x))})
