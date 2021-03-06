Reporducible Research: Peer Assessment 1

Loading and preprocessing the data
```{r}
library(base)
library(lattice)
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")
```
What is mean total number of steps taken per day?
```{r}
library(ggplot2)
total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps, na.rm=TRUE)
median(total_steps, na.rm=TRUE)
```


What is the average daily activity pattern?
```{r}
library(ggplot2)
average <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                     FUN=mean, na.rm=TRUE)
ggplot(data=average, aes(x=interval, y=steps, type = "l")) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
average[which.max(average$steps), ]
```

Imputing missing values
```{r}
missing <- is.na(data$steps)
table(missing)
```

```{r}
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
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
```{r}
total_steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total_steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total_steps)
median(total_steps)
```

Are there differences in activity patterns between weekdays and weekends?
```{r}
weekday_or_weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday_or_weekend)
```
Let's make a panel plot containing plots of average number of steps taken on weekdays and weekends.
```{r}
average <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
