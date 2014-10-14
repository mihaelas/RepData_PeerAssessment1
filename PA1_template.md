Loading and preprocessing the data
========================================================


```r
if(!suppressMessages(require(ggplot2))){
    print('trying to install ggplot2')
    install.packages('ggplot2')
    if(suppressMessagesrequire(ggplot2)){
        print("ggplot2 installed and loaded")
    } else {
        stop("could not install ggplot2")
    }
}

# Define some options for knitr
knitr::opts_chunk$set(tidy=FALSE, fig.path='figures/')


unzip("U:/mis/HOPKINS/repdata_data_activity.zip")
activity <- read.csv(file="U:/mis/HOPKINS/activity.csv",stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

head(activity)
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
##What is mean total number of steps taken per day?

daily_activity <-
  aggregate(formula = steps~date, data = activity,
            FUN = sum, na.rm=TRUE)


mean_steps <- round(mean(daily_activity$steps), 2)  # Mean
median_steps <- quantile(x = daily_activity$steps, probs = 0.5)  # Median, 50%Q
mean_steps
```

```
## [1] 10766
```

```r
median_steps
```

```
##   50% 
## 10765
```

```r
histogram <- 
qplot(x=date, y=steps,
      data=subset(activity, complete.cases(activity)),
      stat='summary', fun.y=sum, geom='bar') +
  labs(title='Figure 1: Number of steps taken daily\n',
       y='Total steps per day', x='Date')
plot(histogram)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r
## What is the average daily activity pattern? 
##Aggregate the steps per interval, calculating the mean across the days

interval_activity <- 
  aggregate(steps ~ interval, data=activity,
            FUN=mean, na.rm=TRUE)
# Get the data for the interval with the most average activity across the days
max_steps <- interval_activity[which(interval_activity$steps==max(interval_activity$steps)),]
max_steps
```

```
##     interval steps
## 104      835 206.2
```

```r
# Function to calculate de mean and normal 
# 95% confidence interval around it
mean_ci <- function(data){
    m <- mean(data)
    data.frame(y=m,
               ymin = m-(1.96*sd(data)/sqrt(length(data))),
               ymax = m+(1.96*sd(data)/sqrt(length(data))))
}

# Plot the average number of steps per interval.
# Use ggplot2 to summarize de data, to
# find inconsistencies with the analysis.
# Geom 'line' is equivalent to 'type="l"' in plot.
steps_per_interval <- 
qplot(x=interval, y=steps,
      data=subset(activity, complete.cases(activity)),
      geom='smooth', stat='summary', fun.data=mean_ci) +
  labs(title='Figure 2: Average of steps taken each interval, across the days\n',
       y='Average steps per interval', x='Interval')

steps_per_interval
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
##IMPUTING MISSING VALUES

# Count the number of NAs
total_NAs <- sum(!complete.cases(activity))
step_NAs <- sum(is.na(activity$steps))

# Calculate the number of missing dates
dates_in_range <- seq.Date(from = min(activity$date),
                           to = max(activity$date),
                           by='1 day')
date_NAs <- sum(!activity$date[complete.cases(activity)] %in% dates_in_range)

 ##Use previously calculated means
interval_activity$imputed_steps <- floor(interval_activity$steps)

# Merge the replacement values
imputed_activity <- merge(activity,
                          interval_activity[,c('interval', 'imputed_steps')],
                          by='interval')

# Replace the missing values
imputed_activity$steps <- ifelse(is.na(imputed_activity$steps),
                                 imputed_activity$imputed_steps,
                                 imputed_activity$steps)

# Remove unnecesary data
imputed_activity$imputed_steps <- NULL


# Summarize the data by day
daily_imputed_activity <-
  aggregate(formula = steps~date, data = imputed_activity,
            FUN = sum, na.rm=TRUE)

# Calculate summary statistics
mean_imputed_steps <- round(mean(daily_imputed_activity$steps), 2)
median_imputed_steps <- quantile(x = daily_imputed_activity$steps, probs = 0.5)

mean_imputed_steps
```

```
## [1] 10750
```

```r
median_imputed_steps
```

```
##   50% 
## 10641
```

```r
# Replace the data in the original histogram with the imputed data
histogram %+% imputed_activity +
  labs(title='Figure 3: Number of steps taken each day,\nafter imputing missing values')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-13.png) 

```r
##Are there differences in activity patterns between weekdays and weekends?

# Label each date as weekday/weekend (1:5 are weekdays, 6:7 are weekends)
imputed_activity$week_part <- factor(
  ifelse(as.integer(format(imputed_activity$date, format = '%u')) %in% c(1:5),
         'weekday', 'weekend'))

# Plot the average steps per interval, given the week_part
steps_per_interval %+% imputed_activity + facet_grid(week_part~.) +
  labs(title='Figure 4: Average of steps taken each interval across the days, \n given the part of the week')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-14.png) 



