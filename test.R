setwd("~/Data/course5week2/RepData_PeerAssessment1")
activity  <- read.csv("activity.csv", sep = ",", header = T)
activity$date <- as.Date(as.character(activity$date), format = "%Y-%m-%d")
totalstep <- aggregate(steps~date, data = activity, FUN = sum)
hist(totalstep$steps, main = "Total Steps Taken Each Day", xlab = "Number of Steps", col = "yellow")
dailypattern <- aggregate(steps~interval, activity, FUN = mean)
meanstep <- mean(totalstep$steps)
medianstep <- median(totalstep$steps)
ggplot(dailypattern, aes(x=interval, y=steps)) + geom_line(col = "blue")
dailypattern[which.max(dailypattern$steps), 1]
sum(is.na(activity$steps))
for (i in 1:nrow(activity)) 
        {if (is.na(activity[i,1])) 
                activity[i,1] <- dailypattern[activity[i,3]==dailypattern$interval, ]$steps
        }
fillactivity <- activity
totalstep1 <- aggregate(steps~date, data = fillactivity, FUN = sum)
hist(totalstep1$steps, main = "Total Steps Taken Each Day (with NA filled in)", xlab = "Number of Steps", col = "green")
meanstep1 <- mean(totalstep1$steps)
medianstep1 <- median(totalstep1$steps)
fillactivity$w <- weekdays(fillactivity$date)
for (i in 1:nrow(fillactivity)) 
        {if (fillactivity$w[i] == "Saturday"|fillactivity$w == "Sunday")
                fillactivity$day[i] <- "weekend"
        else fillactivity$day[i] <- "weekday"
}
library(lattice)
weekday <- aggregate(steps~interval + day, fillactivity, FUN = mean)
xyplot(steps~interval|day, data=weekday, type="l", xlab = "Interval", ylab= "Number of steps")

xyplot(steps~interval|day, data=weekday, type="l", xlab = "Interval", ylab= "Number of steps")