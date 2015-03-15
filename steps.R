rm(list=ls())

unzip(zipfile = "activity.zip")
activities <- read.csv("activity.csv")
activities$date <- as.Date(activities$date, format="%Y-%m-%d")

summary(activities)

steps_per_day <- tapply(activities$steps, activities$date, sum, na.rm = TRUE)
mean(steps_per_day)
median(steps_per_day)

hist(steps_per_day, breaks = length(steps_per_day), main="Histogram: Number of Steps per Day", xlab="Steps per Day", ylab="Frequency", col="red")

averages <- aggregate(x = list(steps = activities$steps), by = list(interval = activities$interval), FUN = mean, na.rm = TRUE)
plot(averages$steps~averages$interval, type="l", xlab="Time interval", ylab="Average number of steps")


for (i in 1:length(activities$step)) {
  if (is.na(activities$step[i])) {
    activities$step[i] <- averages$steps[i]
  }
}