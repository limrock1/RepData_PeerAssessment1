### Project 1 - Global Settings ###

## Load libraries
library(dplyr)
library(ggplot2)
library(lattice)
library(scales)

## Check for existence of raw data - download & extract if not present
if(!file.exists("repdata%2Fdata%2Factivity.zip")) {
        dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(dataURL, dest="./repdata%2Fdata%2Factivity.zip", method="curl")
        unzip("./repdata%2Fdata%2Factivity.zip")
}
data <- read.csv("activity.csv", header=T, sep=",")

### End Of Global Settings ###

### Project 1 - Part 1 ###
total_steps <- data %>% group_by(date) %>% summarise(total = sum(steps,na.rm = T))
mean <- mean(total_steps$total)
median <- median(total_steps$total)
png(filename="histogram.png", width=480, height=480, units="px")
histogram(date ~ total, data=total_steps, 
          main="Histogram of total steps taken per day",
          xlab="Total Steps",
          ylab="% Days")
# sexier version - not required
#ggplot(total_steps, aes(x=total), binwidth=x) + 
#        geom_histogram(fill="grey") +
#        geom_vline(xintercept = mean, color = "red", linetype = "dashed", size = 1) + 
#        geom_vline(xintercept = median, color = "blue", linetype = "dashed", size = 1) + 
#        labs(title="Histogram of total steps taken per day", x="Total Steps")
dev.off()
### Part 2 ###

# time <= convert intervals into 4 digit numbers & separate hours/mins with ':'
data$time <- sprintf("%04d",data$interval)
data$time <- paste(substr(data$time, 1, 2),substr(data$time, 3, 4), sep=":")

mean_steps <- stack(tapply(data$steps, data$time, mean, na.rm=TRUE))
colnames(mean_steps) <- c("mean", "time")

# make a type 1 base plot of average steps per time interval
png(filename="time_series.png", width=480, height=480, units="px")
plot(mean_steps$time, mean_steps$mean, type="b",
     main="Daily Avg Steps Per Time Interval",
     xlab = "Time Intervals",
     ylab = "Mean Steps")
dev.off()

# report the time of the max average step count
max_interval <- mean_steps[which.max(mean_steps$mean),]

### Part 3 ###

NA_count <- nrow(subset(data, is.na(data))) 

# create a merged table with the mean values per time 
# we then swap out NA for the mean value at this time
merged <- merge(data,mean_steps, by="time")
merged$steps[is.na(merged$steps)] <- merged$mean[is.na(merged$steps)]

# new dataset == original but with NAs replaced & ordered by date, interval
data_new <- merged[,1:4]
data_new <- data_new[order(data_new$date,data_new$interval),]
total_steps_new <- data_new %>% group_by(date) %>% summarise(total = sum(steps))
mean <- mean(total_steps_new$total)
median <- median(total_steps_new$total)

# replot the summed steps histogram from part 1 with NAs replaced
png(filename="histogram_new.png", width=480, height=480, units="px")
histogram(date ~ total, data=total_steps_new, 
          main="Histogram of total steps taken per day",
          xlab="Total Steps",
          ylab="% Days")
# sexier version - not required
#ggplot(total_steps_new, aes(x=total), binwidth=x) + 
#        geom_histogram(fill="grey") +
#        geom_vline(xintercept = mean, color = "red", linetype = "dashed", size = 1) + 
#        geom_vline(xintercept = median, color = "blue", linetype = "dashed", size = 1) + 
#        labs(title="Histogram of total steps taken per day", x="Total Steps")
dev.off()

### Part 4 ###

# Add a day of the week column representiing each date
data_week <- cbind(data_new,weekdays(as.Date(data_new$date)))
colnames(data_week) <- c("time","steps","date","interval","day")
# substitute weekdays and weekend appropriately
data_week$day <- gsub("Saturday|Sunday", "weekend", data_week$day)
data_week$day <- gsub("(.*day)", "weekday", data_week$day)
data_week_avg <- data_week %>% group_by(interval, day) %>% summarise(steps_avg = mean(steps))

png(filename="time_series_new.png", width=480, height=480, units="px")
xyplot(steps_avg ~ interval | day, data=data_week_avg, type="b",
       layout=c(1,2),
       main="Avg Steps Taken per Interval (Weekdays v Weekends)",
       xlab="Intervals",
       ylab="Number Of Steps"
       )
dev.off()
