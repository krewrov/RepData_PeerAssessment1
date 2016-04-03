#
# Coursera 
# Reproducible  Research
# Peer Assessment 1
#
# April 3rd, 2016
#
# Erwin Vorwerk
#

# Ensure required libraries are included

require(ggplot2)
require(lubridate)
require(RColorBrewer)
require(dplyr)
require(lattice)

# Assume data is downloaed via URL provided and stored in working directory

# 1. Load data
activity_data <- read.csv("activity.csv", stringsAsFactors=FALSE)

# 2. Process/transform the data (if necessary) into a format suitable for analysis

# Convert text to date
activity_data$date<-as.Date(activity_data$date)
# Insert weekday descriptors
activity_data$Weekday<-wday(activity_data$date, label=TRUE, abbr=FALSE)

# 3. What is the mean total number of steps taken per day?

# For this part of the assignment, we can ignore missing values in the dataset.

# 3.1 Make a histogram of the total number of steps taken each day

# Calculate the total number of steps each day, while removing NA values
total_steps_per_day <- aggregate(activity_data$steps,
                                 by=list(activity_data$date),
                                 FUN=sum, na.rm=TRUE)
# Relabel the attributes
names(total_steps_per_day) <- c("date","total")

# Now create the histogram
hist(total_steps_per_day$total,
     xlab="Total number of steps",
     main="Histogram of the total number of steps taken each day (wihtout NA)",
     col="red",
     breaks=seq(from=0,to=25000, by=1000))

# 3.2 Calculate and report the mean and median total number of steps taken per day

mean(total_steps_per_day$total)
median(total_steps_per_day$total)

# 4 What is the average daily activity pattern?

# 4.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

# Create the aggregate
mean_steps_per_day_by_interval <- aggregate(activity_data$steps,
                                             by=list(activity_data$interval),
                                             FUN=mean, na.rm=TRUE)
 
 # Relabel the attribues
 names(mean_steps_per_day_by_interval) <- c("interval","mean")
 
# Now create the plot
 plot(mean_steps_per_day_by_interval$interval,
      mean_steps_per_day_by_interval$mean,
      type="l",
      xlab="Average number of steps",
      main="Timeseries of the average number of steps taken per interval \n(wihtout NA)",
      col="red")

 # 4.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 
 # Locate the position of the maximum mean
 max_position <- which(mean_steps_per_day_by_interval$mean == max(mean_steps_per_day_by_interval$mean))
 
 # Now find the value of the interval at the max
 max_interval <- mean_steps_per_day_by_interval[max_position, 1]
 
 # 5 Inputing the missing values
 
 # Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing 
 # days may introduce bias into some calculations or summaries of the data.
 
 # 5.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA’s)

 # Establish the number of NA's 
 activity_NA_count <- sum(is.na(activity_data$steps))
 
 # 5.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be 
 # sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
 
 # Let's use the strategy to replace NA values with the mean of steps
 # First, find the elements that have NA
 activity_NA_position <- which(is.na(activity_data$steps))
 
 # Then, establish a vector of mean values
 activity_mean_vector <- rep(mean(activity_data$steps, na.rm=TRUE), times=length(activity_NA_position))
 
# 5.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
 
# Replace the NA values found by the mean values calculated
activity_data[activity_NA_position,"steps"] <- activity_mean_vector
 
# 5.4 Make a histogram of the total number of steps taken each day and calculate and report the mean and median 
# total number of steps taken per day. Do these values differ from the estimates from the first part of the 
# assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Calculate the total number of steps per day
total_steps_by_day <- aggregate(activity_data$steps,
                                by=list(activity_data$date),
                                FUN=sum)
# Relabel the attributes
names(total_steps_by_day) <- c("date","total")

# Create the histogram of the total number of steps each day
hist(total_steps_by_day$total,
     xlab="Total number of steps",
     main="Histogram of the total number of steps taken each day \n(NA replace by means)",
     col="red",
     breaks=seq(from=0,to=25000, by=1000))

# Calculate mean and median with NAs replaced by means

mean(total_steps_by_day$total)
median(total_steps_by_day$total)

# 6 Are there differences in activity patterns between weekdays and weekends?

# 6.1 Create a new factor variable in the dataset with two levels - “weekdays” 
# and “weekend” indicating whether a given date is a weekday or weekend day.

# Add an extra column to activity_data containing indicator for weekday/weekend
activity_data <- activity_data %>% 
  mutate(Daytype = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# 6.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 
# 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday 
# days or weekend days (y-axis).

# Calculate the average number of steps per daytype
mean_steps_per_day_by_daytype <- aggregate(activity_data$steps,
                                           by=list(activity_data$Daytype,
                                                   activity_data$Weekday, 
                                                   activity_data$interval),
                                           mean)

# Relabel the attribues
names(mean_steps_per_day_by_daytype) <- c("daytype","Daytype","interval","mean")

# Extract data to be plotted from vector
plot_data_by_daytype<- activity_data%>%
  group_by(interval, Daytype)%>%
  summarise(average_steps = mean(steps, na.rm=TRUE))
head(plot_data_by_daytype)

# Createthe time serie plot
plot<- ggplot(plot_data_by_daytype, aes(x =interval , y=average_steps, color=Daytype)) +
  geom_line() +
  labs(title = "Avgerage Daily Steps by Daytype", x = "Interval", y = "Number of Steps") +
  facet_wrap(~Daytype, ncol = 1, nrow=2)
print(plot)
