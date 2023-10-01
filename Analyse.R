# Step 1
# ------
# Loading and preprocessing the data
# Show any code that is needed to
# Load the data (i.e. read.csv())
# Process/transform the data (if necessary) into a format suitable for your analysis
#

data = read.csv("activity.csv")
# date recognized as such
library(lubridate)
data$pretty_date = ymd(data$date)

# Step 2
# ------
# What is mean total number of steps taken per day?
#   For this part of the assignment, you can ignore the missing values in the dataset.
# - Calculate the total number of steps taken per day
# - Make a histogram of the total number of steps taken each day
# - Calculate and report the mean and median of the total number of steps taken per day
#

# Total Number of Steps taken per day
library('dplyr')
total_number_of_steps_taken_by_day = data %>% group_by(pretty_date) %>% summarise(total_number_of_steps_taken_by_day=sum(steps))

# Plot the total Number of Steps taken per day
library('ggplot2')
with(total_number_of_steps_taken_by_day, qplot(total_number_of_steps_taken_by_day, xlab = 'Total Number of Steps taken per day', main = 'Histogram for the total number of steps taken per day'))

# Calculate Mean and Median
summary(total_number_of_steps_taken_by_day)
mean_total_number_of_steps_taken_by_day = mean(total_number_of_steps_taken_by_day$total_number_of_steps_taken_by_day, na.rm = TRUE)
# [1] 10766.19
median_total_number_of_steps_taken_by_day = median(total_number_of_steps_taken_by_day$total_number_of_steps_taken_by_day, na.rm = TRUE)
# [1] 10765
#

# Step 3
# ------
# What is the average daily activity pattern?
#
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#
mean_number_of_steps_taken_by_interval = data %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarise(mean_number_of_steps_taken_by_day=mean(steps))
# Plot the mean Number of Steps taken per day
plot(x=mean_number_of_steps_taken_by_interval$interval, y=mean_number_of_steps_taken_by_interval$mean_number_of_steps_taken_by_day, xlab=("interval"), ylab = "mean number of steps taken by day", main = "Mean Number of Steps taken by day", type="l")
qplot(data = mean_number_of_steps_taken_by_interval, x=interval, y=mean_number_of_steps_taken_by_day, xlab=("interval"), ylab = "mean number of steps taken by day", main = "Mean Number of Steps taken by day")

index_max = which.max(mean_number_of_steps_taken_by_interval$mean_number_of_steps_taken_by_day)
max_val = mean_number_of_steps_taken_by_interval[index_max,]$mean_number_of_steps_taken_by_day
# max_val
# [1] 206.1698

# Step 4
# ------
#
# Imputing missing values
#
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.
#
# - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# - Create a new dataset that is equal to the original dataset but with the missing data filled in.
# - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#   Do these values differ from the estimates from the first part of the assignment? 
#   What is the impact of imputing missing data on the estimates of the total daily number of steps?
#      It created new data with value = mean
#      As a result, it had no impact on mean values but changed the distribution of data. It had an effect on the median.
  
number_of_row_with_na = sum(is.na(data$steps))
# [1] 2304
# no NA in the other columns
#

# We fill NA with the mean for this interval
# Should not change any of the Step 3 results
data_with_average = full_join(data, mean_number_of_steps_taken_by_interval, by="interval")
data_with_average$steps_wo_na = ifelse(is.na(data_with_average$steps), data_with_average$mean_number_of_steps_taken_by_day, data_with_average$steps)

# Sanity check
mean_number_of_steps_taken_by_interval_wo_na = data_with_average %>% group_by(interval) %>% summarise(mean_number_of_steps_taken_by_day_wo_na=mean(steps_wo_na))
# The mean is the same as before
# Replacing NA has no impact on the mean (as expected)

data_wo_na = data_with_average %>% select(steps_wo_na, pretty_date, interval) %>% rename(steps = steps_wo_na, date=pretty_date)
total_number_of_steps_taken_by_day_wo_na = data_wo_na %>% group_by(date) %>% summarise(total_number_of_steps_taken_by_day=sum(steps))

# Plot the total Number of Steps taken per day
library('ggplot2')
with(total_number_of_steps_taken_by_day_wo_na, qplot(total_number_of_steps_taken_by_day, xlab = 'Total Number of Steps taken per day', main = 'Histogram for the total number of steps taken per day'))

# Calculate Mean and Median
summary(total_number_of_steps_taken_by_day_wo_na)
mean_total_number_of_steps_taken_by_day_wo_na = mean(total_number_of_steps_taken_by_day_wo_na$total_number_of_steps_taken_by_day, na.rm = TRUE)
# [1] 10766.19
median_total_number_of_steps_taken_by_day_wo_na = median(total_number_of_steps_taken_by_day_wo_na$total_number_of_steps_taken_by_day, na.rm = TRUE)
# [1] 10766.19
#

# Step 5
# ------

# Are there differences in activity patterns between weekdays and weekends?
# For this part the 
# weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#

# Question 1
data_wo_na$is_weekend = (wday(data_wo_na$date) == 1) | (wday(data_wo_na$date) == 7)
data_wo_na$day_of_week = wday(data_wo_na$date, abbr = FALSE, label = TRUE)
data_wo_na$weekpart = as.factor(ifelse(data_wo_na$is_weekend, "weekend", "weekday"))

# Question 2
mean_number_of_steps_taken_by_interval_weekpart = data_wo_na %>% summarise(mean_number_of_steps_taken_by_day=mean(steps), .by=c(interval, weekpart))
mean_number_of_steps_taken_by_interval_weekpart_check = aggregate(data=data_wo_na, steps ~ interval + weekpart, mean)

library(lattice)
xyplot(mean_number_of_steps_taken_by_day ~ interval | weekpart, mean_number_of_steps_taken_by_interval_weekpart, scales = list(relation = "same", alternating = FALSE), ylab = "Mean Number of Steps taken by day and interval", main = "Mean Number of Steps taken by day and interval", type="l",layout = c(1,2))
