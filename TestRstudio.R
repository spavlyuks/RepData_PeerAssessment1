
# load data to variable. Read date variable as character
activity = read.csv(unz("activity.zip", "activity.csv"), 
                colClasses = c("integer", "character", "integer" ))


#I have checked date format and it is "year-month-day". So I convert
#the date variable
activity = transform(activity, date = as.Date(date, "%Y-%m-%d"))
activity = with(activity, activity[order(date, interval),])

#convert intervals to time and attach it to dataframe
from =  as.POSIXct(min(activity$date))

time_int = seq (from = from, length.out = nrow(activity), by = "5 mins")
activity$time_int = time_int


max(activity$interval)


#plot hist of everyday activities
library("ggplot2")
ggplot(activity, aes(x=date)) +
  geom_histogram( aes(weight = steps), binwidth = 1) +
  labs (aes(y = "Steps", x = "Date"))

#find mean and median of steps per day
steps_p_day = tapply(activity$steps, activity$date, sum, na.rm = TRUE)
mean(steps_p_day)
median(steps_p_day)

#plot patterns
steps_p_inter = tapply(activity$steps, activity$interval,
                       mean, na.rm = TRUE)
steps_p_inter = data.frame(interval = as.integer(names(steps_p_inter)), 
                           steps = steps_p_inter)

#calculate breaks and labes to time in human readeble format
breaks = seq(0, to = 2400, by = 300)
labels = paste(as.character(breaks/100), ":00", sep="")

ggplot(steps_p_inter,aes(interval, steps)) +
         geom_line() +
          scale_x_discrete (breaks = breaks, labels = labels)

#find interval with max steps per day
with(steps_p_inter, steps_p_inter[steps == max(steps), "interval"])


#calculate the number of rows with NA's
nrow(activity[is.na(activity$steps), ])

#creat new databes where NA change to mean for interval
n_activity = merge(activity, steps_p_inter, by="interval")

n_activity[is.na(n_activity$steps.x), ]$steps.x = 
  n_activity[is.na(n_activity$steps.x), ]$steps.y

#plot pattern for new dataset
ggplot(n_activity, aes(interval, steps.x)) +
  stat_summary(fun.y = "mean", geom = "line") 

#show new mean and median
steps_p_day_n = tapply(n_activity$steps.x, activity$date, sum)
mean(steps_p_day_n)
median(steps_p_day_n)

# Mean now is higher. This meen that usualy where absent information 
# about intervals where is high activity (much number of steps) 

week_day = as.POSIXlt(n_activity$date)$wday

n_activity$day_type = "weekend"
n_activity[week_day <= 5, ]$day_type = "weekday"

#Transform interval to  time. plot day activity by the week day and weekend
ggplot(n_activity, aes(x = interval, y = steps.x, colour = day_type)) + 
  stat_summary(fun.y = mean, geom = "line")


