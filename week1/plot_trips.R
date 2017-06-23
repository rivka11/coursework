
########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides

ggplot(trips, aes(x = tripduration)) +
  geom_density() 
?geom_density
# plot the distribution of trip times by rider type
  ggplot(trips, aes(x = tripduration, fill = usertype)) + 
    geom_histogram(bins = 60)+ xlim(0, 10)
# plot the number of trips over each day
ggplot(trips, aes(x = ymd)) + geom_histogram() # use group by
# plot the number of trips by gender and age group_by(age, gender)
trips %>% group_by(gender, age) %>% summarise(count=n())
ggplot(trips, aes(x = birth_year, fill = gender)) + geom_point(bins = 50, alpha = .25)
# plot the ratio of male to female trips by age
spread(trips, birth_year, gender) %>% ggplot(trips, aes(x = ))
ggplot(trips, aes(x= birth_year, y= count(gender), color = gender, bins = 30))
# hint: use the spread() function to reshape things to make it easier to compute this ratio
#INCOMPLETE!
?group_by


# remove unknown
# group by (age, gender)
# s

r <- (spread(trips, gender, birth_year) %>% group_by(Male) %>% count(Male))
(spread(trips, gender, birth_year) %>% group_by(Female) %>% count(Female))
count(trips, vars=c("birth","Size"))
?count
########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather, aes(x = ymd, y = tmin)) + geom_point() + geom_smooth()
# plot the minimum temperature and maximum temperature over each day


# hint: try using the gather() function for this to reshape things before plotting

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day


# you'll need to summarize the trips and join to the weather data to do this

trips %>% group_by(ymd, bikeid) %>% 
  mutate(numTrips =n()) %>% inner_join(weather, by = "ymd") %>% ggplot(aes(x = tripduration, y= tmin)) + geom_point()
# repeat this, splitting results by whether there was substantial precipitation or not


# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
