library(scales)
library(readr)
library(tidyverse)

# set plot theme
theme_set(theme_bw())

# read ratings from csv file
ratings <- read_csv('ratings.csv')

# for reference: same thing, using base R functions and explicitly setting column information
  ratings <- read.delim('ratings.csv',
                         sep=',',
                         header=F,
                         col.names=c('user_id','movie_id','rating','timestamp'),
                         colClasses=c('integer','integer','numeric','integer'))

print(object.size(ratings), units="Mb")

####################
# brief look at data
####################


head(ratings)
nrow(ratings)
str(ratings)
summary(ratings)

####################
# aggregate stats
####################

# plot distribution of rating values (slide 21)
ratings %>% 
  ggplot(aes(x= rating)) + geom_histogram(stat = "count") + scale_y_continuous(label = comma)


####################
# per-movie stats
####################

# aggregate ratings by movie, computing mean and number of ratings
ratings %>%
  group_by(movie_id) %>%
  summarise(avgRating = mean(rating), count = n())
# hint: use the n() function for easy counting within a group

# my own q get avg rating and total ratings overall
ratings %>% summarise(avgRating = mean(rating), numRatings = n())

# plot distribution of movie popularity (= number of ratings the movie received)
# hint: try scale_x_log10() for a logarithmic x axis
ratings %>% 
  group_by(rating, movie_id) %>% 
  summarise( numLikes = n()) %>% 
  ggplot(aes(x = movie_id)) + geom_histogram() + scale_x_log10() + scale_y_continuous(label = comma)
# plot distribution of mean ratings by movie (slide 23)
# hint: try geom_histogram and geom_density
ratings %>%
  group_by(movie_id) %>%
  summarise(avgRating = mean(rating), count = n()) %>% ggplot(aes(x= avgRating)) +geom_density() #swap geom_density fr geom_histogram
# rank movies by popularity and compute the cdf, or fraction of movies covered by the top-k moves (slide 25)
ratings %>%
  group_by(movie_id) %>%
  summarise(moviePop = n()) %>%
  arrange(desc(moviePop)) %>%
  mutate(freq = cumsum(movie_id)) %>%
  ggplot(aes(x = moviePop, y = freq)) + geom_density()
# hint: use dplyr's rank and arrange functions, and the base R sum and cumsum functions
ratings %>% 
  group_by(movie_id) %>%
  summarise(count = n()) %>%
  rank()
# plot the CDF of movie popularity

####################
# per-user stats
####################

# aggregate ratings by user, computing mean and number of ratings

# plot distribution of user activity (= number of ratings the user made)
ratings %>% group_by(user_id) %>% summarise(avgRatingForUser = mean(rating)) %>%ggplot(aes(x= user_id))+ geom_bar() + scale_y_log10()
# hint: try a log scale here

####################
# anatomy of the long tail
####################

# generate the equivalent of figure 2 of this paper:
# https://5harad.com/papers/long_tail.pdf

# Specifically, for the subset of users who rated at least 10 movies,
# produce a plot that shows the fraction of users satisfied (vertical
# axis) as a function of inventory size (horizontal axis). We will
# define "satisfied" as follows: an individual user is satisfied p% of
# the time at inventory of size k if at least p% of the movies they
# rated are contained in the top k most popular movies. As in the
# paper, produce one curve for the 100% user satisfaction level and
# another for 90%---do not, however, bother implementing the null
# model (shown in the dashed lines).

