#Write code to create a new data frame,
#called 'pf.fc_by_age_gender', that contains
#information on each age AND gender group.
#The data frame should contain the following variables:
#mean_friend_count,
#median_friend_count,
#n (the number of users in each age and gender grouping)

fb_df<-read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/pseudo_facebook.tsv", header = T)
fc_by_age_gender <- group_by(fb_df,age,gender)
pf.fc_by_age_gender <- summarise(fc_by_age_gender,
                                 mean_friend_count = mean(friend_count),
                                 median_friend_count = median(friend_count),
                                 n=n())

# Create a line graph showing the
# median friend count over the ages
# for each gender. Be sure to use
# the data frame you just created,
# pf.fc_by_age_gender.

library(ggplot2)
ggplot(data = subset(pf.fc_by_age_gender, !is.na(gender)), aes(x=age,y=median_friend_count))+
  geom_line(aes(color = gender))

#pf.fc_by_age_gender is in long format 
#thus converting it into wide format
#using median friend count
#dcast() wher 'd' means a data frame
#also acast for converting to an array, etc

library(reshape2)
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                  age ~ gender,
                                  value.var = "median_friend_count")
## use melt() on pf.fc_by_age_gender.wide 
## to cast it back to original format


# Plot the ratio of the female to male median
# friend counts using the data frame
# pf.fc_by_age_gender.wide.

ggplot(data = pf.fc_by_age_gender.wide, aes(x=age,y=female/male))+
  geom_line() +
  geom_hline(yintercept = 1, linetype=2)


# Create a variable called year_joined
# in the pf data frame using the variable
# tenure and 2014 as the reference year.
fb_df$year_joined <- floor(2014 - (fb_df$tenure/365)) 


# Create a new variable in the data frame
# called year_joined.bucket by using
# the cut function on the variable year_joined.
# You need to create the following buckets for the
# new variable, year_joined.bucket
#        (2004, 2009]
#        (2009, 2011]
#        (2011, 2012]
#        (2012, 2014]

summary(fb_df$year_joined)  #most of the people joined in the year 2012
table(fb_df$year_joined)  #how many users joined in each year

fb_df$year_joined.bucket <-  cut(fb_df$year_joined, breaks = c(2004,2009,2011,2012,2014))
table(fb_df$year_joined.bucket)

# Create a line graph of friend_count vs. age
# so that each year_joined.bucket is a line
# tracking the median user friend_count across
# age. This means you should have four different
# lines on your plot.

ggplot(data = subset(fb_df, !is.na(year_joined.bucket)), aes(x=age,y=friend_count))+
  geom_line(aes(color=year_joined.bucket), stat = "summary", fun.y=median) 
  

## Write code to do the following:
# (1) Add another geom_line to code below
# to plot the grand mean of the friend count vs age.
# (2) Exclude any users whose year_joined.bucket is NA.
# (3) Use a different line type for the grand mean.  

ggplot(data = subset(fb_df, !is.na(year_joined.bucket)), aes(x=age,y=friend_count))+
  geom_line(aes(color=year_joined.bucket), stat = "summary", fun.y=median) +
  geom_line(stat = "summary", fun.y=mean ,linetype=2)

##friending rate
with(subset(fb_df,tenure >=1), summary(friend_count/tenure))


# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.
library(ggplot2)
ggplot(data = subset(fb_df, tenure >=1), aes(x=tenure, y= (friendships_initiated/tenure)))+
  geom_line(aes(color= fb_df$year_joined.bucket))

