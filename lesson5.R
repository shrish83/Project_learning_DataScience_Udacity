fb_df<-read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/pseudo_facebook.tsv", header = T)

# "alpha" attribute in the geom_point()
# is used to get rid of overplotting and add transparency
# eg.
library(ggplot2)
ggplot(aes(age,friend_count), data = fb_df) +
  geom_point() + xlim(13,90)
##because the bove plot looks over plotted at the starting
##therfore using alpha=1/20th or 0.05 which means
## 1 black dot in scatter plot means 20 points(black dots in reality)
##hence properly scaled
ggplot(aes(age,friend_count), data = fb_df) +
  geom_point(alpha=1/20) + xlim(13,90)
##add jitter(i.e adding random noise in order to see the cloud more clearly) instead of geom_point
ggplot(aes(age,friend_count), data = fb_df) +
  geom_jitter(alpha=1/20) + xlim(13,90)
##using coord_trans()
ggplot(aes(age,friend_count), data = fb_df) +
  geom_point(alpha=1/20) + xlim(13,90) +
  coord_trans(y="sqrt")
##adding position_jitter so as to prevent
##having imaginary sqrts on y axis for friend_counts=0 
##as adding noise to "0" friend_count will result in -ve number
##sqrt of which will be imaginary, hence adding
###position_jitter with min height(h)=0
ggplot(aes(age,friend_count), data = fb_df) +
  geom_point(alpha=1/20, position=position_jitter(h=0)) + xlim(13,90) +
  coord_trans(y="sqrt")


# Examine the relationship between
# friendships_initiated (y) and age (x)
# using the ggplot syntax.
ggplot(aes(age,friendships_initiated), data = fb_df) +
  geom_point(alpha=1/10, position = position_jitter(h=0))


# Plot mean friend count vs. age using a line graph.
# Be sure you use the correct variable names
# and the correct data frame. You should be working
# with the new data frame created from the dplyr
# functions. The data frame is called 'pf.fc_by_age'.
# Use geom_line() rather than geom_point to create
# the plot.
#group_by() creates a table
library(dplyr)    #n() for counting no. of elements
age_groups <- group_by(fb_df,age)
pf.fc_by_age<- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median =median(friend_count),
          n=n())
head(pf.fc_by_age)
pf.fc_by_age <- as.data.frame(pf.fc_by_age)
ggplot(data = pf.fc_by_age, aes(x=age,y=friend_count_mean))+geom_line()

###Overlying Summaries of Raw data
ggplot(aes(age,friend_count), data = fb_df) +
  geom_point(alpha=1/20, position=position_jitter(h=0), color = "orange") + xlim(13,90) +
  geom_line(stat = "summary", fun.y=mean) +  #summary line for mean friend_count by age
  geom_line(stat = "summary", fun.y= "quantile", fun.args=list(probs=0.1), color="blue") + #10% quantile
  geom_line(stat = "summary", fun.y="quantile", fun.args=list(probs=0.5), color="green") + #50% quantile
  geom_line(stat = "summary", fun.y="quantile", fun.args=list(probs=0.9), color="red") + #90% quantile
  coord_cartesian(xlim = c(13,80), ylim = c(0,2500)) 


###PEARSON'S COEFFICENT 'LINEAR' CORRELATION (test statictic called 'cor')

##cor.test() using r( Pearson's correlation coefficent)
##to measure linear correlation b/w two variables
##a cor value i.e -0.3 >= cor >= 0.3 proposes a meaningful relation
cor.test(fb_df$age,fb_df$friend_count)
#since here cor = -0.0274 thus not a meaningful 
#correlation exists b/w these two varibles
##CAN ALSO USE with() function in place of cor.test
with(data = fb_df,cor.test(age,friend_count, method = "pearson"))
#same result as above i.e cor = -0.0274
#method="pearson" is by default so you even can omit it

##for age less than 70 yrs
with(data = subset(fb_df,age<70), cor.test(age,friend_count, method = "pearson"))

###SPEARMAN'S RANK COEFFICENT 'MONOTONIC' CORRELATION (test statictic called 'rho'
###monotonic - not entirely increasing or not entirely decreasing
 
with(fb_df, cor.test(age,friend_count, method = "spearman"))

# Create a scatterplot of likes_received (y)
# vs. www_likes_received (x).
library(ggplot2)
qplot(x=fb_df$www_likes_received,y=fb_df$likes_received)
##to remove outliers and focus on the 95% of the bulk region that is on the lower left
qplot(x=fb_df$www_likes_received,y=fb_df$likes_received) + 
  xlim(0,quantile(fb_df$www_likes_received,0.95)) + 
  ylim(0,quantile(fb_df$likes_received,0.95))
##the slope of the line that best fits is the correlation
qplot(x=fb_df$www_likes_received,y=fb_df$likes_received) + 
  xlim(0,quantile(fb_df$www_likes_received,0.95)) + 
  ylim(0,quantile(fb_df$likes_received,0.95)) +
  geom_smooth(method = "lm", color="red")
###What's the correlation b/w these two variables?
###Include the top 5% values for the variables in the calculation.
cor.test(fb_df$www_likes_received,fb_df$likes_received)


# Create a new variable, 'age_with_months', in the 'pf' data frame.
# Be sure to save the variable in the data frame rather than creating
# a separate, stand-alone variable. You will need to use the variables
# 'age' and 'dob_month' to create the variable 'age_with_months'.

fb_df$age_with_months <- (2013-fb_df$dob_year) + (12-fb_df$dob_month)/12

age_with_months_groups <- group_by(fb_df,age_with_months)
pf.fc_by_age_months <- summarise(age_with_months_groups,
                                 friend_count_mean = mean(friend_count),
                                 friend_count_median =median(friend_count),
                                 n=n())


# Create a new line plot showing friend_count_mean versus the new variable,
# age_with_months. Be sure to use the correct data frame (the one you created
# in the last exercise) AND subset the data to investigate users with ages less
# than 71.

##Using geom_smooth() surely gives us an edge over analyizing
##but it misses out on important stuff eg. it does not show
##the steep rise and fall at age 69
library(ggplot2)
p2<-ggplot(data = subset(pf.fc_by_age_months,age_with_months<71), aes(x=age_with_months,y=friend_count_mean)) +
  geom_line(color = "red") + geom_smooth()
p1<-ggplot(data = subset(pf.fc_by_age,age<71), aes(x=age,y=friend_count_mean)) +
  geom_line() + geom_smooth()
p3<-ggplot(data = subset(pf.fc_by_age,age<71), aes(x=round(age/5)*5,y=friend_count_mean)) +
  geom_line(stat = "summary", fun.y="mean")

library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1)





