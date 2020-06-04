##LESSON 4
#diamonds dataset
data("diamonds")
str(diamonds)
 library(ggplot2)
qplot(x=diamonds$price,binwidth=500,color="black",fill="yellow")
summary(diamonds$price)

#diamonds cost less than $500,$250 and >= $15000?
nrow(diamonds[diamonds$price>=15000,]) #or
summary(diamonds$price>=15000)

#Explore the largest peak in the histogram
qplot(x=price,data = diamonds,binwidth=1,color=I("black"),fill=I("yellow"),xlab = "Price of diamonds",ylab = "Number of diamonds")+scale_x_continuous(limits = c(600,700), breaks = seq(600,1000,5))

# Break out the histogram of diamond prices by cut.
qplot(x=price,data = diamonds,binwidth=1,fill=I("yellow"),xlab = "Price of diamonds",ylab = "Number of diamonds")+scale_x_continuous(limits = c(600,700), breaks = seq(600,1000,5))+facet_wrap(~diamonds$cut)

#Which cut has the a)lowest median price b)lowest priced diamond c)highest priced diamond?
by(diamonds$price,diamonds$cut,min)

#different scales for y axis or every graph _ use scales="free_y" or "free_x" on x-axis
#because scales="fixed" by default Thus showssame scales on axis for every graph
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales="free_y")

# Create a histogram of price per carat
# and facet it by cut.
qplot(x=price/carat,data = diamonds,fill=I("yellow"),xlab = "Price of diamonds",ylab = "Number of diamonds")+scale_x_log10()+facet_wrap(~diamonds$cut)


# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.
qplot(x=diamonds$color,y=diamonds$price,geom = "boxplot")+coord_cartesian(ylim = c(1000,8000))

#price range for the middle 50%with color D,J?
tapply(diamonds$price,diamonds$color,quantile)

#best/worst color
levels(diamonds$color)

#iqr for best/worst color
price_D<-diamonds[diamonds$color=="D",]
price_J<-diamonds[diamonds$color=="J",]
IQR(price_D$price)
IQR(price_J$price)

# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
qplot(x=diamonds$color,y=diamonds$price/diamonds$carat,geom = "boxplot")+coord_cartesian(ylim = c(1000,8000))

#carat frequency polygon - #carat size having count grater than 2000?
qplot(x=carat,data = diamonds, geom = "freqpoly",color=I("green"))+scale_x_continuous(breaks = seq(0,1.5,0.1))+scale_y_continuous(seq(0,9000,1000))+coord_cartesian(ylim=c(0,2500))

##tidyr - reshapes layout of data
##dplyr - transform tidy tabular data


##********************************************************************************************************************************


##LESSON 6
ggplot(data = diamonds, aes(x=x,y=price)) +
  geom_point()

##correlation b/w x and price,y and price, z and price?
cor.test(diamonds$price,diamonds$z)

# Create a simple scatter plot of price vs depth.
#make the transparency of the points to be 1/100 
#of what they are now and mark the x-axis every 2 units
range(diamonds$depth)
ggplot(data = diamonds, aes(x=depth,y=price)) +
  geom_point(alpha=0.01) +
  scale_x_discrete(breaks = seq(43,79,2))

##Most diamonda are between whatvalues of depth
diamonds_depth_groups <- group_by(diamonds,depth)
pf.fc_by_diamonds_depth <- summarise(diamonds_depth_groups,
                                 diamonds_count=n())
pf.fc_by_diamonds_depth[order(pf.fc_by_diamonds_depth$diamonds_count),]

a <- max(pf.fc_by_diamonds_depth$diamonds_count)
pf.fc_by_diamonds_depth[pf.fc_by_diamonds_depth$diamonds_count==a,]


##correlation b/w depth vs price
cor.test(diamonds$depth, diamonds$price)

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
ggplot(data = diamonds, aes(x=carat,y=price)) +
  geom_point() +
  xlim(0, quantile(diamonds$carat,0.99)) +
  ylim(0,quantile(diamonds$price,0.99))


# Create a scatterplot of price vs. volume (x * y * z).
volume <- diamonds$x * diamonds$y * diamonds$z
diamonds$volume <- volume
ggplot(data = diamonds, aes(x=volume,y=price)) +
  geom_point() 

##diamonds with volume 0
library(plyr)
count(diamonds$volume == 0)  

#***The plyr package will conflict with the dplyr package in later exercises.***
#therfore unload the plyr package as required or load the dplyr again

## What is the correlation b/w price and volume
##exclude diamonds that have volume of 0 or greater than
##or equal to 800
subset_diamond <- subset(diamonds, (volume!=0 & volume<800) )
cor.test(subset_diamond$price,subset_diamond$volume)

ggplot(data= subset_diamond, aes(x=volume,y=price)) +
  geom_point(alpha=1/40) +
  geom_smooth()


# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.
# Name the data frame diamondsByClarity
# The data frame should contain the following
# variables in this order.
#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
# where n is the number of diamonds in each
# level of clarity.
detach("package:plyr", unload=TRUE)
library(dplyr)

clarity_groups <- group_by(diamonds, clarity)
diamondsbyClarity <- summarise(clarity_groups,
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n())


### We’ve created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

bar_color <- ggplot(data = diamonds_mp_by_color, aes(x=color,y=mean_price)) +
  geom_bar(stat = "identity")
bar_clarity <- ggplot(data = diamonds_mp_by_clarity, aes(x=clarity,y=mean_price))+
  geom_bar(stat = "identity")
library(gridExtra)
grid.arrange(bar_color,bar_clarity,ncol=1)


##*********************************************************************************************************************88


##LESSON 8 - PROBLEM SET
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.
#The diamonds data set is actually part of the excellent ggplot2 package
library(ggplot2)
data(diamonds)

qplot(data = diamonds, x = log(price)) +
  facet_wrap(~color) + aes(fill = cut) + scale_x_log10()

# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.
ggplot(data = diamonds, aes(x=table, y=price))+
  geom_point(aes(color=cut))

#table range for diamonds of the ideal and premium cut
by(diamonds$table,diamonds$cut,range)

# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.
volume <- diamonds$x * diamonds$y * diamonds$z
diamonds$volume <- volume
ggplot(data = diamonds, aes(x=volume,y=price)) +
  geom_point(aes(color=clarity)) +
  xlim(0, quantile(diamonds$volume,0.99)) +
  scale_y_log10() 
  

# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).
########
# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

fb_df<-read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/pseudo_facebook.tsv", header = T)
fb_df$prop_initiated = fb_df$friendships_initiated / fb_df$friend_count
fb_df$prop_initiated[is.nan(fb_df$prop_initiated)] <- 0


# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

fb_df$year_joined <- floor(2014 - (fb_df$tenure/365)) 
fb_df$year_joined.bucket <- cut(fb_df$year_joined, c(2004, 2009, 2011, 2012, 2014))
ggplot(data = fb_df, aes(x=tenure, y= prop_initiated)) +
  geom_line(aes(color=year_joined.bucket),stat = "summary", fun.y=median)


# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

ggplot(data = fb_df, aes(x=tenure, y= prop_initiated)) +
  geom_line(aes(color=year_joined.bucket),stat = "summary", fun.y=median) +
  geom_smooth()

##which group initiated greatest proportions of facebook friendships
by(fb_df$prop_initiated, fb_df$year_joined.bucket, summary)


# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.
ggplot(data = diamonds, aes(x=cut, y=price/carat))+
  geom_jitter(aes(color = color)) +
  facet_wrap(~clarity)
  
