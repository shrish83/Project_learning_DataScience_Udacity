##Diamonds and Price Predictions

#**Using Linear Regression models**
##WE bulid the "line of best fit" that best describes
##the data using the regression coefficients i.e slope and
##y-intercept. 
##In the "line of best fit" the "residual value" ie. the difference between
##the expected and observed "y" values, is the least.
##We can create the "line of best fit" by
#***"MINIMIZING THE SUM OF SQUARED RESIDUALS"***
#NOTE: slope = r(sd(y)/sd(x))  i.e r = Pearson's coefficent


#****HISTOGRAMS - for qualitative -qualitative pairs
#****SCATTER PLOTS ans CORRELATION- for quantitative -quantitative pairs
#****BOXPLOTS - for qualitative-quantitative pairs


library(ggplot2)
data(diamonds)

# We can quickly see if the relationship is linear or not.
# In this case, we can use a variety of diamond
# characteristics to help us figure out whether
# the price advertised for any given diamond is 
# reasonable or a rip-off.
# Let's consider the price of a diamond and it's carat weight.
# Create a scatterplot of price (y) vs carat weight (x).
# Limit the x-axis and y-axis to omit the top 1% of values.

ggplot(data = diamonds, aes(x=carat, y= price))+
  geom_point(color="blue")+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))

## relationship is not linear and the dispersion 
##increases as the carat size increases

##Add a  linear model
ggplot(data = diamonds, aes(x=carat, y= price))+
  geom_point(color="blue", alpha=1/5)+
  xlim(0,quantile(diamonds$carat,0.99))+
  ylim(0,quantile(diamonds$price,0.99))+
  stat_smooth(method = "lm")

##ggpairs function
library(GGally)   ##for ggpairs fucntion
library(scales)
#install.packages("memisc")
library(memisc)    ##to summarize the regression
library(lattice)
library(MASS) 
library(car)       ###to recode variables
library(reshape2)  ##to reshape and wrangle your data
library(plyr)    ##to create interesting summarise and transmissions

##set seed for randomization purposes and
##sample 10000 diamonds from the dataset
set.seed(20022012)
diamond_sample <- diamonds[sample(1:length(diamonds$price),10000),]
ggpairs(diamond_sample, params = c(shape=I("."), outlier.shape=I(".")))


# Create two histograms of the price variable
# and place them side by side on one output image.
library(gridExtra)

plot1 <- qplot(x=diamonds$price) + 
  ggtitle('Price')

plot2 <- qplot(x=log10(diamonds$price)) +
  ggtitle('Price (log10)')

grid.arrange(plot1,plot2,ncol=2)

##NOW
#color by clarity does offer variance in price as in
#better the clarity higher the price
cuberoot_trans = function() trans_new("cuberoot", transform = function(x) x^(1/3),
                                      inverse = function(x) x^3)

ggplot(data = diamonds, aes(x=carat, y= price, color=clarity))+
  geom_point(alpha=1/2, position="jitter", size=3/4)+
  scale_x_continuous(trans=cuberoot_trans(), limits =c(0.2,3), breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(), limits=c(350,15000), breaks=c(350,1000,5000,10000,15000))

###Hence we see a linear relationship

##color by $cut doesn't offer much variance
ggplot(data = diamonds, aes(x=carat, y= price, color=cut))+
  geom_point(alpha=1/2, position="jitter", size=3/4)+
  scale_x_continuous(trans=cuberoot_trans(), limits =c(0.2,3), breaks = c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(), limits=c(350,15000), breaks=c(350,1000,5000,10000,15000))



##LINEAR MODELS IN R
#Use lm(y~x) where y - outcome variable and x - explanatory variable

##Building Linear model for price
#Use I() wrapper which means 'as is'
##***WE'LL USE THIS MODEL TO KNOW WHETHER THE PRICE
##***OF A DIAMOND IS REASONABLE
m1 <- lm(data = diamonds, I(log(price)) ~ I(carat^(1/3)))
m2 <- update(m1, ~. + carat)
m3 <- update(m2, ~. + cut)
m4 <- update(m3, ~. + color)
m5 <- update(m4, ~. + clarity)
mtable(m1,m2,m3,m4,m5)

# For R-squared, you want the regression model to
# explain higher percentages of the variance. 
# Higher R-squared values indicate that the data 
# points are closer to the fitted values. While 
# higher R-squared values are good, they don't 
# tell you how far the data points are from the 
# regression line.



#######A BETTER DATASET 
#install.packages("RCurl")
library(RCurl)
library(bitops)
setwd("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_")
BD <- load("BigDiamonds.rda")

#********DESCRIPTION************

# Your task is to build five linear models like Solomon
# did for the diamonds data set only this
# time you'll use a sample of diamonds from the
# diamondsbig data set.

# Be sure to make use of the same variables
# (logprice, carat, etc.) and model
# names (m1, m2, m3, m4, m5).

#There's 598,024 diamonds in this data set!
# Since the data set is so large,
# you are going to use a sample of the
# data set to compute the models. You can use
# the entire data set on your machine which
# will produce slightly different coefficients
# and statistics for the models.

DB_sample <- sample(diamondsbig,200000)
mB1 <- lm(data = subset(diamondsbig, price<10000 & cert == "GIA"), I(log(price)) ~ I(carat^(1/3)))
mB2 <- update(mB1, ~. + carat)
mB3 <- update(mB2, ~. + cut)
mB4 <- update(mB3, ~. + color)
mB5 <- update(mB4, ~. + clarity)
mtable(mB1,mB2,mB3,mB4,mB5)


###****PREDICTIONS****
#Example Diamond from BlueNile
#Round 1.00 Very Good I VS1 $5,601
this.diamond <- data.frame(carat=1.00, cut="V.Good", color="I",clarity="VS1")
modelEstimate <- predict(mB5,newdata = this.diamond,interval = "prediction", level = 0.95)
modelEstimate
