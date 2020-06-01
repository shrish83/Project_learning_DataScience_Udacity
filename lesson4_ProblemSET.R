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

