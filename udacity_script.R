reddit_df<-read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/reddit.csv")
summary(reddit_df)

#ordering factors
reddit_df$age.range<-factor(reddit_df$age.range, levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"))
qplot(reddit_df$age.range)

#Lesson 3
### Statics 'by' gender
fb_df<-read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/pseudo_facebook.tsv", header = T)
by(fb_df$friend_count,fb_df$gender,summary)  #by is a wrapper function for tapply
96-74

### plot using tenure in years
library(ggplot2)
tenure_yrs<-fb_df$tenure/365;
tenure_yrs
ggplot(fb_df,aes(x=tenure_yrs))+geom_histogram(binwidth=1,color="black",fill="blue")+ggtitle("tenure")+xlab("tenure in years")

###histogram of ages
qplot(x=fb_df$age, binwidth=1,fill="orange",color="black")+ggtitle("plot of ages")+xlab("Age")

###package gridExtra...changing variables
install.packages("gridExtra")  #to arrange plots in one layout
library(gridExtra)
p1<-qplot(fb_df$friend_count)
summary(fb_df$friend_count)
p2<-qplot(log10(fb_df$friend_count))
summary(log10(fb_df$friend_count))
p3<-qplot(sqrt(fb_df$friend_count))
summary(sqrt(fb_df$friend_count))

grid.arrange(p1,p2,p3,ncol=1)

###Frequency polygons
qplot(x=www_likes,data = subset(fb_df,!is.na(gender)),geom = "freqpoly",color=gender)+scale_x_log10()
#Quest- which gender has more www_likes?
by(fb_df$www_likes,fb_df$gender,sum) #OR
tapply(fb_df$www_likes,fb_df$gender,sum)


###Box Plots
qplot(x=gender,y=friend_count,data=subset(fb_df,!is.na(gender)),geom = "boxplot")
##removing outliers
qplot(x=gender,y=friend_count,data=subset(fb_df,!is.na(gender)),geom = "boxplot", ylim=c(0,1000))
##use scale_y_continuous(limits=c(0,1000) OR
##for best: use coord_cartesian()
qplot(x=gender,y=friend_count,data=subset(fb_df,!is.na(gender)),geom = "boxplot")+coord_cartesian(ylim = c(0,1000))


###What % of check ins using mobile?
summary(fb_df$mob)
