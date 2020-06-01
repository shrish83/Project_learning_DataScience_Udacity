# Your task is to investigate the distribution of your friends'
# birth months and days.

#import birthday data
setwd("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_")
birthday_df <- read.csv("birthdaysExample.csv",header = T)

#description
str(birthday_df)
 
#use lubridate ymd() for robust variations OR
library(tidyr)
#install.packages("zeallot")
library(zeallot)

birthday_df1 <- separate(birthday_df,dates, into = c("month","day","year")) 

# How many people have the same birthday as you?
nrow(birthday_df1[birthday_df1$day==9 & birthday_df1$month==11,])

# Which month contains the most number of birthdays?
max_month<-as.data.frame(table(birthday_df1$month))
max_month[max_month$Freq==max(max_month$Freq),]

# How many birthdays are in each month?
tapply(max_month$Freq,max_month$Var1,sum)

# Which day of the year has the most number of birthdays?
days<-as.numeric(as.vector(birthday_df1$day))
df_days<-as.data.frame(count(days))
df_days[df_days$freq ==max(df_days$freq),]
