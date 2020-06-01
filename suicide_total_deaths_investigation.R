##gapminder data set: Suicide, total deaths(1990 to 2016)
##description: Total number of estimated deaths from self inflicted injury
deaths_df <- read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/suicide_total_deaths.csv",header = T)

#Learning details
str(deaths_df)
#1 - no clarity on the x axis
qplot(x=deaths_df$country,y=deaths_df$X1990,binwidth=1,color=I("pink"))

#2 - death rate at peak in india in the year 1990
##a very extreme case as the others in comparison have extremely lower rates
qplot(x=deaths_df$country,y=deaths_df$X1990,color=I("red"))+coord_cartesian(xlim=c(70,80))
