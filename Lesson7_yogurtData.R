##The yogurt dataset has over 2000 observations for 9 different variables.
##These observations are for households that buy Dannon yogurt over time

##read dataset
yo_df <- read.csv("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/yogurt.csv",header = T)
str(yo_df)

yo_df$id <- factor(yo_df$id)

##histogram of yogurt prices
ggplot(data = yo_df, aes(x=yo_df$price))+
  geom_histogram(binwidth = 1, fill="yellow")

##unique prices
length(unique(yo_df$price))  #20
table(yo_df$price)

# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.
##using transform() for robustness
yo_df <- transform(yo_df, all.purchases = strawberry+blueberry+pina.colada+plain+mixed.berry)

# Create a scatterplot of price vs time.
library(ggplot2)
ggplot(data = yo_df, aes(x=time,y=price))+
  geom_jitter(alpha=1/5 ,color = "blue")


##using samples to have a closer look
set.seed(3000)
sample_ids <- sample(levels(yo_df$id),16)
##using sample of levels(ids) so as to have unique 
##different ids
ggplot(data = subset(yo_df,id %in% sample_ids), aes(x=time, y=price)) +
  facet_wrap(~id)+
  geom_line() +
  geom_point(aes(size=all.purchases), pch=1)


##Scatterplot Matrices
install.packages("GGally")
library(GGally)
theme_set(theme_minimal(20))
fb_df<-read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/pseudo_facebook.tsv", header = T)

set.seed(1836)
pf_subset <- fb_df[,c(2:15)]
names(pf_subset)
## ggpairs(pf_subset[sample.int(nrow(pf_subset),1000),]) 
##don't run this command as it might take over an hour to produce a scatterplot matrix
##instead there's a pdf in the folder

##create a Heat Map
nci <- read.table("C:/Users/Swapnil Kumar Vaish/Documents/R/DataScience/Udacity_/nci.tsv", header = T)
 
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c("gene", "case","value")
head(nci.long.samp)

#make the heat map
ggplot(data = nci.long.samp, aes(x=case, y=gene, fill=value))+
  geom_tile()+
  scale_fill_gradientn(colours=colorRampPalette(c("blue","red"))(100))









