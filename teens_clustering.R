#K means clustering-- unsupervised learning algorithm
#grouping of similar occuring category and naming accordingly

teams_df=read.csv("D:/datascience/snsdata.csv")
str(teams_df)
table(teams_df$gender,useNA ="ifany")
summary(teams_df$age)
View(teams_df)
#ifelse command used for filtering required values
teams_df$age=ifelse(teams_df$age>=13 & teams_df$age<20,teams_df$age,NA)
summary(teams_df$age)


#gender preprocessing
#creating new column for self understanding
teams_df$female= ifelse(teams_df$gender=="F" &! is.na(teams_df$gender),1,0)
table(teams_df$female)
teams_df$no_gender=ifelse(is.na(teams_df$gender),1,0)
table(teams_df$no_gender)


summary(teams_df$age)
mean(teams_df$age, na.rm = TRUE)
# average age as per graduation year#like a groupby command of sql-- aggregate in R
aggregate(data=teams_df,age~gradyear,mean,na.rm=TRUE)
#this below (average)function actually converted all values to average according to grad year
avg_age= ave(teams_df$age,teams_df$gradyear,FUN = function(x) mean(x,na.rm=TRUE))
avg_age[10000:10500]
#new column we merged averaged values in na values of original column
teams_df$new_age=ifelse(is.na(teams_df$age),avg_age,teams_df$age)
summary(teams_df$new_age)


install.packages("devtools")
devtools::install_github("arunsrinivasan/cran.stats")
library(stats)

interest=teams_df[5:40]
myclusters= kmeans(interest,5)#5 indicates
myclusters
 
#zscore normalization
interests_z= as.data.frame(lapply(interest,scale))
#lapply returns in matrix form
View(interests_z)

myclusters= kmeans(interests_z,5)
myclusters$size
myclusters$centers

teams_df$cluster= myclusters$cluster
str(teams_df)
teens_dfnew=teams_df[2:4]
str(teens_dfnew)
teens_dfnew$cluster=teams_df$cluster


aggregate(data=teens_dfnew, age~cluster, mean)
teens_dfnew$female=teams_df$female
aggregate(data=teens_dfnew, female~cluster, mean)
aggregate(data=teens_dfnew, friends~cluster, mean)

