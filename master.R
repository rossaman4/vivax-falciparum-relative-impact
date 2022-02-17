
##########
# master.r
###########


rm(list=ls())


# set directory
# setwd("...")


#read in data from the csv file
data<- read.csv(file = 'database_rel_impact.csv', header=TRUE, sep=";" )
# correct first column name
colnames(data)[1]<-c("first_authors")


# clean and prepare the data
source("data_preparation_and_cleaning.R")
source("reshaping_dataframe.R")
source("preparation_analysis.R")


 




