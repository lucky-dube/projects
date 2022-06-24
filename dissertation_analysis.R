#############
### DATA ####
#############

# load data
usoc <- read.csv("~/Documents/Birkbeck work/Dissertation/data/usoc.csv")

# Check to see if packages are installed. 
# Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse", "stringr", "haven", "tidyr", "panelr") # list packages
check.packages(packages) # check packages are installed and load them

### CLEANING ###

# counting observations by year before cleaning
# create vectors of years in the data
sample_period <- c(2009:2020)

for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc, usoc$intdatey == sample_period[[i]]))
}

# number of observations
nrow(usoc)

# number of unique observations
library(plyr)
nrow(ddply(usoc,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# CLEANING FOR CONTINUOUSLY EMPLOYED RESPONDENTS
# create variable that gives the year of interview if the respondent is in paid employment
usoc$ce2016 <- usoc$jbstat_2*usoc$intdatey

library(data.table)
usoc <- data.table(usoc)
usoc[, dce2016 := c(NA, diff(ce2016)), by = id] # create differenced ce2016 variable
# if the varibale has a value != {1,2} then this indicates that the respondent has had a break in employment
# between interviews

# find duplicate observations and delete them
usoc$unique_id <- paste(usoc$id, usoc$intdatey) # concatenate to make unique ID
usoc$duplicate = duplicated(usoc$unique_id) # generate the duplicate variable
subset(usoc, duplicate=="TRUE") # find the duplicate
usoc_1 <- subset(usoc, usoc$duplicate == "FALSE") # remove duplicates
usoc_1 <- subset(usoc_1, select = -c(unique_id, duplicate)) # remove unique_id and duplicate columns

# remove households not in continuous employment across the sample
library(tidyverse)
usoc_1 <- usoc_1 %>%
  mutate_all(as.numeric) %>%
  group_by(id) %>%
  filter(id %in% id[any(dce2016 == 1) & any(dce2016 == 2)]) 

# number of respondents with changes in labour force status from paid employmnent
# count individuals moving from not in paid emp to paid emp
x <- count(usoc_1, usoc_1$dce2016 > 2)
x1 <- subset(x, x$`usoc_1$dce2016 > 2` == "TRUE") 
x1 <- data.frame(x1) # convert into data frame object
count(x1, x1$n >= 1) # count their number
x1id <- x1$id # convert ids to be removed into vectors
# data set containing all removed observations
usoc_x <- usoc_1 %>%
  mutate_all(as.numeric) %>%
  group_by(id) %>%
  filter(id %in% id[any(dce2016 > 2)]) 

usoc_x <- data.frame(usoc_x)

# counting observations by year
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_x, usoc_x$intdatey == sample_period[[i]]))
}

# total of observations
nrow(usoc_x)

# number of unique observations
library(plyr)
nrow(ddply(usoc_x,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# count individuals moving from paid emp to not in paid emp
y <- count(usoc_1, usoc_1$dce2016 <= 0)
y1 <- subset(y, y$`usoc_1$dce2016 <= 0` == "TRUE") 
y1 <- data.frame(y1)
count(y1, y1$n >= 1) 
y1id <- y1$id
# data set containing all removed observations
usoc_y <- usoc_1 %>%
  mutate_all(as.numeric) %>%
  group_by(id) %>%
  filter(id %in% id[any(dce2016 <= 0)]) 

usoc_y <- data.frame(usoc_y)

# counting observations by year
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_y, usoc_y$intdatey == sample_period[[i]]))
}

# total of observations
nrow(usoc_y)

# number of unique observations
library(plyr)
nrow(ddply(usoc_y,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# CREATE SUBSET SUCH THAT ALL IDs ARE IN CONTINUOUS PAID EMPLOYMENT
usoc_1 <- data.frame(usoc_1)

for (i in seq_along(x1id)) {
  usoc_1 <- usoc_1[usoc_1$id != x1id[[i]], ]
} # remove households moving from not in paid emp to paid emp

for (i in seq_along(y1id)) {
  usoc_1 <- usoc_1[usoc_1$id != y1id[[i]], ]
} # remove households moving from paid emp to not in paid emp

# cleanup 
rm(list = ls(pattern = "x"))
rm(list = ls(pattern = "y"))

# counting observations by year
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_1, usoc_1$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_1)

# number of unique observations
library(plyr)
nrow(ddply(usoc_1,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# REMOVE OBSERVATIONS WHERE RESPONDENT HASN'T REPORTED WHETHER THEY SAVE OR NOT
# this functions also removes NA values
usoc_2 <- subset(usoc_1, usoc_1$save_d >= 0)

# counting observations
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_2, usoc_2$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_2)

# number of unique observations
library(plyr)
nrow(ddply(usoc_2,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# CLEANING FOR SAVINGS >= 0
# where save == 2 then replace value in saved with 0
usoc_2$saved[usoc_2$save == 2] <- 0

# clean for savings >= 0
usoc_3 <- subset(usoc_2, usoc_2$saved >= 0)

# counting observations
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_3, usoc_3$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_3)

# number of unique observations
library(plyr)
nrow(ddply(usoc_3,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# REMOVE RESPONDENTS WITH 0 OR NEGATIVE INCOME
usoc_4 <- subset(usoc_3, usoc_3$fihhmnlabnet > 0)

# counting observations
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_4, usoc_4$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_4)

# number of unique observations
library(plyr)
nrow(ddply(usoc_4,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# REOMOVE OBSERVATIONS WITH MISSING YEAR DATA
usoc_5 <- subset(usoc_4, usoc_4$intdatey > 0)

# counting observations
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_5, usoc_5$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_5)

# number of unique observations
library(plyr)
nrow(ddply(usoc_5,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# REMOVE OBSERVATIONS INTERVIEWED AFTER FEB 2020
# create dummy for observations taken before feb 2020
usoc_5$b4_covid <- ifelse(usoc_5$intdatey*10000 + usoc_5$intdatem*100 + usoc_5$intdated <= 20200131, 1, 0)

usoc_6 <- subset(usoc_5, usoc_5$b4_covid == 1)

# counting observations by year
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_6, usoc_6$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_6)

# number of unique observations
library(plyr)
nrow(ddply(usoc_6,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# REMOVES OBSERVATIONS WHERE REPORTED SAVINGS EXCEED NET LABOUR INCOME LESS NON DURABLE CONSUMPTION
usoc_6 <- subset(usoc_6, (usoc_6$saved)/(usoc_6$fihhmnlabnet - usoc_6$xpfood1g3) < 1)
usoc_6 <- subset(usoc_6, (usoc_6$saved)/(usoc_6$fihhmnlabnet) < 1)

# counting observations by year
for (i in seq_along(sample_period)) {
  print(sample_period[[i]])
  print(count(usoc_6, usoc_6$intdatey == sample_period[[i]]))
}

# total number of observation
nrow(usoc_6)

# number of unique observations
library(plyr)
nrow(ddply(usoc_6,~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# NUMBER OF OBSERVATIONS AND UNIQUE INDIVIDUALS BY REGIONS

# North East
for (i in seq_along(sample_period)) {
  print("North East")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 1), 
              subset(usoc_6, usoc_6$gor == 1)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 1))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 1),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# North West
for (i in seq_along(sample_period)) {
  print("North West")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 2), 
              subset(usoc_6, usoc_6$gor == 2)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 2))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 2),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# Yorkshire and the Humber
for (i in seq_along(sample_period)) {
  print("Yorkshire and the Humber")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 3), 
              subset(usoc_6, usoc_6$gor == 3)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 3))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 3),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# East Midlands
for (i in seq_along(sample_period)) {
  print("East Midlands")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 4), 
              subset(usoc_6, usoc_6$gor == 4)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 4))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 4),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# West Midlands
for (i in seq_along(sample_period)) {
  print("West Midlands")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 5), 
              subset(usoc_6, usoc_6$gor == 5)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 5))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 5),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# East of England
for (i in seq_along(sample_period)) {
  print("East of England")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 6), 
              subset(usoc_6, usoc_6$gor == 6)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 6))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 6),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# London
for (i in seq_along(sample_period)) {
  print("London")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 7), 
              subset(usoc_6, usoc_6$gor == 7)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 7))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 7),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# South East
for (i in seq_along(sample_period)) {
  print("South East")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 8), 
              subset(usoc_6, usoc_6$gor == 8)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 8))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 8),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# South West
for (i in seq_along(sample_period)) {
  print("South West")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 9), 
              subset(usoc_6, usoc_6$gor == 9)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 9))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 9),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# Wales 
for (i in seq_along(sample_period)) {
  print("Wales")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 10), 
              subset(usoc_6, usoc_6$gor == 10)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 10))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 10),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# Scotland
for (i in seq_along(sample_period)) {
  print("Scotland")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 11), 
              subset(usoc_6, usoc_6$gor == 11)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 11))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 11),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

# Nothern Ireland
for (i in seq_along(sample_period)) {
  print("Northern Ireland")
  print(sample_period[[i]])
  print(count(subset(usoc_6, usoc_6$gor == 12), 
              subset(usoc_6, usoc_6$gor == 12)$intdatey == sample_period[[i]]))
}

# total observations
nrow(subset(usoc_6, usoc_6$gor == 12))

# number of unique observations
library(plyr)
nrow(ddply(subset(usoc_6, usoc_6$gor == 12),~id,summarise,number_of_distinct_orders=length(unique(age))))
detach("package:plyr", unload=TRUE)

### DESCPRIPTIVE STATISTICS

# create variable of savings as a proportion of labour income
usoc_6$sir <- usoc_6$saved/usoc_6$fihhmnlabnet

library(astsa)
tsplot(usoc_6$sir) # include this to improve formatting of the plots below

# histogram of savings rate
# North East
hist(usoc_6[usoc_6$gor == 1,]$sir, xlim = c(0,0.5), main = " ", xlab = "Savings rate", breaks = 10)
box(which = "plot", lty = "solid")
# North West
hist(usoc_6[usoc_6$gor == 2,]$sir, xlim = c(0,0.6), main = "", xlab = "Savings rate", breaks = 12)
box(which = "plot", lty = "solid")
# Yorkshire and The Humber
hist(usoc_6[usoc_6$gor == 3,]$sir, xlim = c(0,0.8), main = "", xlab = "Savings rate", breaks = 16)
box(which = "plot", lty = "solid")
# East Midlands
hist(usoc_6[usoc_6$gor == 4,]$sir, xlim = c(0,0.8), main = "", xlab = "Savings rate", breaks = 16)
box(which = "plot", lty = "solid")
# West Midlands
hist(usoc_6[usoc_6$gor == 5,]$sir, xlim = c(0,0.7), main = "", xlab = "Savings rate", breaks = 14)
box(which = "plot", lty = "solid")
# East of England
hist(usoc_6[usoc_6$gor == 6,]$sir, xlim = c(0,0.7), main = "", xlab = "Savings rate", breaks = 14)
box(which = "plot", lty = "solid")
# London
hist(usoc_6[usoc_6$gor == 7,]$sir, xlim = c(0,0.7), main = "", xlab = "Savings rate", breaks = 14)
box(which = "plot", lty = "solid")
# South East
hist(usoc_6[usoc_6$gor == 8,]$sir, xlim = c(0,1), main = "", xlab = "Savings rate", breaks = 20)
box(which = "plot", lty = "solid")
# south West
hist(usoc_6[usoc_6$gor == 9,]$sir, xlim = c(0,0.8), main = "", xlab = "Savings rate", breaks = 16)
box(which = "plot", lty = "solid")
# Wales
hist(usoc_6[usoc_6$gor == 10,]$sir, xlim = c(0,0.4), main = "", xlab = "Savings rate", breaks = 8)
box(which = "plot", lty = "solid")
# Scotland
hist(usoc_6[usoc_6$gor == 11,]$sir, xlim = c(0,0.9), main = "", xlab = "Savings rate", breaks = 18)
box(which = "plot", lty = "solid")
# Northern Ireland
hist(usoc_6[usoc_6$gor == 12,]$sir, xlim = c(0,0.7), main = "", xlab = "Savings rate", breaks = 17)
box(which = "plot", lty = "solid")

# All
library(moments)
mean(usoc_6$save_d) # proportion of people who save
mean(usoc_6$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6$sir) # median proportion of income saved
sd(usoc_6$sir) # standard deviation of proportion of income saved
skewness(usoc_6$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$saved > 0,]$saved) # average monthly savings for those who save

# North East
mean(usoc_6[usoc_6$gor == 1,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 1,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 1,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 1,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 1,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 1,][usoc_6[usoc_6$gor == 1,]$saved > 0,]$saved) # average monthly savings for those who save

# North West
mean(usoc_6[usoc_6$gor == 2,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 2,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 2,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 2,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 2,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 2,][usoc_6[usoc_6$gor == 2,]$saved > 0,]$saved) # average monthly savings for those who save

# Yorkshire and The Humber
mean(usoc_6[usoc_6$gor == 3,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 3,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 3,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 3,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 3,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 3,][usoc_6[usoc_6$gor == 3,]$saved > 0,]$saved) # average monthly savings for those who save

# East Midlands
mean(usoc_6[usoc_6$gor == 4,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 4,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 4,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 4,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 4,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 4,][usoc_6[usoc_6$gor == 4,]$saved > 0,]$saved) # average monthly savings for those who save

# West Midlands
mean(usoc_6[usoc_6$gor == 5,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 5,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 5,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 5,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 5,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 5,][usoc_6[usoc_6$gor == 5,]$saved > 0,]$saved) # average monthly savings for those who save

# East of England
mean(usoc_6[usoc_6$gor == 6,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 6,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 6,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 6,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 6,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 6,][usoc_6[usoc_6$gor == 6,]$saved > 0,]$saved) # average monthly savings for those who save

# London
mean(usoc_6[usoc_6$gor == 7,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 7,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 7,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 7,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 7,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 7,][usoc_6[usoc_6$gor == 7,]$saved > 0,]$saved) # average monthly savings for those who save

# South East
mean(usoc_6[usoc_6$gor == 8,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 8,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 8,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 8,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 8,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 8,][usoc_6[usoc_6$gor == 8,]$saved > 0,]$saved) # average monthly savings for those who save

# South West
mean(usoc_6[usoc_6$gor == 9,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 9,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 9,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 9,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 9,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 9,][usoc_6[usoc_6$gor == 9,]$saved > 0,]$saved) # average monthly savings for those who save

# Wales
mean(usoc_6[usoc_6$gor == 10,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 10,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 10,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 10,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 10,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 10,][usoc_6[usoc_6$gor == 10,]$saved > 0,]$saved) # average monthly savings for those who save

# Scotland
mean(usoc_6[usoc_6$gor == 11,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 11,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 11,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 11,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 11,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 11,][usoc_6[usoc_6$gor == 11,]$saved > 0,]$saved) # average monthly savings for those who save

# Northern Ireland
mean(usoc_6[usoc_6$gor == 12,]$save_d) # proportion of people who save
mean(usoc_6[usoc_6$gor == 12,]$sir) # average proportion of income saved (NB this includes 0 vlaues)
median(usoc_6[usoc_6$gor == 12,]$sir) # median proportion of income saved
sd(usoc_6[usoc_6$gor == 12,]$sir) # standard deviation of proportion of income saved
skewness(usoc_6[usoc_6$gor == 12,]$sir) # skewness of propotion of income saved
mean(usoc_6[usoc_6$gor == 12,][usoc_6[usoc_6$gor == 12,]$saved > 0,]$saved) # average monthly savings for those who save

### TREATMENT PERIOD
# MOTIVATING DIFFERENT SHOCK DEFINITIONS
library(readxl)
UK_Policy_Uncertainty_Data <- read_excel("~/Documents/Birkbeck work/Dissertation/UK_Policy_Uncertainty_Data.xlsx")
UK_Policy_Uncertainty_Data <- UK_Policy_Uncertainty_Data[-c(1:144, 277:277), ] # Remove rows up tp 2010 and after 2020
UKPU_2016 <- UK_Policy_Uncertainty_Data[-c(1:72, 109:132), ] # subset from 2016 to 2018 only

# load daily data
UK_Daily_Policy_Data <- read.csv("~/Documents/Birkbeck work/Dissertation/UK_Daily_Policy_Data.csv")
UKPUD_2016_a <- subset(UK_Daily_Policy_Data, UK_Daily_Policy_Data$year == 2016)
UKPUD_2016_b <- subset(UK_Daily_Policy_Data, UK_Daily_Policy_Data$year == 2017)
UKPUD_2016_c <- subset(UK_Daily_Policy_Data, UK_Daily_Policy_Data$year == 2018)
UKPUD_2016 <- rbind(UKPUD_2016_a, UKPUD_2016_b, UKPUD_2016_c) # subset of daily data from 2016 to mid 2017

# transform data so jan 2016 Index starts at 100
UKPU_2016$index <- (UKPU_2016$UK_EPU_Index/first(UKPU_2016$UK_EPU_Index))*100
UKPUD_2016$index <- (UKPUD_2016$daily_policy_index/first(UKPUD_2016$daily_policy_index))*100

# create time series object
policy_uncertainty <- ts(UK_Policy_Uncertainty_Data$UK_EPU_Index, start = 2010, frequency = 12) # monthly index
policy_uncertainty2016 <- ts(UKPU_2016$index, start = 2016, frequency = 12) # monthly index subset
daily_uncertainty2016 <- ts(UKPUD_2016$index, start = 2016, frequency = 365) # daily index subset

# Clean up
rm(list=ls(pattern = "UKPU_"))
rm(list=ls(pattern = "UKPUD_"))
rm(list=ls(pattern = "UK_"))

# plot data
# convert date into decimal form
library(lubridate)
eu_ref <- ymd("2016-06-24")
art_50 <- ymd("2017-03-29")
withagree_pub <- ymd("2018-11-14")
Q2_2016 <- ymd("2016-04-01")
Q3_2016 <- ymd("2016-07-01")
Q4_2016 <- ymd("2016-10-01")
Q2_2017 <- ymd("2017-04-01")

# Monthly EPU plot for sample period
library(astsa)
line_ticks_1 <- c(decimal_date(eu_ref))

# generate plot using astsa programme
# generating this plot improves formatting of the plot in base R graphics
tsplot(policy_uncertainty, ylab = "Economic Policy Uncertainty Index", lwd = 1) 

plot(policy_uncertainty, ylab = "Economic Policy Uncertainty Index", lwd = 1) # 2010-2020
text(x = line_ticks_1, par("usr")[3], adj = c(-2.6,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
abline(v=decimal_date(eu_ref))
legend('topleft', col = c("#000000"), lwd = 2,
       legend = c("Monthly index"), cex = 0.75)

# daily and monthly EPU plot
line_ticks_2 <- c(decimal_date(eu_ref), decimal_date(art_50))
line_ticks_3 <- c(decimal_date(withagree_pub))

tsplot(policy_uncertainty2016, lwd = 1, ylim = c(0,850), ylab = "Economic Policy Uncertainty Index")

plot(daily_uncertainty2016, lwd = 1, ylim = c(0,850), ylab = "Economic Policy Uncertainty Index", col = "#939393", xaxt = "n")
lines(policy_uncertainty2016, col = '#000000', lwd = 1)
axis(1, at = c(2016, 2017, 2018, 2019), 
     labels = c(2016, 2017, 2018, 2019))
text(x = line_ticks_2, par("usr")[3], adj = c(-2.1,1.5), 
     labels = c("EU Referendum", "Article 50"), srt = 90, cex = 0.65, xpd = TRUE)
text(x = line_ticks_3, par("usr")[3], adj = c(-0.5,1.5), 
     labels = c("Withdrawal Agreement published"), srt = 90, cex = 0.65, xpd = TRUE)
abline(v=decimal_date(eu_ref))
abline(v=decimal_date(art_50))
abline(v=decimal_date(withagree_pub))
legend('topright', col = c("#000000", "#939393"), lwd = 2,
       legend = c("Monthly index", "Daily Index"), cex = 0.75)

# defining shocks and pre-treatment periods
# set treatment period between EU Referendum and Article 50 declaration
usoc_6$ref_dummy <- ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated <= 20160623, 0, 
                           ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated >= 20170329, 0, 1))

# pre treatment period
usoc_6$pre_ref_dummy <- ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated <= 20160623, 1, 0)

# from 24th June 2016 to 14th November 2018 (Withdrawal agreement agreed and published)
usoc_6$withagree_dummy <- ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated <= 20160623, 0, 
                                 ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated >= 20181115, 0, 1))

# from 24th June 2016 to the end of the dataset (When UK leaves the EU)
usoc_6$leaveeu_dummy <- ifelse(usoc_6$intdatey*10000 + usoc_6$intdatem*100 + usoc_6$intdated <= 20160623, 0, 1)

### CHARACTERISTICS OF THE SAMPLE IN THE TREATMENT PERIOD
# vector of region-treatment period interaction terms
trtment <- cbind(usoc_6$ref_dummy*usoc_6$gor_1, usoc_6$ref_dummy*usoc_6$gor_2, usoc_6$ref_dummy*usoc_6$gor_3, usoc_6$ref_dummy*usoc_6$gor_4, 
                 usoc_6$ref_dummy*usoc_6$gor_5, usoc_6$ref_dummy*usoc_6$gor_6, usoc_6$ref_dummy*usoc_6$gor_7, usoc_6$ref_dummy*usoc_6$gor_8, 
                 usoc_6$ref_dummy*usoc_6$gor_9, usoc_6$ref_dummy*usoc_6$gor_10, usoc_6$ref_dummy*usoc_6$gor_11, usoc_6$ref_dummy*usoc_6$gor_12)

# pre tretment-region interaction terms (these are for later on)
pre_trtment <- cbind(usoc_6$pre_ref_dummy*usoc_6$gor_1, usoc_6$pre_ref_dummy*usoc_6$gor_2, usoc_6$pre_ref_dummy*usoc_6$gor_3, usoc_6$pre_ref_dummy*usoc_6$gor_4, 
                     usoc_6$pre_ref_dummy*usoc_6$gor_5, usoc_6$pre_ref_dummy*usoc_6$gor_6, usoc_6$pre_ref_dummy*usoc_6$gor_7, usoc_6$pre_ref_dummy*usoc_6$gor_8, 
                     usoc_6$pre_ref_dummy*usoc_6$gor_9, usoc_6$pre_ref_dummy*usoc_6$gor_10, usoc_6$pre_ref_dummy*usoc_6$gor_11, usoc_6$pre_ref_dummy*usoc_6$gor_12)

# vector of region dummies
region <- cbind(usoc_6$gor_1, usoc_6$gor_2, usoc_6$gor_3, usoc_6$gor_4, 
                usoc_6$gor_5, usoc_6$gor_6, usoc_6$gor_7, usoc_6$gor_8,
                usoc_6$gor_9, usoc_6$gor_10, usoc_6$gor_11, usoc_6$gor_12)

# regress number of kids on treatment period
library(plm)
summary(plm(usoc_6$nkids ~ trtment + region + usoc_6$ref_dummy, data = usoc_6, model = "pooling")) 

# number in employment in the household on treatment period
summary(plm(usoc_6$nemp ~ trtment + region + usoc_6$ref_dummy, data = usoc_6, model = "pooling")) 

# household size on treatment period
summary(plm(usoc_6$hhsize ~ trtment + region + usoc_6$ref_dummy, data = usoc_6, model = "pooling")) 

# job security on treatment period
usoc_6 <- data.frame(usoc_6)
# create new jbsec where negatives are NA
usoc_6$cjbsec <- ifelse(usoc_6$jbsec < 0, NA, usoc_6$jbsec)
library(rms)
Xvar <- c("ref_dummy")
ddist<- datadist(Xvar)
options(datadist='ddist') #ordered logit model coefficients
print(lrm(usoc_6$cjbsec ~ trtment + region , data = usoc_6))
detach("package:rms", unload=TRUE)

### TRENDS IN SAVINGS BEHAVIOUR
usoc_6 <- pdata.frame(usoc_6, index = c("id", "intdatey")) # create panel data frame
usoc_6$sir <- usoc_6$saved/usoc_6$fihhmnlabnet

## create time series of save and save variables
# create region vector
Region <- c("North East", "North West", "Yorkshire and The Humber", 
            "East Midlands", "West Midlands", "East of England", 
            "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland") 

# create export data frame
savings <- data.frame(Region)

# create yearly subsets
usoc_6_2010 <- subset(usoc_6, usoc_6$intdatey == 2010)
usoc_6_2011 <- subset(usoc_6, usoc_6$intdatey == 2011)
usoc_6_2012 <- subset(usoc_6, usoc_6$intdatey == 2012)
usoc_6_2013 <- subset(usoc_6, usoc_6$intdatey == 2013)
usoc_6_2014 <- subset(usoc_6, usoc_6$intdatey == 2014)
usoc_6_2015 <- subset(usoc_6, usoc_6$intdatey == 2015)
usoc_6_2016 <- subset(usoc_6, usoc_6$intdatey == 2016)
usoc_6_2017 <- subset(usoc_6, usoc_6$intdatey == 2017)
usoc_6_2018 <- subset(usoc_6, usoc_6$intdatey == 2018)
usoc_6_2019 <- subset(usoc_6, usoc_6$intdatey == 2019)

savings$save_2010 <- ifelse(savings$Region == "North East", mean(usoc_6_2010[usoc_6_2010$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2010[usoc_6_2010$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2010[usoc_6_2010$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2010[usoc_6_2010$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2010[usoc_6_2010$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2010[usoc_6_2010$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2010[usoc_6_2010$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2010[usoc_6_2010$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2010[usoc_6_2010$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2010[usoc_6_2010$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2010[usoc_6_2010$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2010[usoc_6_2010$gor == 12, ]$save_d), 0))))))))))))

savings$save_2011 <- ifelse(savings$Region == "North East", mean(usoc_6_2011[usoc_6_2011$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2011[usoc_6_2011$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2011[usoc_6_2011$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2011[usoc_6_2011$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2011[usoc_6_2011$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2011[usoc_6_2011$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2011[usoc_6_2011$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2011[usoc_6_2011$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2011[usoc_6_2011$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2011[usoc_6_2011$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2011[usoc_6_2011$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2011[usoc_6_2011$gor == 12, ]$save_d), 0))))))))))))

savings$save_2012 <- ifelse(savings$Region == "North East", mean(usoc_6_2012[usoc_6_2012$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2012[usoc_6_2012$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2012[usoc_6_2012$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2012[usoc_6_2012$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2012[usoc_6_2012$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2012[usoc_6_2012$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2012[usoc_6_2012$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2012[usoc_6_2012$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2012[usoc_6_2012$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2012[usoc_6_2012$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2012[usoc_6_2012$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2012[usoc_6_2012$gor == 12, ]$save_d), 0))))))))))))

savings$save_2013 <- ifelse(savings$Region == "North East", mean(usoc_6_2013[usoc_6_2013$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2013[usoc_6_2013$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2013[usoc_6_2013$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2013[usoc_6_2013$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2013[usoc_6_2013$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2013[usoc_6_2013$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2013[usoc_6_2013$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2013[usoc_6_2013$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2013[usoc_6_2013$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2013[usoc_6_2013$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2013[usoc_6_2013$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2013[usoc_6_2013$gor == 12, ]$save_d), 0))))))))))))

savings$save_2014 <- ifelse(savings$Region == "North East", mean(usoc_6_2014[usoc_6_2014$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2014[usoc_6_2014$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2014[usoc_6_2014$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2014[usoc_6_2014$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2014[usoc_6_2014$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2014[usoc_6_2014$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2014[usoc_6_2014$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2014[usoc_6_2014$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2014[usoc_6_2014$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2014[usoc_6_2014$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2014[usoc_6_2014$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2014[usoc_6_2014$gor == 12, ]$save_d), 0))))))))))))

savings$save_2015 <- ifelse(savings$Region == "North East", mean(usoc_6_2015[usoc_6_2015$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2015[usoc_6_2015$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2015[usoc_6_2015$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2015[usoc_6_2015$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2015[usoc_6_2015$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2015[usoc_6_2015$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2015[usoc_6_2015$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2015[usoc_6_2015$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2015[usoc_6_2015$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2015[usoc_6_2015$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2015[usoc_6_2015$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2015[usoc_6_2015$gor == 12, ]$save_d), 0))))))))))))

savings$save_2016 <- ifelse(savings$Region == "North East", mean(usoc_6_2016[usoc_6_2016$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2016[usoc_6_2016$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2016[usoc_6_2016$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2016[usoc_6_2016$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2016[usoc_6_2016$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2016[usoc_6_2016$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2016[usoc_6_2016$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2016[usoc_6_2016$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2016[usoc_6_2016$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2016[usoc_6_2016$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2016[usoc_6_2016$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2016[usoc_6_2016$gor == 12, ]$save_d), 0))))))))))))

savings$save_2017 <- ifelse(savings$Region == "North East", mean(usoc_6_2017[usoc_6_2017$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2017[usoc_6_2017$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2017[usoc_6_2017$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2017[usoc_6_2017$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2017[usoc_6_2017$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2017[usoc_6_2017$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2017[usoc_6_2017$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2017[usoc_6_2017$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2017[usoc_6_2017$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2017[usoc_6_2017$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2017[usoc_6_2017$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2017[usoc_6_2017$gor == 12, ]$save_d), 0))))))))))))

savings$save_2018 <- ifelse(savings$Region == "North East", mean(usoc_6_2018[usoc_6_2018$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2018[usoc_6_2018$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2018[usoc_6_2018$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2018[usoc_6_2018$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2018[usoc_6_2018$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2018[usoc_6_2018$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2018[usoc_6_2018$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2018[usoc_6_2018$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2018[usoc_6_2018$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2018[usoc_6_2018$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2018[usoc_6_2018$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2018[usoc_6_2018$gor == 12, ]$save_d), 0))))))))))))

savings$save_2019 <- ifelse(savings$Region == "North East", mean(usoc_6_2019[usoc_6_2019$gor == 1, ]$save_d),
                            ifelse(savings$Region == "North West", mean(usoc_6_2019[usoc_6_2019$gor == 2, ]$save_d), 
                                   ifelse(savings$Region == "Yorkshire and The Humber", mean(usoc_6_2019[usoc_6_2019$gor == 3, ]$save_d), 
                                          ifelse(savings$Region == "East Midlands", mean(usoc_6_2019[usoc_6_2019$gor == 4, ]$save_d), 
                                                 ifelse(savings$Region == "West Midlands", mean(usoc_6_2019[usoc_6_2019$gor == 5, ]$save_d), 
                                                        ifelse(savings$Region == "East of England", mean(usoc_6_2019[usoc_6_2019$gor == 6, ]$save_d), 
                                                               ifelse(savings$Region == "London", mean(usoc_6_2019[usoc_6_2019$gor == 7, ]$save_d), 
                                                                      ifelse(savings$Region == "South East", mean(usoc_6_2019[usoc_6_2019$gor == 8, ]$save_d), 
                                                                             ifelse(savings$Region == "South West", mean(usoc_6_2019[usoc_6_2019$gor == 9, ]$save_d), 
                                                                                    ifelse(savings$Region == "Wales", mean(usoc_6_2019[usoc_6_2019$gor == 10, ]$save_d), 
                                                                                           ifelse(savings$Region == "Scotland", mean(usoc_6_2019[usoc_6_2019$gor == 11, ]$save_d), 
                                                                                                  ifelse(savings$Region == "Northern Ireland", mean(usoc_6_2019[usoc_6_2019$gor == 12, ]$save_d), 0))))))))))))

savings$psaved_2010 <- ifelse(savings$Region == "North East", mean((usoc_6_2010[usoc_6_2010$gor == 1, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2010[usoc_6_2010$gor == 2, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2010[usoc_6_2010$gor == 3, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2010[usoc_6_2010$gor == 4, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2010[usoc_6_2010$gor == 5, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2010[usoc_6_2010$gor == 6, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2010[usoc_6_2010$gor == 7, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2010[usoc_6_2010$gor == 8, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2010[usoc_6_2010$gor == 9, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2010[usoc_6_2010$gor == 10, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2010[usoc_6_2010$gor == 11, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2010[usoc_6_2010$gor == 12, ]$saved)/(usoc_6_2010[usoc_6_2010$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2011 <- ifelse(savings$Region == "North East", mean((usoc_6_2011[usoc_6_2011$gor == 1, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2011[usoc_6_2011$gor == 2, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2011[usoc_6_2011$gor == 3, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2011[usoc_6_2011$gor == 4, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2011[usoc_6_2011$gor == 5, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2011[usoc_6_2011$gor == 6, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2011[usoc_6_2011$gor == 7, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2011[usoc_6_2011$gor == 8, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2011[usoc_6_2011$gor == 9, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2011[usoc_6_2011$gor == 10, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2011[usoc_6_2011$gor == 11, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2011[usoc_6_2011$gor == 12, ]$saved)/(usoc_6_2011[usoc_6_2011$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2012 <- ifelse(savings$Region == "North East", mean((usoc_6_2012[usoc_6_2012$gor == 1, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2012[usoc_6_2012$gor == 2, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2012[usoc_6_2012$gor == 3, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2012[usoc_6_2012$gor == 4, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2012[usoc_6_2012$gor == 5, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2012[usoc_6_2012$gor == 6, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2012[usoc_6_2012$gor == 7, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2012[usoc_6_2012$gor == 8, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2012[usoc_6_2012$gor == 9, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2012[usoc_6_2012$gor == 10, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2012[usoc_6_2012$gor == 11, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2012[usoc_6_2012$gor == 12, ]$saved)/(usoc_6_2012[usoc_6_2012$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2013 <- ifelse(savings$Region == "North East", mean((usoc_6_2013[usoc_6_2013$gor == 1, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2013[usoc_6_2013$gor == 2, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2013[usoc_6_2013$gor == 3, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2013[usoc_6_2013$gor == 4, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2013[usoc_6_2013$gor == 5, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2013[usoc_6_2013$gor == 6, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2013[usoc_6_2013$gor == 7, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2013[usoc_6_2013$gor == 8, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2013[usoc_6_2013$gor == 9, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2013[usoc_6_2013$gor == 10, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2013[usoc_6_2013$gor == 11, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2013[usoc_6_2013$gor == 12, ]$saved)/(usoc_6_2013[usoc_6_2013$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2014 <- ifelse(savings$Region == "North East", mean((usoc_6_2014[usoc_6_2014$gor == 1, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2014[usoc_6_2014$gor == 2, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2014[usoc_6_2014$gor == 3, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2014[usoc_6_2014$gor == 4, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2014[usoc_6_2014$gor == 5, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2014[usoc_6_2014$gor == 6, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2014[usoc_6_2014$gor == 7, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2014[usoc_6_2014$gor == 8, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2014[usoc_6_2014$gor == 9, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2014[usoc_6_2014$gor == 10, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2014[usoc_6_2014$gor == 11, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2014[usoc_6_2014$gor == 12, ]$saved)/(usoc_6_2014[usoc_6_2014$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2015 <- ifelse(savings$Region == "North East", mean((usoc_6_2015[usoc_6_2015$gor == 1, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2015[usoc_6_2015$gor == 2, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2015[usoc_6_2015$gor == 3, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2015[usoc_6_2015$gor == 4, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2015[usoc_6_2015$gor == 5, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2015[usoc_6_2015$gor == 6, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2015[usoc_6_2015$gor == 7, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2015[usoc_6_2015$gor == 8, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2015[usoc_6_2015$gor == 9, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2015[usoc_6_2015$gor == 10, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2015[usoc_6_2015$gor == 11, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2015[usoc_6_2015$gor == 12, ]$saved)/(usoc_6_2015[usoc_6_2015$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2016 <- ifelse(savings$Region == "North East", mean((usoc_6_2016[usoc_6_2016$gor == 1, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2016[usoc_6_2016$gor == 2, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2016[usoc_6_2016$gor == 3, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2016[usoc_6_2016$gor == 4, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2016[usoc_6_2016$gor == 5, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2016[usoc_6_2016$gor == 6, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2016[usoc_6_2016$gor == 7, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2016[usoc_6_2016$gor == 8, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2016[usoc_6_2016$gor == 9, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2016[usoc_6_2016$gor == 10, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2016[usoc_6_2016$gor == 11, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2016[usoc_6_2016$gor == 12, ]$saved)/(usoc_6_2016[usoc_6_2016$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2017 <- ifelse(savings$Region == "North East", mean((usoc_6_2017[usoc_6_2017$gor == 1, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2017[usoc_6_2017$gor == 2, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2017[usoc_6_2017$gor == 3, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2017[usoc_6_2017$gor == 4, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2017[usoc_6_2017$gor == 5, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2017[usoc_6_2017$gor == 6, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2017[usoc_6_2017$gor == 7, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2017[usoc_6_2017$gor == 8, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2017[usoc_6_2017$gor == 9, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2017[usoc_6_2017$gor == 10, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2017[usoc_6_2017$gor == 11, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2017[usoc_6_2017$gor == 12, ]$saved)/(usoc_6_2017[usoc_6_2017$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2018 <- ifelse(savings$Region == "North East", mean((usoc_6_2018[usoc_6_2018$gor == 1, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2018[usoc_6_2018$gor == 2, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2018[usoc_6_2018$gor == 3, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2018[usoc_6_2018$gor == 4, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2018[usoc_6_2018$gor == 5, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2018[usoc_6_2018$gor == 6, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2018[usoc_6_2018$gor == 7, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2018[usoc_6_2018$gor == 8, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2018[usoc_6_2018$gor == 9, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2018[usoc_6_2018$gor == 10, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2018[usoc_6_2018$gor == 11, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2018[usoc_6_2018$gor == 12, ]$saved)/(usoc_6_2018[usoc_6_2018$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

savings$psaved_2019 <- ifelse(savings$Region == "North East", mean((usoc_6_2019[usoc_6_2019$gor == 1, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 1, ]$fihhmnlabnet)),
                              ifelse(savings$Region == "North West", mean((usoc_6_2019[usoc_6_2019$gor == 2, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 2, ]$fihhmnlabnet)), 
                                     ifelse(savings$Region == "Yorkshire and The Humber", mean((usoc_6_2019[usoc_6_2019$gor == 3, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 3, ]$fihhmnlabnet)), 
                                            ifelse(savings$Region == "East Midlands", mean((usoc_6_2019[usoc_6_2019$gor == 4, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 4, ]$fihhmnlabnet)), 
                                                   ifelse(savings$Region == "West Midlands", mean((usoc_6_2019[usoc_6_2019$gor == 5, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 5, ]$fihhmnlabnet)), 
                                                          ifelse(savings$Region == "East of England", mean((usoc_6_2019[usoc_6_2019$gor == 6, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 6, ]$fihhmnlabnet)), 
                                                                 ifelse(savings$Region == "London", mean((usoc_6_2019[usoc_6_2019$gor == 7, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 7, ]$fihhmnlabnet)), 
                                                                        ifelse(savings$Region == "South East", mean((usoc_6_2019[usoc_6_2019$gor == 8, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 8, ]$fihhmnlabnet)), 
                                                                               ifelse(savings$Region == "South West", mean((usoc_6_2019[usoc_6_2019$gor == 9, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 9, ]$fihhmnlabnet)), 
                                                                                      ifelse(savings$Region == "Wales", mean((usoc_6_2019[usoc_6_2019$gor == 10, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 10, ]$fihhmnlabnet)), 
                                                                                             ifelse(savings$Region == "Scotland", mean((usoc_6_2019[usoc_6_2019$gor == 11, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 11, ]$fihhmnlabnet)), 
                                                                                                    ifelse(savings$Region == "Northern Ireland", mean((usoc_6_2019[usoc_6_2019$gor == 12, ]$saved)/(usoc_6_2019[usoc_6_2019$gor == 12, ]$fihhmnlabnet)), 0))))))))))))

rm(list = ls(pattern = "usoc_6_"))

# subsets
savings_save <- subset(savings, select = c(save_2010, save_2011, save_2012, save_2013, save_2014, save_2015, 
                                           save_2016, save_2017, save_2018, save_2019))
savings_psaved <- subset(savings, select = c(psaved_2010, psaved_2011, psaved_2012, psaved_2013, psaved_2014, 
                                             psaved_2015, psaved_2016, psaved_2017, psaved_2018, psaved_2019))

# transpose data
savings_save <- t(savings_save)
savings_psaved <- t(savings_psaved)

# rename columns
colnames(savings_save)[1] <- "North_East"
colnames(savings_save)[2] <- "North_West"
colnames(savings_save)[3] <- "Yorkshire_and_The_Humber"
colnames(savings_save)[4] <- "East_Midlands"
colnames(savings_save)[5] <- "West_Midlands"
colnames(savings_save)[6] <- "East_of_England"
colnames(savings_save)[7] <- "London"
colnames(savings_save)[8] <- "South_East"
colnames(savings_save)[9] <- "South_West"
colnames(savings_save)[10] <- "Wales"
colnames(savings_save)[11] <- "Scotland"
colnames(savings_save)[12] <- "Northern_Ireland"
colnames(savings_psaved)[1] <- "North_East"
colnames(savings_psaved)[2] <- "North_West"
colnames(savings_psaved)[3] <- "Yorkshire_and_The_Humber"
colnames(savings_psaved)[4] <- "East_Midlands"
colnames(savings_psaved)[5] <- "West_Midlands"
colnames(savings_psaved)[6] <- "East_of_England"
colnames(savings_psaved)[7] <- "London"
colnames(savings_psaved)[8] <- "South_East"
colnames(savings_psaved)[9] <- "South_West"
colnames(savings_psaved)[10] <- "Wales"
colnames(savings_psaved)[11] <- "Scotland"
colnames(savings_psaved)[12] <- "Northern_Ireland"

# create data frame
savings_save <- data.frame(savings_save)
savings_psaved <- data.frame(savings_psaved)

# cleanup
rm(Region)

# time series plots of savings profiles
# create time series objects
# whether saves
ne_s <- ts(savings_save$North_East, start = 2010, frequency = 1)
nw_s <- ts(savings_save$North_West, start = 2010, frequency = 1)
yh_s <- ts(savings_save$Yorkshire_and_The_Humber, start = 2010, frequency = 1)
em_s <- ts(savings_save$East_Midlands, start = 2010, frequency = 1)
wm_s <- ts(savings_save$West_Midlands, start = 2010, frequency = 1)
ee_s <- ts(savings_save$East_of_England, start = 2010, frequency = 1)
l_s <- ts(savings_save$London, start = 2010, frequency = 1)
se_s <- ts(savings_save$South_East, start = 2010, frequency = 1)
sw_s <- ts(savings_save$South_West, start = 2010, frequency = 1)
w_s <- ts(savings_save$Wales, start = 2010, frequency = 1)
s_s <- ts(savings_save$Scotland, start = 2010, frequency = 1)
ni_s<- ts(savings_save$Northern_Ireland, start = 2010, frequency = 1)
# proprotion of income saved
ne_ps <- ts(savings_psaved$North_East, start = 2010, frequency = 1)
nw_ps <- ts(savings_psaved$North_West, start = 2010, frequency = 1)
yh_ps <- ts(savings_psaved$Yorkshire_and_The_Humber, start = 2010, frequency = 1)
em_ps <- ts(savings_psaved$East_Midlands, start = 2010, frequency = 1)
wm_ps <- ts(savings_psaved$West_Midlands, start = 2010, frequency = 1)
ee_ps <- ts(savings_psaved$East_of_England, start = 2010, frequency = 1)
l_ps <- ts(savings_psaved$London, start = 2010, frequency = 1)
se_ps <- ts(savings_psaved$South_East, start = 2010, frequency = 1)
sw_ps <- ts(savings_psaved$South_West, start = 2010, frequency = 1)
w_ps <- ts(savings_psaved$Wales, start = 2010, frequency = 1)
s_ps <- ts(savings_psaved$Scotland, start = 2010, frequency = 1)
ni_ps<- ts(savings_psaved$Northern_Ireland, start = 2010, frequency = 1)

# PLOTS
library(astsa)
tsplot(ne_s, ylab = "Proportion reporting whether they save", ylim = c(0.2,0.9), col = "#000000", lwd = 1)

# plot of proportion reporting whether they save by region
plot(ne_s, ylab = "Proportion reporting whether they save", ylim = c(0.2,0.9), col = "#000000", lwd = 1)
lines(nw_s, col = "#bababa", lwd = 1)
lines(yh_s, col = "#005e99", lwd = 1)
lines(em_s, col = "#f25959", lwd = 1)
lines(wm_s, col = "#a92169", lwd = 1)
lines(ee_s, col = "#f6a705", lwd = 1)
lines(l_s, col = "#bbed01", lwd = 1)
lines(se_s, col = "#ee99ed", lwd = 1)
lines(sw_s, col = "#6e4e31", lwd = 1)
lines(w_s, col = "#2a098a", lwd = 1)
lines(s_s, col = "#d5b3a5", lwd = 1)
lines(ni_s, col = "#495103", lwd = 1)
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-0.1,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99", "#f25959", "#a92169", "#f6a705", 
                          "#bbed01", "#ee99ed", "#6e4e31", "#2a098a", "#d5b3a5", "#495103"), lwd = 1.4,
       legend = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", 
                  "West Midlands", "East of England", "London", "South East", "South West", 
                  "Wales", "Scotland", "Northern Ireland"), cex = 0.60, bg = "#ffffff", ncol = 2)

# plot of proportion of income saved by region
tsplot(ne_ps, ylab = "Mean proportion of income saved", ylim = c(0,0.2), col = "#000000", lwd = 1)

plot(ne_ps, ylab = "Mean savings rate", ylim = c(0,0.24), col = "#000000", lwd = 1)
lines(nw_ps, col = "#bababa", lwd = 1)
lines(yh_ps, col = "#005e99", lwd = 1)
lines(em_ps, col = "#f25959", lwd = 1)
lines(wm_ps, col = "#a92169", lwd = 1)
lines(ee_ps, col = "#f6a705", lwd = 1)
lines(l_ps, col = "#bbed01", lwd = 1)
lines(se_ps, col = "#ee99ed", lwd = 1)
lines(sw_ps, col = "#6e4e31", lwd = 1)
lines(w_ps, col = "#2a098a", lwd = 1)
lines(s_ps, col = "#d5b3a5", lwd = 1)
lines(ni_ps, col = "#495103", lwd = 1)
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-1.3,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99", "#f25959", "#a92169", "#f6a705", 
                          "#bbed01", "#ee99ed", "#6e4e31", "#2a098a", "#d5b3a5", "#495103"), lwd = 1.4,
       legend = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", 
                  "West Midlands", "East of England", "London", "South East", "South West", 
                  "Wales", "Scotland", "Northern Ireland"), cex = 0.60, bg = "#ffffff", ncol = 2)

# TESTS FOR COMMON TREND
library(plm)
usoc_6 <- data.frame(usoc_6)
# pre treatment dummy is "pre_ref_dummy"
# treatment dummy is "ref_dummy"

# create panel data object
usoc_6 <- pdata.frame(usoc_6, index = c("id", "intdatey"))

# vector of pre treatment dummy interaction with region dummy is "pre_trtment"
# vector of region-treatment period interaction terms is "trtment"

# estimating the model
trend_model1 <- plm(usoc_6$save_d ~ pre_trtment + trtment + region + ref_dummy + pre_ref_dummy, 
                    data = usoc_6, model = "within", effect = "twoways")
summary(trend_model1) # whether respondent saves

trend_model2 <- plm(usoc_6$sir ~ pre_trtment + trtment + region + ref_dummy + pre_ref_dummy,
                    data = usoc_6, model = "within", effect = "twoways")
summary(trend_model2) # proportion of income saved

# REGRESS SAVINGS TRAJECTORIES ON REGIONAL DUMMIES
# create differenced proportion of income saved
library(data.table)
usoc_6 <- data.table(usoc_6)
usoc_6[, dsir := c(NA, diff(sir)), by = id] 
usoc_6 <- pdata.frame(usoc_6, index = c("id", "intdatey"))

trajectory_model <- plm(dsir ~ pre_trtment + trtment + region + ref_dummy + pre_ref_dummy, 
                        data = usoc_6, model = "within", effect = "twoways")
summary(trajectory_model)

summary(plm(dsir ~ treatment, 
    data = usoc_6, model = "within", effect = "twoways"))

# compute average age in each region across the sample
one_to_twelve <- c(1:12)
for (i in seq_along(one_to_twelve)) {
  print(mean(usoc_6[usoc_6$gor == one_to_twelve[[i]],]$age))
}

mean(usoc_6$age)

# explaining the North East divergence in savings rates
summary(plm(log(usoc_6$fihhmnlabnet) ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # log income
summary(plm(usoc_6$age ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # age
summary(plm(usoc_6$tenure_6 ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # rented private unfurnished
summary(plm(usoc_6$tenure_7 ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # rented private furnished
summary(plm(usoc_6$tenure_3 ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # local authority rent
# cleaning hiqual variable
usoc_6$hiqual <- ifelse(usoc_6$hiqual < 1, NA, usoc_6$hiqual)
summary(plm(I(usoc_6$hiqual <= 3) ~ usoc_6$ref_dummy*usoc_6$gor_1, data = usoc_6, model = "within", effect = "twoways")) # in education passed age of 16

##########################
### STATISTICAL MODEL ####
##########################

### FINDING A GOOD INSTRUMENT
## EXPORT CONCENTRATION PLOTS
# import data
exports <- read.csv("~/Documents/Birkbeck work/Dissertation/data/exports.csv", header = TRUE) 
exports_exc <- read.csv("~/Documents/Birkbeck work/Dissertation/data/exports_exc.csv", header = TRUE) 

# time series objects 
ne_exc <- ts(exports_exc$North_East, start = 2013, frequency = 4)
nw_exc <- ts(exports_exc$North_West, start = 2013, frequency = 4)
yh_exc <- ts(exports_exc$Yorkshire_and_The_Humber, start = 2013, frequency = 4)
em_exc <- ts(exports_exc$East_Midlands, start = 2013, frequency = 4)
wm_exc <- ts(exports_exc$West_Midlands, start = 2013, frequency = 4)
ee_exc <- ts(exports_exc$East_of_England, start = 2013, frequency = 4)
l_exc <- ts(exports_exc$London, start = 2013, frequency = 4)
se_exc <- ts(exports_exc$South_East, start = 2013, frequency = 4)
sw_exc <- ts(exports_exc$South_West, start = 2013, frequency = 4)
w_exc <- ts(exports_exc$Wales, start = 2013, frequency = 4)
s_exc <- ts(exports_exc$Scotland, start = 2013, frequency = 4)
ni_exc <- ts(exports_exc$Northern_Ireland, start = 2013, frequency = 4)

library(astsa)
tsplot(ne_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.43,0.75), lwd = 1)

plot(ne_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.43,0.75), lwd = 1)
lines(nw_exc, col = "#bababa")
lines(yh_exc, col = "#005e99")
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-2.5,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99"), lwd = 1.4,
       legend = c("North East", "North West", "Yorkshire and the Humber"), cex = 0.75, bg = "#ffffff")

tsplot(em_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.39,0.68), lwd = 1)

plot(em_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.39,0.68), lwd = 1)
lines(wm_exc, col = "#bababa")
lines(ee_exc, col = "#005e99")
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-2.5,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99"), lwd = 1.4,
       legend = c("East Midlands", "West Midlands", "East of England"), cex = 0.75, bg = "#ffffff")

tsplot(l_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.37,0.63), lwd = 1)

plot(l_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.37,0.63), lwd = 1)
lines(se_exc, col = "#bababa")
lines(sw_exc, col = "#005e99")
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-2.5,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99"), lwd = 1.4,
       legend = c("London", "South East", "South West"), cex = 0.75, bg = "#ffffff")

tsplot(w_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.45,0.72), lwd = 1)

plot(w_exc, ylab = "EU export concentration", xlab = "Time", ylim = c(0.45,0.72), lwd = 1)
lines(s_exc, col = "#bababa")
lines(ni_exc, col = "#005e99")
abline(v=decimal_date(eu_ref))
text(x = line_ticks_1, par("usr")[3], adj = c(-2.5,2), 
     labels = c("EU Referendum"), srt = 90, cex = 0.65, xpd = TRUE)
legend('topleft', col = c("#000000", "#bababa", "#005e99"), lwd = 1.4,
       legend = c("Wales", "Scotland", "Northern Ireland"), cex = 0.75, bg = "#ffffff")

# export concentration in different regions
# North East
exports_exc[exports_exc$X == "excQ3_2016", ]$North_East
# North West
exports_exc[exports_exc$X == "excQ3_2016", ]$North_West
# Yorkshire and The Humber
exports_exc[exports_exc$X == "excQ3_2016", ]$Yorkshire_and_The_Humber
# East Midlands
exports_exc[exports_exc$X == "excQ3_2016", ]$East_Midlands
# West Midlands
exports_exc[exports_exc$X == "excQ3_2016", ]$West_Midlands
# East of England
exports_exc[exports_exc$X == "excQ3_2016", ]$East_of_England
# London
exports_exc[exports_exc$X == "excQ3_2016", ]$London
# South East
exports_exc[exports_exc$X == "excQ3_2016", ]$South_East
# South West
exports_exc[exports_exc$X == "excQ3_2016", ]$South_West
# Wales
exports_exc[exports_exc$X == "excQ3_2016", ]$Wales
# Scotland
exports_exc[exports_exc$X == "excQ3_2016", ]$Scotland
# Northern Ireland
exports_exc[exports_exc$X == "excQ3_2016", ]$Northern_Ireland

# add lags of export concentration data to sample
usoc_6$excQ2_2016 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ2_2016, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ2_2016, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ2_2016, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ2_2016, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ2_2016, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ2_2016, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ2_2016, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ2_2016, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ2_2016, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ2_2016, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ2_2016, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ2_2016, 0))))))))))))

usoc_6$excQ1_2016 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ1_2016, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ1_2016, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ1_2016, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ1_2016, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ1_2016, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ1_2016, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ1_2016, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ1_2016, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ1_2016, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ1_2016, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ1_2016, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ1_2016, 0))))))))))))

usoc_6$excQ4_2015 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ4_2015, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ4_2015, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ4_2015, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ4_2015, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ4_2015, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ4_2015, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ4_2015, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ4_2015, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ4_2015, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ4_2015, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ4_2015, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ4_2015, 0))))))))))))

usoc_6$excQ3_2015 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ3_2015, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ3_2015, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ3_2015, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ3_2015, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ3_2015, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ3_2015, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ3_2015, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ3_2015, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ3_2015, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ3_2015, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ3_2015, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ3_2015, 0))))))))))))

usoc_6$excQ2_2015 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ2_2015, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ2_2015, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ2_2015, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ2_2015, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ2_2015, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ2_2015, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ2_2015, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ2_2015, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ2_2015, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ2_2015, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ2_2015, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ2_2015, 0))))))))))))

usoc_6$excQ1_2015 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ1_2015, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ1_2015, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ1_2015, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ1_2015, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ1_2015, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ1_2015, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ1_2015, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ1_2015, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ1_2015, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ1_2015, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ1_2015, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ1_2015, 0))))))))))))

usoc_6$excQ4_2014 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ4_2014, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ4_2014, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ4_2014, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ4_2014, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ4_2014, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ4_2014, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ4_2014, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ4_2014, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ4_2014, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ4_2014, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ4_2014, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ4_2014, 0))))))))))))

usoc_6$excQ3_2014 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ3_2014, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ3_2014, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ3_2014, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ3_2014, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ3_2014, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ3_2014, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ3_2014, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ3_2014, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ3_2014, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ3_2014, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ3_2014, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ3_2014, 0))))))))))))

usoc_6$excQ2_2014 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ2_2014, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ2_2014, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ2_2014, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ2_2014, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ2_2014, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ2_2014, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ2_2014, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ2_2014, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ2_2014, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ2_2014, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ2_2014, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ2_2014, 0))))))))))))

usoc_6$excQ1_2014 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ1_2014, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ1_2014, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ1_2014, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ1_2014, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ1_2014, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ1_2014, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ1_2014, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ1_2014, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ1_2014, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ1_2014, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ1_2014, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ1_2014, 0))))))))))))

usoc_6$excQ4_2013 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ4_2013, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ4_2013, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ4_2013, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ4_2013, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ4_2013, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ4_2013, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ4_2013, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ4_2013, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ4_2013, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ4_2013, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ4_2013, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ4_2013, 0))))))))))))

usoc_6$excQ3_2013 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ3_2013, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ3_2013, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ3_2013, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ3_2013, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ3_2013, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ3_2013, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ3_2013, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ3_2013, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ3_2013, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ3_2013, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ3_2013, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ3_2013, 0))))))))))))

usoc_6$excQ2_2013 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ2_2013, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ2_2013, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ2_2013, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ2_2013, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ2_2013, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ2_2013, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ2_2013, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ2_2013, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ2_2013, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ2_2013, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ2_2013, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ2_2013, 0))))))))))))

usoc_6$excQ1_2013 <- ifelse(usoc_6$gor == 1, exports[exports$Region == "North East",]$excQ1_2013, 
                            ifelse(usoc_6$gor == 2, exports[exports$Region == "North West",]$excQ1_2013, 
                                   ifelse(usoc_6$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ1_2013, 
                                          ifelse(usoc_6$gor == 4, exports[exports$Region == "East Midlands",]$excQ1_2013, 
                                                 ifelse(usoc_6$gor == 5, exports[exports$Region == "West Midlands",]$excQ1_2013, 
                                                        ifelse(usoc_6$gor == 6, exports[exports$Region == "East of England",]$excQ1_2013, 
                                                               ifelse(usoc_6$gor == 7, exports[exports$Region == "London",]$excQ1_2013, 
                                                                      ifelse(usoc_6$gor == 8, exports[exports$Region == "South East",]$excQ1_2013, 
                                                                             ifelse(usoc_6$gor == 9, exports[exports$Region == "South West",]$excQ1_2013, 
                                                                                    ifelse(usoc_6$gor == 10, exports[exports$Region == "Wales",]$excQ1_2013, 
                                                                                           ifelse(usoc_6$gor == 11, exports[exports$Region == "Scotland",]$excQ1_2013, 
                                                                                                  ifelse(usoc_6$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ1_2013, 0))))))))))))
## regress dependent variables on instrument and lags
exc_lag <- cbind(usoc_6$excQ2_2016, usoc_6$excQ1_2016, usoc_6$excQ4_2015, usoc_6$excQ3_2015, usoc_6$excQ2_2015,
                 usoc_6$excQ1_2015, usoc_6$excQ4_2014, usoc_6$excQ3_2014, usoc_6$excQ2_2014, usoc_6$excQ1_2014,
                 usoc_6$excQ4_2013)

# create panel data frame
# regress whether saves on export concentration in Q3 2016 and lags
saves_on_exc <- plm(usoc_6$save_d ~ usoc_6$exposure + exc_lag, data = usoc_6, 
                    model = "within", effect = "twoways")
summary(saves_on_exc) 

# regress proportion of income saves on export concentration in Q3 2016 and lags
saved_on_exc <- plm(usoc_6$sir ~ usoc_6$exposure + exc_lag, data = usoc_6, 
                    model = "within", effect = "twoways")
summary(saved_on_exc) # proportion saved on lags of export concentration 

# NB it appears in both models that regional export concentration in Q3 2015 and Q2 2015
# have no explanatory power

# plot eu export concentration Q3 2015 and Q3 2016
plot(exports$excQ3_2016 ~ exports$excQ3_2015, pch = 20,
     ylab = "Regional EU export concentration Q3 2016", xlab = "Regional EU export concentration Q3 2015", cex = 0.6)
abline(lm(exports$excQ3_2016 ~ exports$excQ3_2013), col = "#939393")
text(x = c(0.5424009), par("usr")[3], adj = c(1.1,-42), 
     labels = c("North East"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4516917), par("usr")[3], adj = c(-0.1,-17), 
     labels = c("North West"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5513405), par("usr")[3], adj = c(1.05,-35.5), 
     labels = c("Yorkshire and the Humber"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5009865), par("usr")[3], adj = c(-0.1,-20), 
     labels = c("East Midlands"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.419757), par("usr")[3], adj = c(-0.1,-5), 
     labels = c("West Midlands"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5531011), par("usr")[3], adj = c(-0.05,-32), 
     labels = c("East of England"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4365372), par("usr")[3], adj = c(-0.2,-12), 
     labels = c("London"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4817334), par("usr")[3], adj = c(-0.1,-15), 
     labels = c("South East"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4472987), par("usr")[3], adj = c(-0.1,-2), 
     labels = c("South West"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5869157), par("usr")[3], adj = c(1.1,-43), 
     labels = c("Wales"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5105872), par("usr")[3], adj = c(-0.1,-10), 
     labels = c("Scotland"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.518257), par("usr")[3], adj = c(1.05,-33.5), 
     labels = c("Northern Ireland"), srt = 0, cex = 0.6, xpd = TRUE)


# instrument for export concentration in Q3 2015
usoc_6$instrument <- usoc_6$ref_dummy*usoc_6$excQ3_2015

### DO CHANGES IN FIXED CHARACTERISTICS AND JOB SECURITY AFFECT THE TREATMENT?
library(plm)

# compute differenced variables for fixed characteristics
# difference in number of children
library(data.table)
usoc_6 <- data.table(usoc_6)
usoc_6[, dnkids := c(NA, diff(nkids)), by = id] 
# difference household size
usoc_6[, dhhsize := c(NA, diff(hhsize)), by = id] 
# difference number in employment
usoc_6[, dnemp := c(NA, diff(nemp)), by = id] 
# difference in log income
usoc_6$log_y <- log(usoc_6$fihhmnlabnet)
usoc_6[, dlog_y := c(NA, diff(log_y)), by = id]
# difference housing tenure
usoc_6[, dtenure := c(NA, diff(tenure)), by = id] 
# create dummy that takes the value of 1 for change in housing tenure
usoc_6$dtenure_d <- ifelse(usoc_6$dtenure != 0, 1, 0)
# difference highest qualification
usoc_6[, dhiqual := c(NA, diff(hiqual)), by = id] 
# create dummy that is 1 when theres a change in housing tenure
usoc_6$dhiqual_d <- ifelse(usoc_6$dhiqual != 0, 1, 0)
# create new jbsec where negatives are NA
usoc_6$cjbsec <- ifelse(usoc_6$jbsec < 0, NA, usoc_6$jbsec)
# difference new job security variable
usoc_6[, dcjbsec := c(NA, diff(cjbsec)), by = id]
# create dummy for increases in job insecurity
usoc_6$dcjbsec_insec <- ifelse(usoc_6$dcjbsec < 0, 1, 0)
# create dummy for increase in job security
usoc_6$dcjbsec_sec <- ifelse(usoc_6$dcjbsec > 1, 1, 0)

# difference eu export concentration in Q3 2016
usoc_6$dinstrument <- ifelse(usoc_6$instrument > 0, usoc_6$instrument - usoc_6$excQ3_2014, 0)

# regress instrument on changes in household characteristics and in job security
usoc_6 <- pdata.frame(usoc_6, index = c("id", "intdatey"))
summary(plm(dinstrument ~ dnkids + dhhsize + dnemp + dlog_y + dtenure_d + dhiqual_d + dcjbsec_sec + dcjbsec_insec, 
            data = usoc_6, model = "within", effect = "twoways")) 

# what value does dinstrument take for every region?

for (i in seq_along(one_to_twelve)){
  print(mean(usoc_6[usoc_6$gor == one_to_twelve[[i]],]$excQ3_2015)-mean(usoc_6[usoc_6$gor == one_to_twelve[[i]],]$excQ3_2014))
}

#####################
### MAIN RESULTS ####
#####################

# create vectors of controls 
characteristics <- cbind(usoc_6$female, usoc_6$age, usoc_6$nkids, usoc_6$hhsize, usoc_6$nemp, log(usoc_6$fihhmnlabnet), 
           usoc_6$tenure_1, usoc_6$tenure_2, usoc_6$tenure_3, usoc_6$tenure_4, usoc_6$tenure_5, usoc_6$tenure_6, 
           usoc_6$tenure_7, usoc_6$tenure_8, 
           usoc_6$hiqual_1, usoc_6$hiqual_2, usoc_6$hiqual_3, usoc_6$hiqual_4, usoc_6$hiqual_5, usoc_6$hiqual_6, 
           usoc_6$hiqual_7, usoc_6$hiqual_8, usoc_6$hiqual_9)

jobsecurity <- cbind(usoc_6$jbsec_1, usoc_6$jbsec_2, usoc_6$jbsec_3, usoc_6$jbsec_4)

# BASELINE SPECIFICATION
# Dependent variable is whether respondent saves
baseline_saves_first <- plm(usoc_6$treatment ~ instrument + characteristics + jobsecurity, 
                           data = usoc_6, model = "within", effect = "twoways") # first stage of regression
summary(baseline_saves_first) # first stage regression

baseline_saves_1 <- plm(usoc_6$save_d ~ treatment | instrument + characteristics + jobsecurity,
                      data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saves_1) # with no controls

baseline_saves_2 <- plm(usoc_6$save_d ~ treatment + characteristics | instrument + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saves_2) # just household characteristics controls

baseline_saves_3 <- plm(usoc_6$save_d ~ treatment + characteristics + jobsecurity | instrument + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saves_3) # full model

# coefficients with arellano standard errors for linear probability model
coeftest(baseline_saves_first, vcovHC(baseline_saves_first, method = "arellano"))
coeftest(baseline_saves_2, vcovHC(baseline_saves_2, method = "arellano"))
coeftest(baseline_saves_3, vcovHC(baseline_saves_3, method = "arellano"))

# dependent variable is proportion of income saved

# first stage is identical

baseline_saved_1 <- plm(usoc_6$sir ~ treatment | instrument + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saved_1) # with no controls

baseline_saved_2 <- plm(usoc_6$sir ~ treatment + characteristics | instrument + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saved_2) # just household characteristics controls

baseline_saved_3 <- plm(usoc_6$sir ~ treatment + characteristics + jobsecurity | instrument + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saved_3) # full model

# coefficients with arellano standard errors for proportion od income saved model
coeftest(baseline_saved_2, vcovHC(baseline_saved_2, method = "arellano"))
coeftest(baseline_saved_3, vcovHC(baseline_saved_3, method = "arellano"))

# residual standard error for both models
sqrt(deviance(baseline_saves_3)/df.residual(baseline_saves_3)) # linear probability model
sqrt(deviance(baseline_saved_3)/df.residual(baseline_saved_3)) # savings rate model

# residual plots
residuals_saves <- residuals(baseline_saves_3)
fitted_saves <- fitted.values(baseline_saves_3)
residuals_saved <- residuals(baseline_saved_3)
fitted_saved <- fitted.values(baseline_saved_3)
residualplot_data <- data.frame(residuals_saves, fitted_saves, residuals_saved, fitted_saved)
# cleanup
rm(list = ls(pattern = "residuals_"))
rm(list = ls(pattern = "fitted_"))

# write csv to directory, import it back into R and delete file in directory
setwd("~/Documents/Birkbeck work/Dissertation/data")
write.csv(residualplot_data, "residualplot_data.csv")
residualplot_data <- read.csv("~/Documents/Birkbeck work/Dissertation/data/residualplot_data.csv")
fn <- "residualplot_data.csv"
if (file.exists(fn)) {
  file.remove(fn)
}
rm(fn)

# plot of baseline model of whether the respondent saves
tsplot(residualplot_data$residuals_saves ~ residualplot_data$fitted_saves, pch = 20, cex = 0.6,
       xlab = "Fitted values", ylab = "Residuals")
plot(residualplot_data$residuals_saves ~ residualplot_data$fitted_saves, pch = 20, cex = 0.6,
     xlab = "Fitted values", ylab = "Residuals")
# plot of baseline model of the proportion of income saved
plot(residualplot_data$residuals_saved ~ residualplot_data$fitted_saved, pch = 20, cex = 0.6,
     xlab = "Fitted values", ylab = "Residuals")

# MOTIVATING USE OF FIXED EFFECTS IV
library(AER)
usoc_6 <- data.frame(usoc_6)
# estimate linear probability model
baseline_saves_2sls <- ivreg(usoc_6$save_d ~ usoc_6$treatment + characteristics + jobsecurity | 1 + usoc_6$instrument + characteristics + jobsecurity)
summary(baseline_saves_2sls) # regression output
plot(residuals(baseline_saves_2sls) ~ fitted.values(baseline_saves_2sls), ylab = "Residuals", xlab = "Fitted values", pch = 20, cex = 0.6) # residual plot
# estimate savings rate model
baseline_saved_2sls <- ivreg(usoc_6$sir ~ usoc_6$treatment + characteristics + jobsecurity | 1 + usoc_6$instrument + characteristics + jobsecurity)
summary(baseline_saved_2sls) # regression output
plot(residuals(baseline_saved_2sls) ~ fitted.values(baseline_saved_2sls), ylab = "Residuals", xlab = "Fitted values", pch = 20, cex = 0.6) # residual plot
# test for instrument validity
iv_validtest_saves <- lm(residuals(baseline_saves_2sls) ~ usoc_6$excQ3_2015) # significant at the 10% level
summary(iv_validtest_saves)
iv_validtest_saved <- lm(residuals(baseline_saved_2sls) ~ usoc_6$excQ3_2015)
summary(iv_validtest_saved)


# DIAGNOSTIC TESTS FOR BASELINE SPECIFICATIONS
usoc_6 <- pdata.frame(usoc_6, index = c("id", "intdatey"))
library(lmtest)
# serial correlation
pbgtest(baseline_saves_3) # linear probability model
pbgtest(baseline_saved_3) # proportion of income saved
# heteroscedasticity
bptest(baseline_saves_3, data = usoc_6, studentize = F) # linear probability model
bptest(baseline_saved_3, data = usoc_6, studentize = F) # proportion of income saved

# unit root
library(tseries)
#adf.test(usoc_6$save_d, k=2) # linear probability model
#adf.test(usoc_6$sir, k=2) # proportion of income saved

# test for validity of instuemnts
usoc_6$saves_resid <- residuals(baseline_saves_3) # FE IV residuals for linear probability model
usoc_6$saved_resid <- residuals(baseline_saved_3) # residuals for the other model
# regress FE IV residuals on instrument
summary(plm(usoc_6$saves_resid ~ usoc_6$excQ3_2015,
            data = usoc_6, model = "within", effect = "twoways")) 
summary(plm(usoc_6$saved_resid ~ usoc_6$excQ3_2015,
            data = usoc_6, model = "within", effect = "twoways"))

# test for endogeneity
usoc_6$firststage_resid <- residuals(baseline_saves_first)
wuhausman_saves <- plm(usoc_6$save_d ~ treatment + instrument + firststage_resid + characteristics + jobsecurity,
            data = usoc_6, model = "within", effect = "twoways")
wuhausman_saved <- plm(usoc_6$sir ~ treatment + instrument + firststage_resid + characteristics + jobsecurity,
            data = usoc_6, model = "within", effect = "twoways")

# Wu-Hausman regression coefficients with arellano standard errors 
coeftest(wuhausman_saves, vcovHC(wuhausman_saves, method = "arellano")) # test of IV validity for linear prob model
coeftest(wuhausman_saved, vcovHC(wuhausman_saves, method = "arellano")) # test for the other model

# SPECIFICATIONS WITH DIFFERENT SHOCK DEFINITIONS
usoc_6$treatment_2 <- usoc_6$withagree_dummy*usoc_6$exposure

usoc_6$instrument_2 <- usoc_6$withagree_dummy*usoc_6$excQ3_2015

usoc_6$treatment_3 <- usoc_6$leaveeu_dummy*usoc_6$exposure

usoc_6$instrument_3 <- usoc_6$leaveeu_dummy*usoc_6$excQ3_2015

## Dependent variable is whether respondent saves
summary(baseline_saves_3) # shock defined from EU ref to Article 50 notice

baseline_saves_t2 <- plm(usoc_6$save_d ~ treatment_2 + characteristics + jobsecurity | instrument_2 + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saves_t2) # shock defined from EU ref to withdrawal agreement publishing

baseline_saves_t3 <- plm(usoc_6$save_d ~ treatment_3 + characteristics + jobsecurity | instrument_3 + characteristics + jobsecurity,
                        data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saves_t3) # shock defined from EU ref to day UK leaves the EU

coeftest(baseline_saves_t2, vcovHC(baseline_saves_t2, method = "arellano"))
coeftest(baseline_saves_t3, vcovHC(baseline_saves_t3, method = "arellano"))

## Dependent variable is proportion of income saved
summary(baseline_saved_3) # # shock defined from EU ref to Article 50 notice

baseline_saved_t2 <- plm(usoc_6$sir ~ treatment_2 + characteristics + jobsecurity | instrument_2 + characteristics + jobsecurity,
                         data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saved_t2) # shock defined from EU ref to withdrawal agreement publishing

baseline_saved_t3 <- plm(usoc_6$sir ~ treatment_3 + characteristics + jobsecurity | instrument_3 + characteristics + jobsecurity,
                         data = usoc_6, model = "within", effect = "twoways")
summary(baseline_saved_t3) # shock defined from EU ref to day UK leaves the EU

coeftest(baseline_saved_t2, vcovHC(baseline_saved_t2, method = "arellano"))
coeftest(baseline_saved_t3, vcovHC(baseline_saved_t3, method = "arellano"))

### DISCUSSION
# load regional EU referendum vote data into exports data
# number of leave votes
exports$no_leave <- ifelse(exports$Region == "North East", 778103, 
                           ifelse(exports$Region == "North West", 1966925, 
                                  ifelse(exports$Region == "Yorkshire and The Humber", 1580937, 
                                         ifelse(exports$Region == "East Midlands", 1475479, 
                                                ifelse(exports$Region == "West Midlands", 1755687, 
                                                       ifelse(exports$Region == "East of England", 1880367,
                                                              ifelse(exports$Region == "London", 1513232, 
                                                                     ifelse(exports$Region == "South East", 2567965, 
                                                                            ifelse(exports$Region == "South West", 1669711,
                                                                                   ifelse(exports$Region == "Wales", 854572, 
                                                                                          ifelse(exports$Region == "Scotland", 1018322, 
                                                                                                 ifelse(exports$Region == "Northern Ireland", 349442, 0))))))))))))

# number of valid votes
exports$no_validvotes <- ifelse(exports$Region == "North East", 1340698, 
                              ifelse(exports$Region == "North West", 3665945, 
                                     ifelse(exports$Region == "Yorkshire and The Humber", 2739235, 
                                            ifelse(exports$Region == "East Midlands", 2508515, 
                                                   ifelse(exports$Region == "West Midlands", 2962862, 
                                                          ifelse(exports$Region == "East of England", 3328983,
                                                                 ifelse(exports$Region == "London", 3776751, 
                                                                        ifelse(exports$Region == "South East", 4959683, 
                                                                               ifelse(exports$Region == "South West", 3172730,
                                                                                      ifelse(exports$Region == "Wales", 1626919, 
                                                                                             ifelse(exports$Region == "Scotland", 2679513, 
                                                                                                    ifelse(exports$Region == "Northern Ireland", 790149, 0))))))))))))

# proportion of leave votes
exports$prop_leave <- exports$no_leave/exports$no_validvotes

# plot proportion of leave vote by export conentration
library(astsa)
tsplot(exports$prop_leave ~ exports$excQ3_2016)

plot(exports$prop_leave ~ exports$excQ3_2016, ylab = "Regional leave vote share", 
     xlab = "Regional EU export concentration Q3 2016", pch = 20, cex = 0.6)
text(x = c(0.5911108), par("usr")[3], adj = c(1.1,-41.5), 
     labels = c("North East"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4949868), par("usr")[3], adj = c(-0.1,-32), 
     labels = c("North West"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5612835), par("usr")[3], adj = c(1.05,-14), 
     labels = c("Yorkshire and the Humber"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5085336), par("usr")[3], adj = c(-0.1,-41.5), 
     labels = c("East Midlands"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4446684), par("usr")[3], adj = c(-0.1,-41.8), 
     labels = c("West Midlands"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5494958), par("usr")[3], adj = c(-0.1,-37), 
     labels = c("East of England"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.474325), par("usr")[3], adj = c(-0.2,-5), 
     labels = c("London"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4853637), par("usr")[3], adj = c(-0.1,-28), 
     labels = c("South East"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4332357), par("usr")[3], adj = c(-0.1,-29), 
     labels = c("South West"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5924376), par("usr")[3], adj = c(1.1,-29), 
     labels = c("Wales"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.4635194), par("usr")[3], adj = c(1.1,-2), 
     labels = c("Scotland"), srt = 0, cex = 0.6, xpd = TRUE)
text(x = c(0.5594806), par("usr")[3], adj = c(1,-39.5), 
     labels = c("Northern Ireland"), srt = 0, cex = 0.6, xpd = TRUE)

summary(lm(exports$prop_leave ~ exports$excQ3_2016))


## linear probablity model regression voting leave on export concentration in Q3 2016
# clean data
usoc_6$voteeuref_d <- ifelse(usoc_6$voteeuref < 0, NA,
                             ifelse(usoc_6$voteeuref == 2, 1, 0))

# estimate linear prob model
summary(plm(usoc_6$voteeuref_d ~ usoc_6$exposure + characteristics + jobsecurity, data = usoc_6, model = "pooling"))
