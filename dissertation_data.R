#R syntax for Understanding Society data: the syntax in this 
#document loads the main individual and household responses 
#data for all waves, merges within the waves, merges all the 
#waves into a "wide" file, and reshapes to "long".  

#Preparation: get all the downloaded files from their wave specific 
#folders into one directory (folder). 
#(The syntax below uses the _indresp.dta and _hhresp.dta files.)  
#Then, set the path to that directory: 
#Note you need to use forward slash (/) and not backward slash (\) in filepaths

setwd("~/Documents/Birkbeck work/Dissertation/data")

# check.packages function: install and load multiple R packages.
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

#create lists of the _indresp and _hhresp files - substitute your path.
listind <- list.files("~/Documents/Birkbeck work/Dissertation/data", pattern = "*_indresp.dta")
listhh <- list.files("~/Documents/Birkbeck work/Dissertation/data", pattern = "*_hhresp.dta")

#load household data - in the second command, add/remove lines
#to get the variables you will want.
hh <- lapply(listhh, read_dta)
hh <- lapply(hh, function(x) x%>% select(ends_with("idp"),
                                         ends_with("_intdatey"),
                                         ends_with("_intdatem"),
                                         ends_with("_intdated"),
                                         ends_with("_gor_dv"),
                                         ends_with("_nkids_dv"),
                                         ends_with("_hhsize"),
                                         ends_with("_nemp_dv"),
                                         ends_with("_tenure_dv"),
                                         ends_with("fihhmnlabnet_dv"),
                                         ends_with("_xpfood1_g3")))
names(hh) <- gsub("\\.dta$", "", listhh)

#extract the individual dataframes from the list
list2env(hh, .GlobalEnv)

#load individual data - here as well, specify the needed variables 
#in the second command.  Note the use of a negative selection 
#(using -ends_with) to omit _ff_jbstat, which is otherwise 
#captured by the previous line.  
#Loading here will take some time.  Lines 47 to 59 might fail if 
#your computer does not have sufficient memory (RAM).  If that 
#happens, you  can read the files in the more conventional
#way, selecting the needed variables, for each wave separately.
ind <- lapply(listind, read_dta)
ind <- lapply(ind, function(x) x%>% select(ends_with("idp"),
                                           ends_with("_jbstat"),
                                           ends_with("_jbiindb_dv"),
                                           ends_with("_sex_dv"), 
                                           ends_with("_age_dv"), 
                                           ends_with("_hiqual_dv"),
                                           ends_with("_save"),
                                           ends_with("_saved"),
                                           ends_with("_euref"),
                                           ends_with("_voteeuref"),
                                           ends_with("_jbsec")))
names(ind) <- gsub("\\.dta$", "", listind)

list2env(ind, .GlobalEnv)

#merge individual and household data - I'm not aware that 
#it's possible to iterate this portion.  Expand as needed when 
#additional waves are released.
u1 <- merge(a_indresp, a_hhresp, by="a_hidp")
u2 <- merge(b_indresp, b_hhresp, by="b_hidp")
u3 <- merge(c_indresp, c_hhresp, by="c_hidp")
u4 <- merge(d_indresp, d_hhresp, by="d_hidp")
u5 <- merge(e_indresp, e_hhresp, by="e_hidp")
u6 <- merge(f_indresp, f_hhresp, by="f_hidp")
u7 <- merge(g_indresp, g_hhresp, by="g_hidp")
u8 <- merge(h_indresp, h_hhresp, by="h_hidp")
u9 <- merge(i_indresp, i_hhresp, by="i_hidp")
u10 <- merge(j_indresp, j_hhresp, by="j_hidp")

#merge the waves, resulting in "wide" data.  
#Expand the list for additional waves.
usoc <- Reduce(function(x,y) merge(x, y, by="pidp", all=TRUE), list(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10))

#cleanup
rm(list=ls(pattern = "[0-9]$"), ind, hh)
rm(list=ls(pattern = "resp"))

#remove _dv appearing at the end of any variable names - 
#the names need to have only one underscore character, 
#the one following the wave prefix.
names(usoc) <- str_replace(names(usoc), "_(dv)$", "")

#For reshape to long, the wave specifier needs to be at the 
#end (so, sex_a, not a_sex).  The lines below will move the 
#prefix to the end for the patterns of variable names used in 
#this example; they might not capture all possible patterns.
names(usoc) <- str_replace(names(usoc), "^([a-z])_([a-z]+)$", "\\2_\\1")
names(usoc) <- str_replace(names(usoc), "^([a-z]+)_([a-z]+)([0-9]+)([a-z]+)$", "\\2\\3\\4_\\1")
names(usoc) <- str_replace(names(usoc), "^([a-z]+)_([a-z]+)([0-9]+)$", "\\2\\3_\\1")
names(usoc) <- str_replace(names(usoc), "^([a-z]+)_([a-z]+)([0-9]+)_([a-z]+)([0-9])$", "\\2\\3\\4\\5_\\1")
names(usoc) <- str_replace(names(usoc), "^([a-z]+)_([a-z]+)_([a-z]+)$", "\\2\\3_\\1")

#export wide panel to csv into directory to get rid of label under variable names
write.csv(usoc,'usoc.csv')

# import csv
usoc <- read.csv("~/Documents/Birkbeck work/Dissertation/data/usoc.csv", header = TRUE)

#Reshape to long.  Make sure that pidp is the first column.  
usoc <- long_panel(usoc, prefix = "_", begin = "a", end = "j", label_location = "end")

# delete usoc.csv (wide panel) from directory
fn <- "usoc.csv"
if (file.exists(fn)) {
  file.remove(fn)
}

# export csv of long panel
write.csv(usoc,'usoc.csv')

# load csv of long panel (csvs are easier to work with than panel data objects)
# best to define the panel data object once data has been cleaned
usoc <- read.csv("~/Documents/Birkbeck work/Dissertation/data/usoc.csv", header = TRUE)

# delete (long panel) from directory
if (file.exists(fn)) {
  file.remove(fn)
}

# remove objects from environment
rm(list=ls(pattern = "list"))
rm(list=ls(pattern = "packages"))
rm(fn)

## CREATING DUMMY VARIABLES

# turn "save" variable into a dummy that turns on if respondent reports that they
# save and is zero oherwise
usoc$save_d <- ifelse(usoc$save == 1, 1,
                      ifelse(usoc$save == 2, 0, NA))

# job security dummies
usoc$jbsec_1 <- ifelse(usoc$jbsec == 1, 1, 0)
usoc$jbsec_2 <- ifelse(usoc$jbsec == 2, 1, 0)
usoc$jbsec_3 <- ifelse(usoc$jbsec == 3, 1, 0)
usoc$jbsec_4 <- ifelse(usoc$jbsec == 4, 1, 0)
usoc$jbsec_neg1 <- ifelse(usoc$jbsec == -1, 1, 0)
usoc$jbsec_neg2 <- ifelse(usoc$jbsec == -2, 1, 0)
usoc$jbsec_neg7 <- ifelse(usoc$jbsec == -7, 1, 0)
usoc$jbsec_neg8 <- ifelse(usoc$jbsec == -8, 1, 0)

# female dummy
usoc$female <- ifelse(usoc$sex == 2, 1, 0)

# labour force status dummies
usoc$jbstat_1 <- ifelse(usoc$jbstat == 1, 1, 0) # Self employed
usoc$jbstat_2 <- ifelse(usoc$jbstat == 2, 1, 0) # Paid employment (ft/pt)
usoc$jbstat_3 <- ifelse(usoc$jbstat == 3, 1, 0) # Unemployed
usoc$jbstat_4 <- ifelse(usoc$jbstat == 4, 1, 0) # Retired
usoc$jbstat_5 <- ifelse(usoc$jbstat == 5, 1, 0) # On materninty leave
usoc$jbstat_6 <- ifelse(usoc$jbstat == 6, 1, 0) # Family care or home
usoc$jbstat_7 <- ifelse(usoc$jbstat == 7, 1, 0) # Full-time student
usoc$jbstat_8 <- ifelse(usoc$jbstat == 8, 1, 0) # LT sick or disabled
usoc$jbstat_9 <- ifelse(usoc$jbstat == 9, 1, 0) # Govt training scheme
usoc$jbstat_10 <- ifelse(usoc$jbstat == 10, 1, 0) # Unpaid, family business
usoc$jbstat_11 <- ifelse(usoc$jbstat == 11, 1, 0) # On apprenticeship
usoc$jbstat_97 <- ifelse(usoc$jbstat == 97, 1, 0) # Doing something else
usoc$jbstat_neg1 <- ifelse(usoc$jbstat == -1, 1, 0) # dont know
usoc$jbstat_neg2 <- ifelse(usoc$jbstat == -2, 1, 0) # refusal
usoc$jbstat_neg9 <- ifelse(usoc$jbstat == -9, 1, 0) # missing

# current job industrial classification dummies
usoc$jbiindb_1 <- ifelse(usoc$jbiindb == 1, 1, 0) # Agriculture/Forestry
usoc$jbiindb_2 <- ifelse(usoc$jbiindb == 2, 1, 0) # Fisheries
usoc$jbiindb_3 <- ifelse(usoc$jbiindb == 3, 1, 0) # Energy/Water
usoc$jbiindb_4 <- ifelse(usoc$jbiindb == 4, 1, 0) # Mining
usoc$jbiindb_5 <- ifelse(usoc$jbiindb == 5, 1, 0) # Chemicals
usoc$jbiindb_6 <- ifelse(usoc$jbiindb == 6, 1, 0) # Synthetics
usoc$jbiindb_7 <- ifelse(usoc$jbiindb == 7, 1, 0) # Earth/Clay/Stone
usoc$jbiindb_8 <- ifelse(usoc$jbiindb == 8, 1, 0) # Iron/Steel
usoc$jbiindb_9 <- ifelse(usoc$jbiindb == 9, 1, 0) # Mechanical Engineering
usoc$jbiindb_10 <- ifelse(usoc$jbiindb == 10, 1, 0) # Electrical Engineering
usoc$jbiindb_11 <- ifelse(usoc$jbiindb == 11, 1, 0) # Wood/Paper/Prit
usoc$jbiindb_12 <- ifelse(usoc$jbiindb == 12, 1, 0) # Clothing/Textiles
usoc$jbiindb_13 <- ifelse(usoc$jbiindb == 13, 1, 0) # Food Industry
usoc$jbiindb_14 <- ifelse(usoc$jbiindb == 14, 1, 0) # Construction
usoc$jbiindb_15 <- ifelse(usoc$jbiindb == 15, 1, 0) # Constr. Relate
usoc$jbiindb_16 <- ifelse(usoc$jbiindb == 16, 1, 0) # Wholesale
usoc$jbiindb_17 <- ifelse(usoc$jbiindb == 17, 1, 0) # Trading Agents
usoc$jbiindb_18 <- ifelse(usoc$jbiindb == 18, 1, 0) # Retail
usoc$jbiindb_19 <- ifelse(usoc$jbiindb == 19, 1, 0) # Train System
usoc$jbiindb_20 <- ifelse(usoc$jbiindb == 20, 1, 0) # Communication/Entertainment
usoc$jbiindb_21 <- ifelse(usoc$jbiindb == 21, 1, 0) # Other Trans
usoc$jbiindb_22 <- ifelse(usoc$jbiindb == 22, 1, 0) # Financial Inst
usoc$jbiindb_23 <- ifelse(usoc$jbiindb == 23, 1, 0) # Insurance
usoc$jbiindb_24 <- ifelse(usoc$jbiindb == 24, 1, 0) # Restaurants
usoc$jbiindb_25 <- ifelse(usoc$jbiindb == 25, 1, 0) # Service Indust
usoc$jbiindb_26 <- ifelse(usoc$jbiindb == 26, 1, 0) # Trash Removal
usoc$jbiindb_27 <- ifelse(usoc$jbiindb == 27, 1, 0) # Educ./Sport
usoc$jbiindb_28 <- ifelse(usoc$jbiindb == 28, 1, 0) # Health Service
usoc$jbiindb_29 <- ifelse(usoc$jbiindb == 29, 1, 0) # Legal Services
usoc$jbiindb_30 <- ifelse(usoc$jbiindb == 30, 1, 0) # Other Services
usoc$jbiindb_31 <- ifelse(usoc$jbiindb == 31, 1, 0) # Volunt./Church
usoc$jbiindb_32 <- ifelse(usoc$jbiindb == 32, 1, 0) # Priv. Household
usoc$jbiindb_33 <- ifelse(usoc$jbiindb == 33, 1, 0) # Public Admin
usoc$jbiindb_34 <- ifelse(usoc$jbiindb == 34, 1, 0) # Social Sec.
usoc$jbiindb_0 <- ifelse(usoc$jbiindb == 0, 1, 0) # Not Applicable
usoc$jbiindb_neg1 <- ifelse(usoc$jbiindb == -1, 1, 0) # dont know
usoc$jbiindb_neg9 <- ifelse(usoc$jbiindb == -9, 1, 0) # missing

# Highest qualification dummies
usoc$hiqual_1 <- ifelse(usoc$hiqual == 1, 1, 0) # Degree
usoc$hiqual_2 <- ifelse(usoc$hiqual == 2, 1, 0) # Other higher degree
usoc$hiqual_3 <- ifelse(usoc$hiqual == 3, 1, 0) # A-level etc
usoc$hiqual_4 <- ifelse(usoc$hiqual == 4, 1, 0) # GCSE etc
usoc$hiqual_5 <- ifelse(usoc$hiqual == 5, 1, 0) # Other qualification
usoc$hiqual_9 <- ifelse(usoc$hiqual == 9, 1, 0) # No qualification
usoc$hiqual_neg8 <- ifelse(usoc$hiqual == -8, 1, 0) # inapplicable
usoc$hiqual_neg9 <- ifelse(usoc$hiqual == -9, 1, 0) # missing

# Government office region dummies
usoc$gor_1 <- ifelse(usoc$gor == 1, 1, 0) # North East
usoc$gor_2 <- ifelse(usoc$gor == 2, 1, 0) # North West
usoc$gor_3 <- ifelse(usoc$gor == 3, 1, 0) # Yorkshire and The Humber
usoc$gor_4 <- ifelse(usoc$gor == 4, 1, 0) # East Midlands
usoc$gor_5 <- ifelse(usoc$gor == 5, 1, 0) # West Midlands
usoc$gor_6 <- ifelse(usoc$gor == 6, 1, 0) # East of England
usoc$gor_7 <- ifelse(usoc$gor == 7, 1, 0) # London
usoc$gor_8 <- ifelse(usoc$gor == 8, 1, 0) # South East
usoc$gor_9 <- ifelse(usoc$gor == 9, 1, 0) # South West
usoc$gor_10 <- ifelse(usoc$gor == 10, 1, 0) # Wales
usoc$gor_11 <- ifelse(usoc$gor == 11, 1, 0) # Scotland
usoc$gor_12 <- ifelse(usoc$gor == 12, 1, 0) # Northern Ireland
usoc$gor_neg9 <- ifelse(usoc$gor == -9, 1, 0) # missing

# Housing tenure dummies
usoc$tenure_1 <- ifelse(usoc$tenure == 1, 1, 0) # Owned outright
usoc$tenure_2 <- ifelse(usoc$tenure == 2, 1, 0) # Owned with mortgage
usoc$tenure_3 <- ifelse(usoc$tenure == 3, 1, 0) # Local authority rent
usoc$tenure_4 <- ifelse(usoc$tenure == 4, 1, 0) # Housing assoc rented
usoc$tenure_5 <- ifelse(usoc$tenure == 5, 1, 0) # Rented from employer
usoc$tenure_6 <- ifelse(usoc$tenure == 6, 1, 0) # Rented private unfurnished
usoc$tenure_7 <- ifelse(usoc$tenure == 7, 1, 0) # Rented private furnished
usoc$tenure_8 <- ifelse(usoc$tenure == 8, 1, 0) # Other
usoc$tenure_neg9 <- ifelse(usoc$tenure == -9, 1, 0) # missing

## CREATE QUARTERLY EXPORT CONCENTRATION DATA FOR DESCRIPTIVE ANALYSIS
# export data
# NB values are x10^3
exports_eu <- read.csv("~/Documents/Birkbeck work/Dissertation/data/exports_eu.csv", header = TRUE) #laod data
exports_eu2018 <- subset(exports_eu, Year == 2018) # 2018 subset
exports_eu2018Q1 <- subset(exports_eu2018, Quarter == "Q1") # 2018 Q1
exports_eu2018Q2 <- subset(exports_eu2018, Quarter == "Q2") # 2018 Q2
exports_eu2018Q3 <- subset(exports_eu2018, Quarter == "Q3") # 2018 Q3
exports_eu2018Q4 <- subset(exports_eu2018, Quarter == "Q4") # 2018 Q4
exports_eu2017 <- subset(exports_eu, Year == 2017) # 2017 subset
exports_eu2017Q1 <- subset(exports_eu2017, Quarter == "Q1") # 2017 Q1
exports_eu2017Q2 <- subset(exports_eu2017, Quarter == "Q2") # 2017 Q2
exports_eu2017Q3 <- subset(exports_eu2017, Quarter == "Q3") # 2017 Q3
exports_eu2017Q4 <- subset(exports_eu2017, Quarter == "Q4") # 2017 Q4
exports_eu2016 <- subset(exports_eu, Year == 2016) # 2016 subset
exports_eu2016Q1 <- subset(exports_eu2016, Quarter == "Q1") # 2016 Q1
exports_eu2016Q2 <- subset(exports_eu2016, Quarter == "Q2") # 2016 Q2
exports_eu2016Q3 <- subset(exports_eu2016, Quarter == "Q3") # 2016 Q3
exports_eu2016Q4 <- subset(exports_eu2016, Quarter == "Q4") # 2016 Q4
exports_eu2015 <- subset(exports_eu, Year == 2015) # 2015 subset
exports_eu2015Q1 <- subset(exports_eu2015, Quarter == "Q1") # 2015 Q1
exports_eu2015Q2 <- subset(exports_eu2015, Quarter == "Q2") # 2015 Q2
exports_eu2015Q3 <- subset(exports_eu2015, Quarter == "Q3") # 2015 Q3
exports_eu2015Q4 <- subset(exports_eu2015, Quarter == "Q4") # 2015 Q4
exports_eu2014 <- subset(exports_eu, Year == 2014) # 2014 subset
exports_eu2014Q1 <- subset(exports_eu2014, Quarter == "Q1") # 2014 Q1
exports_eu2014Q2 <- subset(exports_eu2014, Quarter == "Q2") # 2014 Q2
exports_eu2014Q3 <- subset(exports_eu2014, Quarter == "Q3") # 2014 Q3
exports_eu2014Q4 <- subset(exports_eu2014, Quarter == "Q4") # 2014 Q4
exports_eu2013 <- subset(exports_eu, Year == 2013) # 2013 subset
exports_eu2013Q1 <- subset(exports_eu2013, Quarter == "Q1") # 2013 Q1
exports_eu2013Q2 <- subset(exports_eu2013, Quarter == "Q2") # 2013 Q2
exports_eu2013Q3 <- subset(exports_eu2013, Quarter == "Q3") # 2013 Q3
exports_eu2013Q4 <- subset(exports_eu2013, Quarter == "Q4") # 2013 Q4
exports_all <- read.csv("~/Documents/Birkbeck work/Dissertation/data/exports_all.csv", header = TRUE)
exports_all2018 <- subset(exports_all, Year == 2018)
exports_all2018Q1 <- subset(exports_all2018, Quarter == "Q1") 
exports_all2018Q2 <- subset(exports_all2018, Quarter == "Q2") 
exports_all2018Q3 <- subset(exports_all2018, Quarter == "Q3") 
exports_all2018Q4 <- subset(exports_all2018, Quarter == "Q4") 
exports_all2017 <- subset(exports_all, Year == 2017)
exports_all2017Q1 <- subset(exports_all2017, Quarter == "Q1") 
exports_all2017Q2 <- subset(exports_all2017, Quarter == "Q2") 
exports_all2017Q3 <- subset(exports_all2017, Quarter == "Q3") 
exports_all2017Q4 <- subset(exports_all2017, Quarter == "Q4") 
exports_all2016 <- subset(exports_all, Year == 2016)
exports_all2016Q1 <- subset(exports_all2016, Quarter == "Q1") 
exports_all2016Q2 <- subset(exports_all2016, Quarter == "Q2") 
exports_all2016Q3 <- subset(exports_all2016, Quarter == "Q3") 
exports_all2016Q4 <- subset(exports_all2016, Quarter == "Q4") 
exports_all2015 <- subset(exports_all, Year == 2015)
exports_all2015Q1 <- subset(exports_all2015, Quarter == "Q1") 
exports_all2015Q2 <- subset(exports_all2015, Quarter == "Q2") 
exports_all2015Q3 <- subset(exports_all2015, Quarter == "Q3") 
exports_all2015Q4 <- subset(exports_all2015, Quarter == "Q4") 
exports_all2014 <- subset(exports_all, Year == 2014)
exports_all2014Q1 <- subset(exports_all2014, Quarter == "Q1") 
exports_all2014Q2 <- subset(exports_all2014, Quarter == "Q2") 
exports_all2014Q3 <- subset(exports_all2014, Quarter == "Q3") 
exports_all2014Q4 <- subset(exports_all2014, Quarter == "Q4") 
exports_all2013 <- subset(exports_all, Year == 2013)
exports_all2013Q1 <- subset(exports_all2013, Quarter == "Q1") 
exports_all2013Q2 <- subset(exports_all2013, Quarter == "Q2") 
exports_all2013Q3 <- subset(exports_all2013, Quarter == "Q3") 
exports_all2013Q4 <- subset(exports_all2013, Quarter == "Q4") 

# create region vector
Region <- c("North East", "North West", "Yorkshire and The Humber", 
            "East Midlands", "West Midlands", "East of England", 
            "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland") 

# create export data frame
exports <- data.frame(Region)

# create columns with export data
exports$euQ1_2013 <- ifelse(exports$Region == "North East", exports_eu2013Q1[exports_eu2013Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2013Q1[exports_eu2013Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2013Q1[exports_eu2013Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2013Q1[exports_eu2013Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2013Q1[exports_eu2013Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2013Q1[exports_eu2013Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2013Q1[exports_eu2013Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2013Q1[exports_eu2013Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2013Q1[exports_eu2013Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2013Q1[exports_eu2013Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2013Q1[exports_eu2013Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2013Q1[exports_eu2013Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2013 <- ifelse(exports$Region == "North East", exports_eu2013Q2[exports_eu2013Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2013Q2[exports_eu2013Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2013Q2[exports_eu2013Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2013Q2[exports_eu2013Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2013Q2[exports_eu2013Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2013Q2[exports_eu2013Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2013Q2[exports_eu2013Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2013Q2[exports_eu2013Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2013Q2[exports_eu2013Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2013Q2[exports_eu2013Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2013Q2[exports_eu2013Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2013Q2[exports_eu2013Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2013 <- ifelse(exports$Region == "North East", exports_eu2013Q3[exports_eu2013Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2013Q3[exports_eu2013Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2013Q3[exports_eu2013Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2013Q3[exports_eu2013Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2013Q3[exports_eu2013Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2013Q3[exports_eu2013Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2013Q3[exports_eu2013Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2013Q3[exports_eu2013Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2013Q3[exports_eu2013Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2013Q3[exports_eu2013Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2013Q3[exports_eu2013Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2013Q3[exports_eu2013Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2013 <- ifelse(exports$Region == "North East", exports_eu2013Q4[exports_eu2013Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2013Q4[exports_eu2013Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2013Q4[exports_eu2013Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2013Q4[exports_eu2013Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2013Q4[exports_eu2013Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2013Q4[exports_eu2013Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2013Q4[exports_eu2013Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2013Q4[exports_eu2013Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2013Q4[exports_eu2013Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2013Q4[exports_eu2013Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2013Q4[exports_eu2013Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2013Q4[exports_eu2013Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ1_2014 <- ifelse(exports$Region == "North East", exports_eu2014Q1[exports_eu2014Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2014Q1[exports_eu2014Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2014Q1[exports_eu2014Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2014Q1[exports_eu2014Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2014Q1[exports_eu2014Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2014Q1[exports_eu2014Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2014Q1[exports_eu2014Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2014Q1[exports_eu2014Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2014Q1[exports_eu2014Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2014Q1[exports_eu2014Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2014Q1[exports_eu2014Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2014Q1[exports_eu2014Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2014 <- ifelse(exports$Region == "North East", exports_eu2014Q2[exports_eu2014Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2014Q2[exports_eu2014Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2014Q2[exports_eu2014Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2014Q2[exports_eu2014Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2014Q2[exports_eu2014Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2014Q2[exports_eu2014Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2014Q2[exports_eu2014Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2014Q2[exports_eu2014Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2014Q2[exports_eu2014Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2014Q2[exports_eu2014Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2014Q2[exports_eu2014Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2014Q2[exports_eu2014Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2014 <- ifelse(exports$Region == "North East", exports_eu2014Q3[exports_eu2014Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2014Q3[exports_eu2014Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2014Q3[exports_eu2014Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2014Q3[exports_eu2014Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2014Q3[exports_eu2014Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2014Q3[exports_eu2014Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2014Q3[exports_eu2014Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2014Q3[exports_eu2014Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2014Q3[exports_eu2014Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2014Q3[exports_eu2014Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2014Q3[exports_eu2014Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2014Q3[exports_eu2014Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2014 <- ifelse(exports$Region == "North East", exports_eu2014Q4[exports_eu2014Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2014Q4[exports_eu2014Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2014Q4[exports_eu2014Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2014Q4[exports_eu2014Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2014Q4[exports_eu2014Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2014Q4[exports_eu2014Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2014Q4[exports_eu2014Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2014Q4[exports_eu2014Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2014Q4[exports_eu2014Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2014Q4[exports_eu2014Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2014Q4[exports_eu2014Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2014Q4[exports_eu2014Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ1_2015 <- ifelse(exports$Region == "North East", exports_eu2015Q1[exports_eu2015Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2015Q1[exports_eu2015Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2015Q1[exports_eu2015Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2015Q1[exports_eu2015Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2015Q1[exports_eu2015Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2015Q1[exports_eu2015Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2015Q1[exports_eu2015Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2015Q1[exports_eu2015Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2015Q1[exports_eu2015Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2015Q1[exports_eu2015Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2015Q1[exports_eu2015Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2015Q1[exports_eu2015Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2015 <- ifelse(exports$Region == "North East", exports_eu2015Q2[exports_eu2015Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2015Q2[exports_eu2015Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2015Q2[exports_eu2015Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2015Q2[exports_eu2015Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2015Q2[exports_eu2015Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2015Q2[exports_eu2015Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2015Q2[exports_eu2015Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2015Q2[exports_eu2015Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2015Q2[exports_eu2015Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2015Q2[exports_eu2015Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2015Q2[exports_eu2015Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2015Q2[exports_eu2015Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2015 <- ifelse(exports$Region == "North East", exports_eu2015Q3[exports_eu2015Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2015Q3[exports_eu2015Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2015Q3[exports_eu2015Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2015Q3[exports_eu2015Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2015Q3[exports_eu2015Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2015Q3[exports_eu2015Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2015Q3[exports_eu2015Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2015Q3[exports_eu2015Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2015Q3[exports_eu2015Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2015Q3[exports_eu2015Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2015Q3[exports_eu2015Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2015Q3[exports_eu2015Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2015 <- ifelse(exports$Region == "North East", exports_eu2015Q4[exports_eu2015Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2015Q4[exports_eu2015Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2015Q4[exports_eu2015Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2015Q4[exports_eu2015Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2015Q4[exports_eu2015Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2015Q4[exports_eu2015Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2015Q4[exports_eu2015Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2015Q4[exports_eu2015Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2015Q4[exports_eu2015Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2015Q4[exports_eu2015Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2015Q4[exports_eu2015Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2015Q4[exports_eu2015Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ1_2016 <- ifelse(exports$Region == "North East", exports_eu2016Q1[exports_eu2016Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2016Q1[exports_eu2016Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2016Q1[exports_eu2016Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2016Q1[exports_eu2016Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2016Q1[exports_eu2016Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2016Q1[exports_eu2016Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2016Q1[exports_eu2016Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2016Q1[exports_eu2016Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2016Q1[exports_eu2016Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2016Q1[exports_eu2016Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2016Q1[exports_eu2016Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2016Q1[exports_eu2016Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2016 <- ifelse(exports$Region == "North East", exports_eu2016Q2[exports_eu2016Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2016Q2[exports_eu2016Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2016Q2[exports_eu2016Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2016Q2[exports_eu2016Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2016Q2[exports_eu2016Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2016Q2[exports_eu2016Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2016Q2[exports_eu2016Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2016Q2[exports_eu2016Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2016Q2[exports_eu2016Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2016Q2[exports_eu2016Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2016Q2[exports_eu2016Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2016Q2[exports_eu2016Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2016 <- ifelse(exports$Region == "North East", exports_eu2016Q3[exports_eu2016Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2016Q3[exports_eu2016Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2016Q3[exports_eu2016Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2016Q3[exports_eu2016Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2016Q3[exports_eu2016Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2016Q3[exports_eu2016Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2016Q3[exports_eu2016Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2016Q3[exports_eu2016Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2016Q3[exports_eu2016Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2016Q3[exports_eu2016Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2016Q3[exports_eu2016Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2016Q3[exports_eu2016Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2016 <- ifelse(exports$Region == "North East", exports_eu2016Q4[exports_eu2016Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2016Q4[exports_eu2016Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2016Q4[exports_eu2016Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2016Q4[exports_eu2016Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2016Q4[exports_eu2016Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2016Q4[exports_eu2016Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2016Q4[exports_eu2016Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2016Q4[exports_eu2016Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2016Q4[exports_eu2016Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2016Q4[exports_eu2016Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2016Q4[exports_eu2016Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2016Q4[exports_eu2016Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ1_2017 <- ifelse(exports$Region == "North East", exports_eu2017Q1[exports_eu2017Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2017Q1[exports_eu2017Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2017Q1[exports_eu2017Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2017Q1[exports_eu2017Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2017Q1[exports_eu2017Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2017Q1[exports_eu2017Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2017Q1[exports_eu2017Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2017Q1[exports_eu2017Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2017Q1[exports_eu2017Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2017Q1[exports_eu2017Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2017Q1[exports_eu2017Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2017Q1[exports_eu2017Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2017 <- ifelse(exports$Region == "North East", exports_eu2017Q2[exports_eu2017Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2017Q2[exports_eu2017Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2017Q2[exports_eu2017Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2017Q2[exports_eu2017Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2017Q2[exports_eu2017Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2017Q2[exports_eu2017Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2017Q2[exports_eu2017Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2017Q2[exports_eu2017Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2017Q2[exports_eu2017Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2017Q2[exports_eu2017Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2017Q2[exports_eu2017Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2017Q2[exports_eu2017Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2017 <- ifelse(exports$Region == "North East", exports_eu2017Q3[exports_eu2017Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2017Q3[exports_eu2017Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2017Q3[exports_eu2017Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2017Q3[exports_eu2017Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2017Q3[exports_eu2017Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2017Q3[exports_eu2017Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2017Q3[exports_eu2017Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2017Q3[exports_eu2017Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2017Q3[exports_eu2017Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2017Q3[exports_eu2017Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2017Q3[exports_eu2017Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2017Q3[exports_eu2017Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2017 <- ifelse(exports$Region == "North East", exports_eu2017Q4[exports_eu2017Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2017Q4[exports_eu2017Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2017Q4[exports_eu2017Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2017Q4[exports_eu2017Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2017Q4[exports_eu2017Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2017Q4[exports_eu2017Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2017Q4[exports_eu2017Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2017Q4[exports_eu2017Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2017Q4[exports_eu2017Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2017Q4[exports_eu2017Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2017Q4[exports_eu2017Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2017Q4[exports_eu2017Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ1_2018 <- ifelse(exports$Region == "North East", exports_eu2018Q1[exports_eu2018Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2018Q1[exports_eu2018Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2018Q1[exports_eu2018Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2018Q1[exports_eu2018Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2018Q1[exports_eu2018Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2018Q1[exports_eu2018Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2018Q1[exports_eu2018Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2018Q1[exports_eu2018Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2018Q1[exports_eu2018Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2018Q1[exports_eu2018Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2018Q1[exports_eu2018Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2018Q1[exports_eu2018Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ2_2018 <- ifelse(exports$Region == "North East", exports_eu2018Q2[exports_eu2018Q2$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2018Q2[exports_eu2018Q2$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2018Q2[exports_eu2018Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2018Q2[exports_eu2018Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2018Q2[exports_eu2018Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2018Q2[exports_eu2018Q2$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2018Q2[exports_eu2018Q2$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2018Q2[exports_eu2018Q2$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2018Q2[exports_eu2018Q2$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2018Q2[exports_eu2018Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2018Q2[exports_eu2018Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2018Q2[exports_eu2018Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ3_2018 <- ifelse(exports$Region == "North East", exports_eu2018Q3[exports_eu2018Q3$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2018Q3[exports_eu2018Q3$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2018Q3[exports_eu2018Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2018Q3[exports_eu2018Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2018Q3[exports_eu2018Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2018Q3[exports_eu2018Q3$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2018Q3[exports_eu2018Q3$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2018Q3[exports_eu2018Q3$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2018Q3[exports_eu2018Q3$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2018Q3[exports_eu2018Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2018Q3[exports_eu2018Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2018Q3[exports_eu2018Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$euQ4_2018 <- ifelse(exports$Region == "North East", exports_eu2018Q4[exports_eu2018Q4$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_eu2018Q4[exports_eu2018Q4$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_eu2018Q4[exports_eu2018Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_eu2018Q4[exports_eu2018Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_eu2018Q4[exports_eu2018Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_eu2018Q4[exports_eu2018Q4$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_eu2018Q4[exports_eu2018Q4$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_eu2018Q4[exports_eu2018Q4$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_eu2018Q4[exports_eu2018Q4$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_eu2018Q4[exports_eu2018Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_eu2018Q4[exports_eu2018Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_eu2018Q4[exports_eu2018Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2013 <- ifelse(exports$Region == "North East", exports_all2013Q1[exports_all2013Q1$Region == "North East", ]$`Value....000.s`, 
                            ifelse(exports$Region == "North West", exports_all2013Q1[exports_all2013Q1$Region == "North West", ]$`Value....000.s`, 
                                   ifelse(exports$Region == "Yorkshire and The Humber", exports_all2013Q1[exports_all2013Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                          ifelse(exports$Region == "East Midlands", exports_all2013Q1[exports_all2013Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                 ifelse(exports$Region == "West Midlands", exports_all2013Q1[exports_all2013Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                        ifelse(exports$Region == "East of England", exports_all2013Q1[exports_all2013Q1$Region == "East", ]$`Value....000.s`,
                                                               ifelse(exports$Region == "London", exports_all2013Q1[exports_all2013Q1$Region == "London", ]$`Value....000.s`,
                                                                      ifelse(exports$Region == "South East", exports_all2013Q1[exports_all2013Q1$Region == "South East", ]$`Value....000.s`,
                                                                             ifelse(exports$Region == "South West", exports_all2013Q1[exports_all2013Q1$Region == "South West", ]$`Value....000.s`,
                                                                                    ifelse(exports$Region == "Wales", exports_all2013Q1[exports_all2013Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                           ifelse(exports$Region == "Scotland", exports_all2013Q1[exports_all2013Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                  ifelse(exports$Region == "Northern Ireland", exports_all2013Q1[exports_all2013Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2013 <- ifelse(exports$Region == "North East", exports_all2013Q2[exports_all2013Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2013Q2[exports_all2013Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2013Q2[exports_all2013Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2013Q2[exports_all2013Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2013Q2[exports_all2013Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2013Q2[exports_all2013Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2013Q2[exports_all2013Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2013Q2[exports_all2013Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2013Q2[exports_all2013Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2013Q2[exports_all2013Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2013Q2[exports_all2013Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2013Q2[exports_all2013Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2013 <- ifelse(exports$Region == "North East", exports_all2013Q3[exports_all2013Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2013Q3[exports_all2013Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2013Q3[exports_all2013Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2013Q3[exports_all2013Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2013Q3[exports_all2013Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2013Q3[exports_all2013Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2013Q3[exports_all2013Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2013Q3[exports_all2013Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2013Q3[exports_all2013Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2013Q3[exports_all2013Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2013Q3[exports_all2013Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2013Q3[exports_all2013Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2013 <- ifelse(exports$Region == "North East", exports_all2013Q4[exports_all2013Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2013Q4[exports_all2013Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2013Q4[exports_all2013Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2013Q4[exports_all2013Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2013Q4[exports_all2013Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2013Q4[exports_all2013Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2013Q4[exports_all2013Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2013Q4[exports_all2013Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2013Q4[exports_all2013Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2013Q4[exports_all2013Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2013Q4[exports_all2013Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2013Q4[exports_all2013Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2014 <- ifelse(exports$Region == "North East", exports_all2014Q1[exports_all2014Q1$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2014Q1[exports_all2014Q1$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2014Q1[exports_all2014Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2014Q1[exports_all2014Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2014Q1[exports_all2014Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2014Q1[exports_all2014Q1$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2014Q1[exports_all2014Q1$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2014Q1[exports_all2014Q1$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2014Q1[exports_all2014Q1$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2014Q1[exports_all2014Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2014Q1[exports_all2014Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2014Q1[exports_all2014Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2014 <- ifelse(exports$Region == "North East", exports_all2014Q2[exports_all2014Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2014Q2[exports_all2014Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2014Q2[exports_all2014Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2014Q2[exports_all2014Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2014Q2[exports_all2014Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2014Q2[exports_all2014Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2014Q2[exports_all2014Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2014Q2[exports_all2014Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2014Q2[exports_all2014Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2014Q2[exports_all2014Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2014Q2[exports_all2014Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2014Q2[exports_all2014Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2014 <- ifelse(exports$Region == "North East", exports_all2014Q3[exports_all2014Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2014Q3[exports_all2014Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2014Q3[exports_all2014Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2014Q3[exports_all2014Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2014Q3[exports_all2014Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2014Q3[exports_all2014Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2014Q3[exports_all2014Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2014Q3[exports_all2014Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2014Q3[exports_all2014Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2014Q3[exports_all2014Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2014Q3[exports_all2014Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2014Q3[exports_all2014Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2014 <- ifelse(exports$Region == "North East", exports_all2014Q4[exports_all2014Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2014Q4[exports_all2014Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2014Q4[exports_all2014Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2014Q4[exports_all2014Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2014Q4[exports_all2014Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2014Q4[exports_all2014Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2014Q4[exports_all2014Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2014Q4[exports_all2014Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2014Q4[exports_all2014Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2014Q4[exports_all2014Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2014Q4[exports_all2014Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2014Q4[exports_all2014Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2015 <- ifelse(exports$Region == "North East", exports_all2015Q1[exports_all2015Q1$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2015Q1[exports_all2015Q1$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2015Q1[exports_all2015Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2015Q1[exports_all2015Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2015Q1[exports_all2015Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2015Q1[exports_all2015Q1$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2015Q1[exports_all2015Q1$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2015Q1[exports_all2015Q1$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2015Q1[exports_all2015Q1$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2015Q1[exports_all2015Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2015Q1[exports_all2015Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2015Q1[exports_all2015Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2015 <- ifelse(exports$Region == "North East", exports_all2015Q2[exports_all2015Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2015Q2[exports_all2015Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2015Q2[exports_all2015Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2015Q2[exports_all2015Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2015Q2[exports_all2015Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2015Q2[exports_all2015Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2015Q2[exports_all2015Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2015Q2[exports_all2015Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2015Q2[exports_all2015Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2015Q2[exports_all2015Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2015Q2[exports_all2015Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2015Q2[exports_all2015Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2015 <- ifelse(exports$Region == "North East", exports_all2015Q3[exports_all2015Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2015Q3[exports_all2015Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2015Q3[exports_all2015Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2015Q3[exports_all2015Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2015Q3[exports_all2015Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2015Q3[exports_all2015Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2015Q3[exports_all2015Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2015Q3[exports_all2015Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2015Q3[exports_all2015Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2015Q3[exports_all2015Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2015Q3[exports_all2015Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2015Q3[exports_all2015Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2015 <- ifelse(exports$Region == "North East", exports_all2015Q4[exports_all2015Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2015Q4[exports_all2015Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2015Q4[exports_all2015Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2015Q4[exports_all2015Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2015Q4[exports_all2015Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2015Q4[exports_all2015Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2015Q4[exports_all2015Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2015Q4[exports_all2015Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2015Q4[exports_all2015Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2015Q4[exports_all2015Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2015Q4[exports_all2015Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2015Q4[exports_all2015Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2016 <- ifelse(exports$Region == "North East", exports_all2016Q1[exports_all2016Q1$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2016Q1[exports_all2016Q1$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2016Q1[exports_all2016Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2016Q1[exports_all2016Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2016Q1[exports_all2016Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2016Q1[exports_all2016Q1$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2016Q1[exports_all2016Q1$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2016Q1[exports_all2016Q1$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2016Q1[exports_all2016Q1$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2016Q1[exports_all2016Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2016Q1[exports_all2016Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2016Q1[exports_all2016Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2016 <- ifelse(exports$Region == "North East", exports_all2016Q2[exports_all2016Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2016Q2[exports_all2016Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2016Q2[exports_all2016Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2016Q2[exports_all2016Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2016Q2[exports_all2016Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2016Q2[exports_all2016Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2016Q2[exports_all2016Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2016Q2[exports_all2016Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2016Q2[exports_all2016Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2016Q2[exports_all2016Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2016Q2[exports_all2016Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2016Q2[exports_all2016Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2016 <- ifelse(exports$Region == "North East", exports_all2016Q3[exports_all2016Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2016Q3[exports_all2016Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2016Q3[exports_all2016Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2016Q3[exports_all2016Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2016Q3[exports_all2016Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2016Q3[exports_all2016Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2016Q3[exports_all2016Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2016Q3[exports_all2016Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2016Q3[exports_all2016Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2016Q3[exports_all2016Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2016Q3[exports_all2016Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2016Q3[exports_all2016Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2016 <- ifelse(exports$Region == "North East", exports_all2016Q4[exports_all2016Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2016Q4[exports_all2016Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2016Q4[exports_all2016Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2016Q4[exports_all2016Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2016Q4[exports_all2016Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2016Q4[exports_all2016Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2016Q4[exports_all2016Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2016Q4[exports_all2016Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2016Q4[exports_all2016Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2016Q4[exports_all2016Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2016Q4[exports_all2016Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2016Q4[exports_all2016Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2017 <- ifelse(exports$Region == "North East", exports_all2017Q1[exports_all2017Q1$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2017Q1[exports_all2017Q1$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2017Q1[exports_all2017Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2017Q1[exports_all2017Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2017Q1[exports_all2017Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2017Q1[exports_all2017Q1$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2017Q1[exports_all2017Q1$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2017Q1[exports_all2017Q1$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2017Q1[exports_all2017Q1$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2017Q1[exports_all2017Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2017Q1[exports_all2017Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2017Q1[exports_all2017Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2017 <- ifelse(exports$Region == "North East", exports_all2017Q2[exports_all2017Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2017Q2[exports_all2017Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2017Q2[exports_all2017Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2017Q2[exports_all2017Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2017Q2[exports_all2017Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2017Q2[exports_all2017Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2017Q2[exports_all2017Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2017Q2[exports_all2017Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2017Q2[exports_all2017Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2017Q2[exports_all2017Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2017Q2[exports_all2017Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2017Q2[exports_all2017Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2017 <- ifelse(exports$Region == "North East", exports_all2017Q3[exports_all2017Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2017Q3[exports_all2017Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2017Q3[exports_all2017Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2017Q3[exports_all2017Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2017Q3[exports_all2017Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2017Q3[exports_all2017Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2017Q3[exports_all2017Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2017Q3[exports_all2017Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2017Q3[exports_all2017Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2017Q3[exports_all2017Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2017Q3[exports_all2017Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2017Q3[exports_all2017Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2017 <- ifelse(exports$Region == "North East", exports_all2017Q4[exports_all2017Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2017Q4[exports_all2017Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2017Q4[exports_all2017Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2017Q4[exports_all2017Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2017Q4[exports_all2017Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2017Q4[exports_all2017Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2017Q4[exports_all2017Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2017Q4[exports_all2017Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2017Q4[exports_all2017Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2017Q4[exports_all2017Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2017Q4[exports_all2017Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2017Q4[exports_all2017Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ1_2018 <- ifelse(exports$Region == "North East", exports_all2018Q1[exports_all2018Q1$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2018Q1[exports_all2018Q1$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2018Q1[exports_all2018Q1$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2018Q1[exports_all2018Q1$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2018Q1[exports_all2018Q1$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2018Q1[exports_all2018Q1$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2018Q1[exports_all2018Q1$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2018Q1[exports_all2018Q1$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2018Q1[exports_all2018Q1$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2018Q1[exports_all2018Q1$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2018Q1[exports_all2018Q1$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2018Q1[exports_all2018Q1$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ2_2018 <- ifelse(exports$Region == "North East", exports_all2018Q2[exports_all2018Q2$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2018Q2[exports_all2018Q2$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2018Q2[exports_all2018Q2$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2018Q2[exports_all2018Q2$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2018Q2[exports_all2018Q2$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2018Q2[exports_all2018Q2$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2018Q2[exports_all2018Q2$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2018Q2[exports_all2018Q2$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2018Q2[exports_all2018Q2$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2018Q2[exports_all2018Q2$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2018Q2[exports_all2018Q2$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2018Q2[exports_all2018Q2$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ3_2018 <- ifelse(exports$Region == "North East", exports_all2018Q3[exports_all2018Q3$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2018Q3[exports_all2018Q3$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2018Q3[exports_all2018Q3$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2018Q3[exports_all2018Q3$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2018Q3[exports_all2018Q3$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2018Q3[exports_all2018Q3$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2018Q3[exports_all2018Q3$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2018Q3[exports_all2018Q3$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2018Q3[exports_all2018Q3$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2018Q3[exports_all2018Q3$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2018Q3[exports_all2018Q3$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2018Q3[exports_all2018Q3$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

exports$allQ4_2018 <- ifelse(exports$Region == "North East", exports_all2018Q4[exports_all2018Q4$Region == "North East", ]$`Value....000.s`, 
                             ifelse(exports$Region == "North West", exports_all2018Q4[exports_all2018Q4$Region == "North West", ]$`Value....000.s`, 
                                    ifelse(exports$Region == "Yorkshire and The Humber", exports_all2018Q4[exports_all2018Q4$Region == "Yorkshire and The Humber", ]$`Value....000.s`,
                                           ifelse(exports$Region == "East Midlands", exports_all2018Q4[exports_all2018Q4$Region == "East Midlands", ]$`Value....000.s`,
                                                  ifelse(exports$Region == "West Midlands", exports_all2018Q4[exports_all2018Q4$Region == "West Midlands", ]$`Value....000.s`,
                                                         ifelse(exports$Region == "East of England", exports_all2018Q4[exports_all2018Q4$Region == "East", ]$`Value....000.s`,
                                                                ifelse(exports$Region == "London", exports_all2018Q4[exports_all2018Q4$Region == "London", ]$`Value....000.s`,
                                                                       ifelse(exports$Region == "South East", exports_all2018Q4[exports_all2018Q4$Region == "South East", ]$`Value....000.s`,
                                                                              ifelse(exports$Region == "South West", exports_all2018Q4[exports_all2018Q4$Region == "South West", ]$`Value....000.s`,
                                                                                     ifelse(exports$Region == "Wales", exports_all2018Q4[exports_all2018Q4$Region == "Wales", ]$`Value....000.s`,
                                                                                            ifelse(exports$Region == "Scotland", exports_all2018Q4[exports_all2018Q4$Region == "Scotland", ]$`Value....000.s`,
                                                                                                   ifelse(exports$Region == "Northern Ireland", exports_all2018Q4[exports_all2018Q4$Region == "Northern Ireland", ]$`Value....000.s`, 0))))))))))))

# quarterly export concentration
exports$excQ1_2013 <- exports$euQ1_2013/exports$allQ1_2013 # 2013
exports$excQ2_2013 <- exports$euQ2_2013/exports$allQ2_2013
exports$excQ3_2013 <- exports$euQ3_2013/exports$allQ3_2013
exports$excQ4_2013 <- exports$euQ4_2013/exports$allQ4_2013
exports$excQ1_2014 <- exports$euQ1_2014/exports$allQ1_2014 # 2014
exports$excQ2_2014 <- exports$euQ2_2014/exports$allQ2_2014
exports$excQ3_2014 <- exports$euQ3_2014/exports$allQ3_2014
exports$excQ4_2014 <- exports$euQ4_2014/exports$allQ4_2014
exports$excQ1_2015 <- exports$euQ1_2015/exports$allQ1_2015 # 2015
exports$excQ2_2015 <- exports$euQ2_2015/exports$allQ2_2015
exports$excQ3_2015 <- exports$euQ3_2015/exports$allQ3_2015
exports$excQ4_2015 <- exports$euQ4_2015/exports$allQ4_2015
exports$excQ1_2016 <- exports$euQ1_2016/exports$allQ1_2016 # 2016
exports$excQ2_2016 <- exports$euQ2_2016/exports$allQ2_2016
exports$excQ3_2016 <- exports$euQ3_2016/exports$allQ3_2016
exports$excQ4_2016 <- exports$euQ4_2016/exports$allQ4_2016
exports$excQ1_2017 <- exports$euQ1_2017/exports$allQ1_2017 # 2017
exports$excQ2_2017 <- exports$euQ2_2017/exports$allQ2_2017
exports$excQ3_2017 <- exports$euQ3_2017/exports$allQ3_2017
exports$excQ4_2017 <- exports$euQ4_2017/exports$allQ4_2017
exports$excQ1_2018 <- exports$euQ1_2018/exports$allQ1_2018 # 2018
exports$excQ2_2018 <- exports$euQ2_2018/exports$allQ2_2018
exports$excQ3_2018 <- exports$euQ3_2018/exports$allQ3_2018
exports$excQ4_2018 <- exports$euQ4_2018/exports$allQ4_2018

## GENERATING TREATMENT AND SHIFT SHARE INSTRUMENT

# Generate a dummy column such that data collected between June 24 2016 
# and March 29 2017 (invocation of article 50)  takes the value of 1 and 0 if not
# NB data in sample were collected between Jan 1 2009 and Feb 1 2018
usoc$ref_dummy <- ifelse(usoc$intdatey*1000 + usoc$intdatem*100 + usoc$intdated <= 2016623, 0, 
                         ifelse(usoc$intdatey*1000 + usoc$intdatem*100 + usoc$intdated >= 2017329, 0, 1))

# move export concentration in Q3 2016 to panel data
usoc$exposure <- ifelse(usoc$gor == 1, exports[exports$Region == "North East",]$excQ3_2016, 
                        ifelse(usoc$gor == 2, exports[exports$Region == "North West",]$excQ3_2016, 
                               ifelse(usoc$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ3_2016, 
                                      ifelse(usoc$gor == 4, exports[exports$Region == "East Midlands",]$excQ3_2016, 
                                             ifelse(usoc$gor == 5, exports[exports$Region == "West Midlands",]$excQ3_2016, 
                                                    ifelse(usoc$gor == 6, exports[exports$Region == "East of England",]$excQ3_2016, 
                                                           ifelse(usoc$gor == 7, exports[exports$Region == "London",]$excQ3_2016, 
                                                                  ifelse(usoc$gor == 8, exports[exports$Region == "South East",]$excQ3_2016, 
                                                                         ifelse(usoc$gor == 9, exports[exports$Region == "South West",]$excQ3_2016, 
                                                                                ifelse(usoc$gor == 10, exports[exports$Region == "Wales",]$excQ3_2016, 
                                                                                       ifelse(usoc$gor == 11, exports[exports$Region == "Scotland",]$excQ3_2016, 
                                                                                              ifelse(usoc$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ3_2016, 0))))))))))))
# lagged export concentration
usoc$exposure_lag <- ifelse(usoc$gor == 1, exports[exports$Region == "North East",]$excQ2_2015, 
                            ifelse(usoc$gor == 2, exports[exports$Region == "North West",]$excQ2_2015, 
                                   ifelse(usoc$gor == 3, exports[exports$Region == "Yorkshire and The Humber",]$excQ2_2015, 
                                          ifelse(usoc$gor == 4, exports[exports$Region == "East Midlands",]$excQ2_2015, 
                                                 ifelse(usoc$gor == 5, exports[exports$Region == "West Midlands",]$excQ2_2015, 
                                                        ifelse(usoc$gor == 6, exports[exports$Region == "East of England",]$excQ2_2015, 
                                                               ifelse(usoc$gor == 7, exports[exports$Region == "London",]$excQ2_2015, 
                                                                      ifelse(usoc$gor == 8, exports[exports$Region == "South East",]$excQ2_2015, 
                                                                             ifelse(usoc$gor == 9, exports[exports$Region == "South West",]$excQ2_2015, 
                                                                                    ifelse(usoc$gor == 10, exports[exports$Region == "Wales",]$excQ2_2015, 
                                                                                           ifelse(usoc$gor == 11, exports[exports$Region == "Scotland",]$excQ2_2015, 
                                                                                                  ifelse(usoc$gor == 12, exports[exports$Region == "Northern Ireland",]$excQ2_2015, 0))))))))))))


# multiply dummy column by exposure to generate treatment treatment variable and instrument
usoc$treatment <- usoc$ref_dummy*usoc$exposure
usoc$instrument <- usoc$ref_dummy*usoc$exposure_lag

# remove objects from environment
rm(Region)

# save csv file
write.csv(usoc, "usoc.csv")

## QUARTERLY REGIONAL EXPORT CONCENTRATION DATA
# create subsets for eu exports and export concentration
exports_exc <- subset(exports, select = c(excQ1_2013, excQ2_2013, excQ3_2013, excQ4_2013, 
                                          excQ1_2014, excQ2_2014, excQ3_2014, excQ4_2014,
                                          excQ1_2015, excQ2_2015, excQ3_2015, excQ4_2015,
                                          excQ1_2016, excQ2_2016, excQ3_2016, excQ4_2016,
                                          excQ1_2017, excQ2_2017, excQ3_2017, excQ4_2017,
                                          excQ1_2018, excQ2_2018, excQ3_2018, excQ4_2018))
# transpose data
exports_exc <- t(exports_exc)

# rename columns
colnames(exports_exc)[1] <- "North_East"
colnames(exports_exc)[2] <- "North_West"
colnames(exports_exc)[3] <- "Yorkshire_and_The_Humber"
colnames(exports_exc)[4] <- "East_Midlands"
colnames(exports_exc)[5] <- "West_Midlands"
colnames(exports_exc)[6] <- "East_of_England"
colnames(exports_exc)[7] <- "London"
colnames(exports_exc)[8] <- "South_East"
colnames(exports_exc)[9] <- "South_West"
colnames(exports_exc)[10] <- "Wales"
colnames(exports_exc)[11] <- "Scotland"
colnames(exports_exc)[12] <- "Northern_Ireland"

# create data frame objects
exports_exc <- data.frame(exports_exc)

# clean up
rm(list=ls(pattern = "exports_eu"))
rm(list=ls(pattern = "exports_all"))