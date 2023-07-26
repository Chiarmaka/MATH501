
setwd("D://MATH500")
#
# You may have a different working directory
#
# The IMPORTANT POINT is that any file that you read in is there
#
## ------------------------------------------------------------------------
#
# Reading in a file
#
# Let's read in questionnaire data 
#
# The data are supplied in the file "MATH500_Questionnaire_Data.csv"
#
# Please have a look at that file
#
# The variables correspond to the following questions: 
#
# Height:	What is your height (in cms)?
# Age:	What is your age in years (as a decimal)?
# Sex:	What is your sex?  (F for female, M for male)
# BirthPlace:	Where were you born?
# SiblingsNo:	How many siblings (brothers and sisters, including step-brothers and step-sisters) do you have?
# EatMeat:	Do you eat meat? (Yes/No)
# DrinkCoffee:	Do you drink coffee? (Yes/No)
# LikeBeer:	Do you like beer? (Yes/No)
# Sports:	Do you like play sport? (Yes/No)
# Driver:	Do you have a full driving licence? (Yes/No)
# LeftHanded:	Are you left handed? (Yes/No)
# Abroad:	Did you go abroad on holiday this year? (Yes/No)
# Sleep:	How much sleep do you think that you had last night? (in hours)
# Rent:	How much do you pay each calendar month for your term time accommodation (in pounds)?
# Happy_accommodation:	Are you happy with the quality of your term time accommodation? (Yes/No)
# Distance:	How far is your term time accommodation from the Babbage Building (to the nearest 0.1 of a mile, best guess)
# Travel_time:	How long does it take you to travel from your term time accommodation to the Babbage Building (in minutes, best guess)
# Mode_of_transport:	What is your usual way of travelling to the University? (Foot, Bicycle, Motorbike,  Car,  Bus, Train,  Other; if you use more than one means of transport, please state the one which takes the most time)?
# Safe:	Do you feel safe returning to your term time accommodation at night? (Yes/No)
#
#
# There are many ways of reading data into R
#
# One of the easiest ways of reading a comma separated variable file into R
# is to use the function read_csv from the readr package, which you may have to install on your own machine
#
## install.packages("readr", repos = "http://www.stats.bris.ac.uk/R/")  # Install the package on your own machine
#
# Load the readr package
#
library(readr)
#
# Read in the data in the file "MATH500_Questionnaire_Data.csv"
#
qd <- read_csv("MATH500_Questionnaire_Data.csv",
               locale = locale(tz = "Europe/London")) # this line specifies the locale time zone and was only added 
                                                      # due to problems with the R installation on University machines
#
# On your own machine, you simply need to type:
# qd <- read_csv("MATH500_Questionnaire_Data.csv")
#
# See the first few rows
#
head(qd) 
#
# Variable or column names
#
names(qd) 
#
# Dimension, number of rows and number of columns
#
dim(qd)
nrow(qd)
ncol(qd)
#
## ------------------------------------------------------------------------
#
# Reading in data from an Excel worksheet
#
# R can read in data from a variety of software including Excel
#
# Here we illustrate how to read in data from an Excel worksheet
#
# It is usually safer practice to convert an Excel worksheet to a comma separated variable file
#
# Here, we read in the data directly from Excel
#
# We use the readxl package, which you may have to install on your own machine
#
## install.packages("readxl", repos = "http://www.stats.bris.ac.uk/R/")  # Install the package on your own machine
#
# Load the readxl package
#
library(readxl)
#
#
# Read in the data from Excel using the read_excel function
#
# The data are in the file MATH500_Questionnaire_Data.xlsx
# which is assumed to be in the same working directory.
# 
# Please have a look at that file
#
qd_excel <- read_excel("MATH500_Questionnaire_Data.xlsx", 
                       sheet = "MATH500_Questionnaire_Data") # Specify the sheet
#
# See the first few rows
#
head(qd_excel) 
#
# Variable or column names
#
names(qd_excel)
#
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
#
# Manipulating data
#
# The dplyr package provides us with excellent tools for manipulating data
# You may have to install this package on your own machine
#
## install.packages("dplyr", repos = "http://www.stats.bris.ac.uk/R/")  # Install the package on your own machine
#
# Load the dplyr package
#
library(dplyr)
#
#
# We will work with the qd data frame that contains the data in the file MATH500_Questionnaire_Data.csv
#
# Let's remind ourselves of the columns or variables of the qd data frame and its structure
#
names(qd)
str(qd) # A lot of information
#
# Display less information
#
str(qd, max.level = 1)
#
## ------------------------------------------------------------------------
#
# We can **select** columns or variables
#
select(qd, Height, Travel_time)
#
# Several packages have a function called select
# If there is every a conflict, we can specify that select from the dplyr package should be used 
# by using ::
#
dplyr::select(qd, Height, Travel_time)
#
## ------------------------------------------------------------------------
#
# We can **filter** rows according to a condition
#
filter(qd, Sex == "Male") # Please note the double equals ==
# This is a logical equals.  It asks a question:  Is Sex equal to "Males"?
#
filter(qd, Sex == "Male" & Height > 170) # & means and
#
## ------------------------------------------------------------------------
#
# We can *summarise* variables
#
# A data frame results
#
# Sample mean of Height
#
summarise(qd, ave = mean(Height)) # mean is a measure of location
#
# The previous code shows you the rounded result.
# To see decimal places you need to transform the result into a data frame:
#
as.data.frame(summarise(qd, ave = mean(Height)))
#
#
# Standard deviation of Height
#
summarise(qd, sd = sd(Height)) # standard deviation is a measure of spread
#
# More than one summary statistic
#
summarise(qd, ave = mean(Height), sd = sd(Height))
#
# TASK FOR YOU: work out the mean and median of Distance in qd...
#
## ------------------------------------------------------------------------
#
# We can *count* the number of values
#
count(qd, Sex) # tabulation
#
## ------------------------------------------------------------------------
#
# We can work out summarise *grouped by* other variables
#
# Group by Sex
#
qd_by_Sex <- group_by(qd, Sex)
#
# The sample mean and the sample standard deviation for each level of sex
#
summarise(qd_by_Sex, ave = mean(Height), sd = sd(Height))
#
## ------------------------------------------------------------------------
#
# We can chain these commands together using %>%
#
# Here we repeat the above
#
# We take qd, 
# we push it into the group_by function to group it by Sex,
# we then push the result into the summarise function
#
qd %>% group_by(Sex) %>% summarise(ave = mean(Height), sd = sd(Height))
#
## ------------------------------------------------------------------------
#
# We can use the function n to count the number of observations
#
qd %>% group_by(Sex) %>% 
  summarise(ave = mean(Height), sd = sd(Height), n = n()) 
#
#
#
