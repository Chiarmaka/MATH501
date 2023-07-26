
#
x <- c(53, 43, 58, 38, 49, 55)
#
# x is an R object containing the data
# To see an R object, just type its name
#
x
#
# This is the age data, the units of which are months
#
# Another example
#
y <- c(98, 91, 104, 89, 97, 99)
y
#
# This is the height data, the units of which are cms
#
# Other names can be used
#
# It's a good idea to use more meaningful names
#
age <- c(53, 43, 58, 38, 49, 55)
age
height <- c(98, 91, 104, 89, 97, 99)
height
#
# Here we'll work with x and y, so that the code can be generalized
#
## ------------------------------------------------------------------------
#
# We can access individual elements of an R object or vector using the square brackets
#
x[2]
2:4 # A integer sequence from 2 to 4
x[2:4]
#
# To access the 3rd and 5th element
#
x[c(3,5)] # Note that we collect together 3 and 5 using c
#
# To access all elements except the 3rd and 5th
#
x[-c(3,5)]
#
#
## ------------------------------------------------------------------------
#
# Creating a data frame
#
# A data frame is a very important R object
# It allows us to keep related data together
#
df <- data.frame(x, y)
df
#
# *** Rows correspond to individuals and columns correspond to variables ***
# Just like in an Excel spreadsheet
#
View(df)
#
# The column names can be found
#
names(df)
#
# To access individual columns, i.e. individual variables
# use the dollar
#
df$x
df$y
#
# Alternatively
#
df[["x"]]
df[["y"]]
#
#
## ------------------------------------------------------------------------
#
# Plotting y (on the vertical axis) against x (on the horizontal axis)
#
# THE KEY IS TO BUILD UP YOUR USE OF R FUNCTIONS ONE STEP AT A TIME
#
# Basic data visualizations with ggplot2
#
# Load the ggplot2 package
#
library(ggplot2)
#
ggplot(df, aes(x = x, y = y)) + 
  # We tell ggplot2 where to look for the data (in the data frame df) and 
  # the **general features** or **aesthetics** of the graph,
  # that is, what to plot on the x and y axis, and
  geom_point() 
#
#
# With axis labels
#
ggplot(df, aes(x = x, y = y)) + 
  geom_point() +
  labs(x = "Age (months)", y = "Height (cms)") # add labels for axes
#
# With a main title
#
ggplot(df, aes(x = x, y = y)) + 
  geom_point() +
  labs(x = "Age (months)", 
       y = "Height (cms)", 
       title = "Data from Children") # add title
#
# With a sub-title
#
ggplot(df, aes(x = x, y = y)) + 
  geom_point() +
  labs(x = "Age (months)", 
       y = "Height (cms)", 
       title = "Data from Children",
       subtitle = "Random sample of size 6") # add subtitle
#
# With a better plotting character
#
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 3) + # increase size of points
  labs(x = "Age (months)", 
       y = "Height (cms)", 
       title = "Data from Children",
       subtitle = "Random sample of size 6") 
#
# With some colour
#
ggplot(df, aes(x = x, y = y)) + 
  geom_point(size = 3, col = "red") + # plot red points
  labs(x = "Age (months)", 
       y = "Height (cms)", 
       title = "Data from Children",
       subtitle = "Random sample of size 6") 
#
## ------------------------------------------------------------------------
#
# Summary statistics
#
# The sample mean: a one number summary of the data, a typical value telling us where the data typically are
# A measure of location
#
mean(x)
#
# It's the sum of the values divided by the number of values (sample size)
#
sum(x) /length(x)
#
# The sample median is a measure of location that divides the data into two parts with
# the same number of data points in each
# The sample median is less sensitive to outlying points than the sample mean
#
median(x)
#
#
# We can check the median by sorting the values
#
sort(x)
#
#
## ------------------------------------------------------------------------
#
# Getting help
#
?mean
help("median")
#
#
# Or use the help window
#
## ------------------------------------------------------------------------
#
# Factors
#
# Let's now assume that we know the sex of each child
#
sex <- c("M","M","M","F","F","F") # The sex of each child
sex
#
# "F" and "M" are characters that are label of the sex of each child
# To allow R to work with data like this conprising labels
# we covert the R object into a factor
#
sex_f <- factor(sex) # Make it into a factor, as "F" and "M" are labels
sex_f
#
# We can tabulate using table:
#
table(sex_f)
#
# Put all the data into a data frame and look at it
#
df_2 <- data.frame(x, y, sex_f) 
df_2
str(df_2) # To see the structure
#
## ------------------------------------------------------------------------
#
# Some more on factors
#
# Factors will be very important to us later in this module
# Here is another example
#
# Data on the agreement of a group of people are recorded as 0 for "No" and 1 for "Yes":
#   
agreement <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
agreement
#
# Let's turn this into a factor with appropriate labels:
#
agreement_f  <- factor(agreement, levels = c(0,1), labels = c("No", "Yes"))
agreement_f
#
# or, slightly shorter
#
agreement_f <- factor(agreement, levels = 0:1, labels = c("No", "Yes"))
agreement_f
#
# We can tabulate using table:
#
table(agreement_f)
#
# We can impose a different order
#
agreement_f <- factor(agreement, levels = c(1,0), labels = c("Yes", "No"))
agreement_f
table(agreement_f)

