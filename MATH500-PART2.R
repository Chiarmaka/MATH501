 # EU IMMIGRATION
## The following are the library that has been used to develop this coursework.
## These libraries need to be loaded before start to 
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(geofacet)
library(ukbabynames)
library(scales)
#
##############################################################################
##
##
## 3.1 Writing R code to produce a sophisticated Graph of EU Immigration Data
##
##
##############################################################################
## The initial task of this coursework is to produce and improve the graph of
## EU Immigration Data. Before we start to work, setting up the work directory 
## should be the first step. This can be done by using for example:
setwd("C:\\Users\\Chiamaka\\Downloads\\Portfolio\\MATH500-PART2 (2)\\MATH500-PART2\\MATH500-PART2")
#
## Now we can start to work on the tasks.
## We can load the data that has been given, this file name is eu_immigration_data.csv.
## Before load the data we can use library(readr) to enable read function.
## If you do not have it, install the package by using install.packages("readr")
## Load csv data
#
eu_immigration <- read_csv("eu_immigration_data.csv")
#
## Separate column
#
### Now we want to seperate the data accordingly.
### The data are about refugess and the variables are unit, citizen, sex, age, geotime.
### To do seperate function, tidyr package can do the job for us.
### If you have not install the package you can do install.packages("tidyr")
eu_separate <- eu_immigration %>% separate(col = "unit,citizen,sex,age,geotime", 
                                           into = c("unit", "citizenship", "sex", "age", "eu_code"), sep = ",")
#
## Remove specific value on eu_code
#
### Now the next step is to remove some data that we do not need right now
### Filtering data can do the job for us to 'remove' the value from the csv files.
### NO (= Norway), EU28 (= EU total) and TOTAL (=Europe total, including countries belonging to the European Free Trade Association)
### The following are what we need to remove, CH(= Switzerland), IS (= Iceland), LI (= Liechtenstein) from eu_code variable.
#
eu_separate_filter <- eu_separate %>% filter(eu_code != "CH" & eu_code != "IS" & 
                                                eu_code != "LI" & eu_code != "NO" & 
                                                eu_code != "EU28" & eu_code != "TOTAL")
#
## Load and modify eu_code variable to create a new variable specifying
#
### After we remove the values that we want to remove, now we can modify the eu_code variable a new variable
### specifying the name of EU country corresponding to each country code.
### Now let's call each country code to see the available country code.
#
levels(factor(eu_separate_filter$eu_code))
# 
## Now we can see that the following are the countries code that we have collected:
## [1] "AT" "BE" "BG" "CY" "CZ" "DE" "DK" "EE" "EL" "ES" "FI" "FR" "HR" 
## "HU" "IE" "IT" "LT" "LU" "LV" "MT" "NL" "PL" "PT" "RO" "SE"
## [26] "SI" "SK" "UK"
## Now let's define each country code with the real country name. 
## To do so we can use mutate function to change the variable/value name.
## Be aware of typo, comma, and unnecessary spaces.
## The following are the website we use to define each country code: https://www.w3schools.com/tags/ref_country_codes.asp
#
eu_separate_filter <- eu_separate_filter %>% mutate(country = 
                                      recode(eu_code, "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria",
                                             "CY" = "Cyprus", "CZ" = "Czech Republic", "DE" = "Germany",
                                             "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain",
                                             "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary",
                                             "IE" = "Ireland", "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg",
                                             "LV" = "Latvia", "MT" = "Malta", "NL" = "Netherlands", "PL" = "Poland",
                                             "PT" = "Portugal", "RO" = "Romania", "SE" = "Sweden", "SI" = "Slovenia",
                                             "SK" = "Slovakia", "UK" = "United Kingdom"))
#
## Now our next task is to transfrom the dataframe in the long form to obtain one column specifying the year and
## one column specifying the number of refugees.
## To do so, we can use gather() function to the job. This will help us to take multiple columns and collapse 
## them into key-value pairs, duplicating all other columns as needed.
#
eu_separate_gather <- eu_separate_filter %>% gather(key = "year", "refugee_number", 6:15)
#
## Remove missing data
#
### now our task is to remove missing data. Meaning that column that has NA should be removed.
### there is no data recorded, therefore, because we are not going to use it so it is better to remove it.
### na.omit function would be useful because this function removes all incomplete cases of 
### data object (typically dataframe, matrix, or vector)
#
eu_separate_gather <- eu_separate_gather%>% na.omit()
#
## Next our task is to calculate the total number of refugees grouped by EU country, age group and year.
## Before to go to next task, lets filter the sex and age to clean data.
#
eu_separate_filter_2 <- eu_separate_gather %>% filter(sex == "T" & age != "TOTAL" & 
                                          age != "UNK" & age != "Y_LT18")
#
## Now lets group it by country, age, and year then we summarise to calculate the total number of refugees grouped by EU 
## country, age group and year.
#
eu_separate_summarise <- eu_separate_filter_2 %>% group_by(country,age,year) %>% 
                          summarise(sum_refugee = sum(refugee_number)) %>%
## We can also use mutate to change the age into suitable label  
  mutate(age_f = factor(age, 
                        levels = c ("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"),
                        labels = c ( "0-13", "14-17", "18-34","35-64", "65+"))) # make age into a factor 
#
## Now we can produce the data visualization from this data.
## Plot it using geofacet will improve the data visualization based on the geographical entities into a grid
## that preserves some of the geographical orientation.
## To do so we can use ggplot and geofacet library and if you do not have you can install.packages("geofacet") 
## and install.packages("ggplot2")
#
## Lets produce the plot

eu_separate_summarise$year <- as.numeric(eu_separate_summarise$year)
#


#
ggplot(eu_separate_summarise, aes(x = year, y = sum_refugee, colour = age_f, group = 1)) +
  geom_point() +
  geom_line() +
  labs(title = "Annual Number of Refugees for Each EU Country by Age Group", x = "Year",
       y = "Number of Refugees (using a square-root scale)", colour = "Age Group")  +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, span = 1) +
  facet_geo(~ country, grid = "eu_grid1", scales = "free_y") +
  scale_y_sqrt() +
  scale_x_continuous(limits = c(2008,2017),
                     breaks = c(2008,2011,2014,2017),
                     labels = function(x) paste("'", substr(x,3,4)))
                     
#
##
###############################################################################
##
##
## 3.2 Manipulate, Summarise and Visualize a Larga Data Set about UK Baby Names
##
##
##############################################################################
#
## The next task on this coursework is to working on with UK baby names
## R has package ukbabynames and it provides a full listing of UK baby names occurring more than three times per year
## between 1996 and 2015 in R obbject ukbabynames
## We can install the package by install.packages("ukbabynames")
## After it finish let's upload new csv file that has been given.
## the file name is invented_data_for_illustration.csv
#
## Load data
#
b_d <- read_csv("invented_data_for_illustration.csv")
#
# Work out percentages for each year and sex combination
# using the dplyr package
# If you do not have dplyr installed, you can install.packages("dplyr")
#
b_p <-b_d %>%
          group_by(year, sex) %>%
          mutate(p = n / sum(n))
#
# Load to see the results
b_p
#
## Now lets plot the content of b_p using ggplot2
#
ggplot(b_p,
       aes(x = year, y = p, colour = sex, group = sex)) +
  geom_point() + 
  geom_line() +
  labs(y = "Proportion of babies having the given name") +
  facet_wrap(~ name) + 
  scale_x_continuous(breaks = c(1964, 1965),
                     minor_breaks = NULL,
                     limits = c(1963.5, 1965.5)) +
  scale_y_continuous(label = percent) + #From the scales package
  theme(legend.position = "bottom")
#
## Now let's working on specific baby names
## We can use the above code and improve it to produce the desire plot
## Before we started the plot let's filter the baby name into "Luciana"
#
baby_luciana <- ukbabynames %>%
              group_by(year, sex) %>%
              mutate(p = n / sum(n))%>%
              filter(name == "Luciana")
baby_luciana
#
## Now let's produce the plot to visualise UK baby names 'Luciana'
#
ggplot(baby_luciana,
       aes(x = year, y = p, colour = sex, group = sex)) +
  geom_point() +
  geom_line() +
  labs(y = "Proportion of babies having the given name",
       title = "Babies' name in the UK, 1996-2015",
       subtitle = "Data from the R package ukbabynames") +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, span = 1) +
  facet_wrap(~ name) + 
  scale_y_continuous(label = percent) +
  scale_x_continuous(limits = c(1995,2015),
                     breaks = c(1995,2000,2005,2010,2015)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))
#
## Now on the next task we need to plot two different name
## Let's filter the name Charlie and Alex
## We will use the same above code and improve it.
#
baby_charlie_and_alex <- ukbabynames %>%
                group_by(year, sex) %>%
                mutate(p = n / sum(n))%>%
                filter(name %in% c("Alex", "Charlie")) 
baby_charlie_and_alex
#
## Now let's produce the plot to visualise UK baby names 'Charlie' and 'Alex' *this part has not completed.
#
ggplot(baby_charlie_and_alex,
       aes(x = year, y = p, colour = sex, group = sex)) +
  geom_point() +
  geom_line() +
  labs(y = "Proportion of babies having the given name",
       title = "Babies' name in the UK, 1996-2015",
       subtitle = "Data from the R package ukbabynames") +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, span = 1) +
  facet_wrap(~ name, scales = "free_y" ) + 
  scale_y_continuous(label = percent) +
  scale_x_continuous(limits = c(1995,2015),
                     breaks = c(1995,2000,2005,2010,2015)) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "light yellow"))
#
## Now our next task is visualizes the popularity trends over time of the top three female and males names
## that were the most popular in 1996 and in 2015.
## To do so we can filter it by rank variable of ukbabynames dataset.
## Let's filter the ukbabynames dataset by rank and filter the whole dataset 
## by the most popular name that we have been identified in each one of the two considered years.
#
## Now let's produce the plot it together for the year 1996
#
popular_name_1996 <- function(x)
{
  names_df <- data.frame(ukbabynames) 
  names_new <- names_df %>% group_by(year, sex) %>% mutate(p = n / sum(n))
  names_new_2 <- names_new %>% filter(rank <= 3 & year == "1996")
  baby_name <- names_new_2$name
  baby_name
  names_filtered <- names_new %>% filter(name %in% baby_name) %>% mutate(Gender = recode(sex, "F" = "Female", "M" = "Male"))
  names_filtered
  ggplot(names_filtered, aes(x = year, y = p, group = name, color = name, shape = Gender, size=2)) +
    scale_y_continuous(label = percent)+
    scale_x_continuous(limits = c(1995,2015),
                       breaks = c(1995,2000,2005,2010,2015)) +
    geom_point() +
    geom_line(lwd = 1) +
    facet_wrap(~Gender)+
    labs(title = "Most Popular Names of 1996", x = "Year", y = "Proportion of babys having the giving name") +
    theme(legend.position = "bottom") 
  
}
popular_name_1996 (ukbabynames)
#
## Now we can plot it together for the year 2015
#
popular_name_2015 <- function(x)
{
  names_df <- data.frame(ukbabynames) 
  names_new <- names_df %>% group_by(year, sex) %>% mutate(p = n / sum(n))
  names_new_2 <- names_new %>% filter(rank <= 3 & year == "2015")
  baby_name <- names_new_2$name
  baby_name
  names_filtered <- names_new %>% filter(name %in% baby_name) %>% mutate(Gender = recode(sex, "F" = "Female", "M" = "Male"))
  names_filtered
  ggplot(names_filtered, aes(x = year, y = p, group = name, color = name, shape = Gender, size=2)) +
    scale_y_continuous(label = percent)+
    scale_x_continuous(limits = c(1995,2015),
                       breaks = c(1995,2000,2005,2010,2015)) +
    geom_point() +
    geom_line(lwd = 1) +
    facet_wrap(~Gender)+
    labs(title = "Most Popular Names of 2015", x = "Year", y = "Proportion of babys having the giving name") +
    theme(legend.position = "bottom") 
  
}
popular_name_2015 (ukbabynames)
#############################################
##
## I'm working on it right now.
##
#############################################
#
## The coefficient of variation is the next task that we need to produce
## Before we started coding the following is how to work out the coefficient of variation
#
coef_var <- function(x){
  sqrt((length(x) - 1)/length(x) * var(x)) / mean(x)
}
#
x <- c(1000, 1000, 1000)
coef_var(x)
#
## Now lets write code that computes and visualise the coefficient of variation across year for both sexes on the same graph
## Lets calculate the summarise that coun of the names within year and sex.
#
coef_var <- function(x){
  sqrt((length(x) - 1)/length(x) * var(x)) / mean(x)
  
}

baby_names_1996_to_2015 <- ukbabynames %>%
  group_by(year, sex) %>%
  mutate(coef = sqrt((length(n) - 1)/length(n) * var(n))/mean(n))

baby_names_1996_to_2015
ukbabynames

ggplot(baby_names_1996_to_2015, aes(x = year, y = coef, group = sex, color = sex)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(limits = c(1996,2015), breaks = c(1996,2000,2005,2010,2015))+
  labs(title = "Coefficient of variation of babies' names in the UK, 1996???2015", 
       subtitle = "Data from the R package ukbabynames", x = "Year", 
       y = "Coefficient of variation as a measure of inequality") +
  theme(legend.position = "bottom") 




