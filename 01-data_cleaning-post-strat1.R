#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Grover et al., 2020.
# Author: Rohan Alexander, Sam Caetano, and Siyuan Tao
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca and s.tao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("~/Desktop/STA304 P3")
raw_data_census <- read_dta("input/usa_00002.dta")


# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_census <- 
  raw_data_census %>% 
  select(stateicp,
         sex, 
         age, 
         race, 
         hhincome)
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         #labforce)
         

#### What's next? ####

# We want to modify some levels of the variables to make the variables in the census data match the 
# variables in the survey data.

# The first one is the household_income.
reduced_data_census <-
  reduced_data_census %>%
  mutate(household_income =
           ifelse(hhincome <= 49999, "Less than $49,999", ifelse(
             hhincome <= 99999, "$50,000 to $99,999", ifelse(
               hhincome <= 149999, "$100,000 to $149,999", ifelse(
                 hhincome <= 199999, "$150,000 to $199,999", ifelse(
                   hhincome >=200000, "$200,000 and above" , NA
                 )
               )
             )
           )))

# Then we can remove the original variable "hhincome".
reduced_data_census = subset(reduced_data_census, select = -c(hhincome) )

# The second variable we are going to work with is the variable "age". There are two specific levels
# which are "less than 1 year old" and "90 (90+ in 1980 and 1990)". In this case, to simplify the process,
# we assume that the age of the people who are in "less than 1 year old" is 1, and the age of people
# who are in "90 (90+ in 1980 and 1990)" is 90.

levels(reduced_data_census$age)[levels(reduced_data_census$age)%in%
                                        c("less than 1 year old")] <- 1

levels(reduced_data_census$age)[levels(reduced_data_census$age)%in%
                                  c("90 (90+ in 1980 and 1990)")] <- 90

# Then, we want to modify the variable "race".
levels(reduced_data_census$race)[levels(reduced_data_census$race)%in%
                                  c("white")] <- "White"

levels(reduced_data_census$race)[levels(reduced_data_census$race)%in%
                                   c("black/african american/negro")] <- "Black, or African American"

levels(reduced_data_census$race)[levels(reduced_data_census$race)%in%
                                   c("american indian or alaska native")] <- "American Indian or Alaska Native"

levels(reduced_data_census$race)[levels(reduced_data_census$race)%in%
                                   c("chinese", "japanese", "other asian or pacific islander")] <- "Asian or Pacific Islander"

levels(reduced_data_census$race)[levels(reduced_data_census$race)%in%
                                   c("other race, nec", "two major races", "three or more major races")] <- "Some other race"

# Finally, as we have already change the zipcode form state in survey data to full name form state, 
# we do not have to modify the variable "state".


## Here we want to split cells by age, sex, state, race and household income.

reduced_data_census <- 
  reduced_data_census %>%
  count(age, sex,stateicp, race, household_income ) %>%
  group_by(age, sex,stateicp, race, household_income ) 

# Here, we want to change the variable "age" into integers.
reduced_data_census$age <- as.integer(reduced_data_census$age)

# Finally, to make the census data match the survey data, we need to rename some variables.

reduced_data_census <-
  reduced_data_census %>% 
    rename(
      gender = sex,
      race_ethnicity = race,
      state = stateicp
      )

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_census, "output/census_data.csv")



         