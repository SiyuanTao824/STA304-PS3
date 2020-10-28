#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Tausanovitch, Chris and Lynn Vavreck, (2020).
# Author: Rohan Alexander, Sam Caetano, and Siyuan Tao
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca, and s.tao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("~/Desktop/STA304 P3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("input/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(#interest,
         #registration,
         #vote_2016,
         #vote_intention,
         vote_2020,
         #ideo5,
         #employment,
         #foreign_born,
         gender,
         #census_region,
         #hispanic,
         race_ethnicity,
         household_income,
         #education,
         state,
         #congress_district,
         age)


#### What else? ####
# There are 5 categories under the variable Vote_2020, which are "Donald Trump," "Joe Biden," 
# "I am not sure/don't know," "I am not sure/don't know," and "Someone else." We want to remove the categories
# "I am not sure/don't know," "I would not vote," and "Someone else" to make the prediction more reliable.
levels(reduced_data$vote_2020)[levels(reduced_data$vote_2020)%in%
                                      c("I am not sure/don't know","I would not vote","Someone else")] <- "Others"

reduced_data <-
  reduced_data %>%
  filter(vote_2020 != "Others") 

reduced_data$vote_2020 <- droplevels(reduced_data$vote_2020, "Others")

# There are too many levels in the variable "Race_ethnicity." We want to simplify it.

levels(reduced_data$race_ethnicity)[levels(reduced_data$race_ethnicity)%in%
                          c("Asian (Asian Indian)","Asian (Chinese)","Asian (Filipino)",
                            "Asian (Japanese)","Asian (Korean)","Asian (Vietnamese)",
                            "Asian (Other)", "Pacific Islander (Native Hawaiian)","Pacific Islander (Guamanian)","Pacific Islander (Samoan)",
                            "Pacific Islander (Other)")] <- "Asian or Pacific Islander"


# There are too many household income levels. We want to simplify it.

levels(reduced_data$household_income)[levels(reduced_data$household_income)%in%
                                      c("Less than $14,999","$15,000 to $19,999","$20,000 to $24,999",
                                        "$25,000 to $29,999", "$30,000 to $34,999", 
                                        "$35,000 to $39,999", "$40,000 to $44,999",
                                        "$45,000 to $49,999")] <- "Less than $49,999"

levels(reduced_data$household_income)[levels(reduced_data$household_income)%in%
                                        c("$50,000 to $54,999","$55,000 to $59,999","$60,000 to $64,999",
                                          "$65,000 to $69,999", "$70,000 to $74,999", 
                                          "$75,000 to $79,999", "$80,000 to $84,999",
                                          "$85,000 to $89,999", "$90,000 to $94,999",
                                          "$95,000 to $99,999")] <- "$50,000 to $99,999"

levels(reduced_data$household_income)[levels(reduced_data$household_income)%in%
                                        c("$100,000 to $124,999","$125,000 to $149,999")] <- "$100,000 to $149,999"

levels(reduced_data$household_income)[levels(reduced_data$household_income)%in%
                                        c("$150,000 to $174,999","$175,000 to $199,999")] <- "$150,000 to $199,999"

levels(reduced_data$household_income)[levels(reduced_data$household_income)%in%
                                        c("$200,000 to $249,999","$250,000 and above")] <- "$200,000 and above"

reduced_data <-
  reduced_data %>%
  filter(household_income != "NA's") 

reduced_data$household_income<- droplevels(reduced_data$household_income, "NA's")

# Here, we want to convert the state from zipcodes to fullnames, to better match the data from census.
# To accomplish this, we use a function called "stateFromLower" which is produced by O'Brien (2012).

stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}

reduced_data$state<-stateFromLower(reduced_data$state)

# Besides, we want to rename the levels of gender to make the survey data match the census data.

levels(reduced_data$gender)[levels(reduced_data$gender)%in%
                                        c("Female")] <- "female"


levels(reduced_data$gender)[levels(reduced_data$gender)%in%
                              c("Male")] <- "male"


# Make a new column to show the binary result of vote_2020.

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "output/survey_data.csv")

