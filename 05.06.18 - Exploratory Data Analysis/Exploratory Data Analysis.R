# EXPLORATORY DATA ANALYSIS

# Load packages
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(GGally)

# Set working directory
setwd("~/SynopticSignals/05.06.18 - Exploratory Data Analysis")

# Load the Dataset
somerville <- read.csv("Somerville_Happiness_Survey_responses_-_2011__2013__2015.csv")

# Understand variables in the dataset
str(somerville)

attach(somerville)

table(Year)

table(How.happy.do.you.feel.right.now.)

somerville$How.happy.clean <- as.character(somerville$How.happy.do.you.feel.right.now.)
somerville$How.happy.clean <- ifelse(somerville$How.happy.clean %in% c("42", "57", "77", "810", "88", "9*", "i", "r", "R", "X"), NA, somerville$How.happy.clean)
somerville$How.happy.clean <- as.numeric(somerville$How.happy.clean)

table(How.satisfied.are.you.with.your.life.in.general.)

somerville$How.satisfied.life.general.clean <- as.character(somerville$How.satisfied.are.you.with.your.life.in.general.)
somerville$How.satisfied.life.general.clean <- ifelse(somerville$How.satisfied.life.general.clean %in% c("5/19/2011", "5/27/2011", "6/1/2011", "6/6/2011", "99", "r", "R", "X"), NA, somerville$How.satisfied.life.general.clean)
somerville$How.satisfied.life.general.clean <- as.numeric(somerville$How.satisfied.life.general.clean)

mean(somerville$How.satisfied.life.general.clean, na.rm = TRUE)

# Create function to clean factors
clean_vars <- function(x, y){
  y <- as.character(x)
  y <- ifelse(y %in% c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6", "6.5", "7", "7.5", "8", "8.5",
                       "9", "9.5", "10"), y, NA)
  y <- as.numeric(y)
}


# Create function to clean factors small
clean_vars2 <- function(x, y){
  y <- as.character(x)
  y <- ifelse(y %in% c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"), y, NA)
  y <- as.numeric(y)
}

# cleaning first set of survey questions
somerville$How.happy.clean <- clean_vars(somerville$How.happy.do.you.feel.right.now., somerville$How.happy.clean)
somerville$How.satisfied.general.clean <- clean_vars(somerville$How.satisfied.are.you.with.your.life.in.general., somerville$How.satisfied.general.clean)
somerville$How.satisfied.somerville.clean <- clean_vars(somerville$How.satisfied.are.you.with.Somerville.as.a.place.to.live., somerville$How.satisfied.somerville.clean)
somerville$How.satisfied.neighborhood.clean <- clean_vars(somerville$How.satisfied.are.you.with.your.neighborhood., somerville$How.satisfied.neighborhood.clean)
somerville$satisfied.resident.clean <- clean_vars(somerville$How.proud.are.you.to.be.a.Somerville.resident._2015, somerville$satisfied.resident.clean)

somerville$other.people.clean <- clean_vars(somerville$In.general..how.similar.are.you.to.other.people.you.know._2011, somerville$other.people.clean)
somerville$advice.clean <- clean_vars(somerville$When.making.decisions..are.you.more.likely.to.seek.advice.or.decide.for.yourself._2011, somerville$advice.clean)

somerville$safe.neighborhood.clean <- clean_vars(somerville$How.safe.do.you.feel.walking.in.your.neighborhood.at.night_2013, somerville$safe.neighborhood.clean)
somerville$safe.community.clean <- clean_vars(somerville$How.safe.do.you.feel.walking.in.your.community.at.night._2015, somerville$safe.community.clean)
somerville$satisfied.beauty.clean <- clean_vars(somerville$How.satisfied.are.you.with.the.beauty.or.physical.setting.of.your.neighborhood., somerville$satisfied.beauty.clean)

somerville$parks.clean <- clean_vars(somerville$How.satisfied.are.you.with.the.appearance.of.parks.in.your.neighborhood._2013, somerville$parks.clean)
somerville$squares.clean <- clean_vars(somerville$How.satisfied.are.you.with.the.appearance.of.parks.and.squares.in.your.neighborhood., somerville$squares.clean)


# cleaning second set of survey questions 
somerville$city.services.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.availability.of.information.about.city.services._2015, somerville$city.services.clean)

somerville$affordable.clean <- clean_vars2(somerville$The.availability.of.affordable.housing_2011,somerville$affordable.clean)
somerville$cost.housing.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.cost.of.housing., somerville$cost.housing.clean)
somerville$schools.community.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.overall.quality.of.public.schools.in.your.community._2011, somerville$schools.community.clean)
somerville$schools.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.overall.quality.of.public.schools., somerville$schools.clean)
somerville$physical.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.beauty.or.physical.setting_2011, somerville$physical.clean)
somerville$phsycial.setting.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.beauty.or.physical.setting.of.Somerville_2013, somerville$phsycial.setting.clean)
somerville$police.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.effectiveness.of.the.local.police_2011_2013, somerville$police.clean)
somerville$police.2015.clean <- clean_vars2(somerville$How.would.you.rate.the.following..Your.trust.in.the.local.police_2015, somerville$police.2015.clean)
somerville$sidewalks.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.maintenance.of.streets..sidewalks..and..squares_2013, somerville$sidewalks.clean)
somerville$sidewalks.2015.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.maintenance.of.streets.and.sidewalks_2015, somerville$sidewalks.2015.clean)
somerville$events.clean <- clean_vars2(somerville$How.would.you.rate.the.following..The.availability.of.social.community.events, somerville$events.clean)

# Third set of variables to clean
somerville$sex.clean <- as.character(somerville$What.is.your.sex.)  
somerville$sex.clean <-  ifelse(somerville$What.is.your.sex. == "Female, Male" , NA, somerville$sex.clean) 
somerville$gender.clean <- as.character(somerville$What.is.your.gender._2011)
somerville$gender.clean <- ifelse(somerville$What.is.your.gender._2011 == "R", NA, somerville$gender.clean)
somerville$age.clean <- as.character(somerville$Age.)
somerville$age.clean <- ifelse(somerville$Age. == "R", NA, somerville$age.clean) # Needs further cleaning, age groupings different by year.
somerville$marital.clean <- as.character(somerville$Marital.status._2011)
somerville$marital.clean <- ifelse(somerville$Marital.status._2011 == "R", NA, somerville$marital.clean)
somerville$language.clean <- ifelse(somerville$What.language..other.than.English..do.you.speak.at.home._2015 %in% c("none", "None", "NONE", "none", "None (Italian Descent)", NA), 0, 1)
somerville$race.clean <- ifelse(somerville$What.is.your.race_2011_2013 == "White, non-Hispanic", 0, 1)
somerville$hispanic.clean <- as.character(somerville$Are.you.of.Hispanic..Latino..or.Spanish.origin._2013)
somerville$race.2015.clean <- ifelse(somerville$What.is.your.race.or.ethnicity._2015 == "White", 0, 1)
somerville$children.clean <- as.character(somerville$Do.you.have.children.age.18.or.younger.who.live.with.you.)
somerville$children.clean <- ifelse(somerville$Do.you.have.children.age.18.or.younger.who.live.with.you. == "no", "No", ifelse(somerville$Do.you.have.children.age.18.or.younger.who.live.with.you. == "yes", "Yes", somerville$children.clean))
somerville$housing.status.clean <- as.character(somerville$Describe.your.housing.status.in.Somerville.)
somerville$move.away.clean <- as.character(somerville$Do.you.plan.to.move.away.from.Somerville.in.the.next.two.years.)
somerville$move.away.clean <- ifelse(somerville$Do.you.plan.to.move.away.from.Somerville.in.the.next.two.years. == "no", "No", ifelse(somerville$Do.you.plan.to.move.away.from.Somerville.in.the.next.two.years. == "yes", "Yes", somerville$children.clean))
somerville$long.lived.clean <- as.character(somerville$How.long.have.you.lived.here.)
somerville$long.lived.clean <- ifelse(somerville$How.long.have.you.lived.here. == "R", NA, somerville$long.lived.clean)
somerville$income.clean <- as.character(somerville$What.is.your.annual.household.income.)
somerville$income.clean <- ifelse(somerville$What.is.your.annual.household.income. == "R", NA, somerville$income.clean)
somerville$student.clean <- as.character(somerville$Are.you.a.student.)
somerville$student.clean <- ifelse(somerville$Are.you.a.student. == "no", "No", ifelse(somerville$Are.you.a.student. == "yes", "Yes", somerville$student.clean))

# creating some plots by year
ggplot(somerville, aes(x = age.clean)) + geom_bar(position = "dodge") + facet_grid(Year ~ .) + scale_y_log10()

ggplot(somerville, aes(x = income.clean)) + geom_bar(position = "dodge") + facet_grid(Year ~ .) + scale_y_log10()


# main plots
somerville_clean %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

somerville_clean %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()


somerville %>%
  keep(is.character) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()


# creating a subset of the full dataset with just the cleaned variables
somerville_clean <- somerville[,44:82]

# creating the correlation graphs
somerville_clean %>%
  keep(is.numeric) %>%
  cor(., use = "pairwise.complete.obs")

somerville_clean %>%
  keep(is.numeric) %>%
  ggcorr(., use = "pairwise.complete.obs", label = TRUE, label_alpha = TRUE)


head(somerville_clean)

# creating a table of all variables
lapply(somerville, table)

# understanding the structure of the dataset
str(somerville)

table(somerville$How.happy.clean)
str(somerville$How.happy.clean)
mean(somerville$How.happy.clean, na.rm = TRUE)

