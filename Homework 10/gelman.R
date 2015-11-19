library(rstan)
setwd("~/Documents/BDA/Homework 10")
death_rates <- read.delim("~/Documents/BDA/Homework 10/cdc/white_nonhisp_death_rates_from_1999_to_2013_by_sex.txt")

age <- death_rates$Age
year <- as.factor(death_rates$Year)
N_years <- nlevels(year)
year <- as.numeric(year)
gender <- as.factor(death_rates$Male)
N_genders <- nlevels(gender)
gender <- as.numeric(gender)
deaths <- death_rates$Deaths
population <- death_rates$Population
rate <- deaths/population
V <- rate * (1 - rate) / population * 1e10
rate <- rate * 1e5
#rate <- rate - mean(rate)
N <- length(age)
age <- age - 35

fit <- stan("simple_model.stan")