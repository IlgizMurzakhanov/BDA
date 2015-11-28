library(rstan)
library(dplyr)
library(ggplot2)
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
rate <- rate - mean(rate)
N <- length(age)
decades <- rep(NA, length(age))
decades[age %in% 35:44] <- 1
decades[age %in% 45:54] <- 2
decades[age %in% 55:65] <- 3
N_decades <- 3
age <- age - 35

fit <- stan("simple_model.stan", iter = 1000)

m <- as.data.frame(monitor(fit))
means <- select(m, `mean`)
years <- 1999:2013
younger <- means[21:35,]
middle <- means[36:50,]
older <- means[51:65,]
df <- data.frame(years, younger, middle, older)
df <- gather(df, decade, value, younger, middle, older)
ggplot(data = df, aes(x = years, y = value, group = decade, color = decade)) + geom_line()

age <- death_rates$Age
rate <- deaths/population * 1e5
year <- as.factor(death_rates$Year)
ggplot(data = death_rates, aes(x = age, y = rate, group = interaction(gender, year), color = year)) + geom_line()