library(rstan)
setwd("~/Documents/BDA/Homework 10")
causes_of_death <- read.delim("~/Documents/BDA/Homework 10/causes_of_death.txt")
ok <- causes_of_death["Hispanic.Origin.Code"]=="2186-2"
nhl <- causes_of_death[ok,]

age <- nhl$Single.Year.Ages.Code
year <- as.factor(nhl$Year.Code)
N_years <- nlevels(year)
year <- as.numeric(year)
gender <- as.factor(nhl$Gender.Code)
N_genders <- nlevels(gender)
gender <- as.numeric(gender)
region <- as.factor(nhl$Census.Region.Code)
N_regions <- nlevels(region)
region <- as.numeric(region)
deaths <- nhl$Deaths
population <- nhl$Population
rate <- deaths/population
V <- rate * (1 - rate) / population * 1e10
rate <- rate * 1e5
#rate <- rate - mean(rate)
N <- length(age)

fit <- stan("linear_model.stan")

male <- gender=="M"
female <- gender=="F"
R1 <- region=="CENS-R1"
R2 <- region=="CENS-R2"
R3 <- region=="CENS-R3"
R4 <- region=="CENS-R4"
mort_data <- data.frame(age, year, gender, region, deaths, population)

fit <- stan("linear_model.stan")

XB[i] <- beta_a * age[i] + beta_y * year[i] + beta_m * male[i] + beta_f * female[i] + beta_R1 * R1[i] + beta_R2 * R2[i] + beta_R3 * R3[i] + beta_R4 * R4[i] + beta_R1m * R1 * male[i] + beta_R1f * R1[i] * female[i] + beta_R2m * R2[i] * male[i] + beta_R2f * R2[i] * female[i] + beta_R3m * R3[i] * male[i] + beta_R3f * R3[i] * female[i] + beta_R4m * R4[i] * male[i] + beta_R4f * R4[i] * female[i] + constant;

years_1 <- 1999:2013
ages_decade <- list(35:44, 45:54, 55:64)
male_raw_death_rate <- array(NA, length(years_1))
female_raw_death_rate <- array(NA, length(years_1))
avg_death_rate <- array(NA, length(years_1))
male_avg_death_rate <- array(NA, length(years_1))
female_avg_death_rate <- array(NA, length(years_1))
R1_avg_death_rate <- array(NA, length(years_1))
R2_avg_death_rate <- array(NA, length(years_1))
R3_avg_death_rate <- array(NA, length(years_1))
R4_avg_death_rate <- array(NA, length(years_1))
R1_male_avg_death_rate <- array(NA, length(years_1))
R2_male_avg_death_rate <- array(NA, length(years_1))
R3_male_avg_death_rate <- array(NA, length(years_1))
R4_male_avg_death_rate <- array(NA, length(years_1))
R1_female_avg_death_rate <- array(NA, length(years_1))
R2_female_avg_death_rate <- array(NA, length(years_1))
R3_female_avg_death_rate <- array(NA, length(years_1))
R4_female_avg_death_rate <- array(NA, length(years_1))


data <- nhl
male <- data[,"Gender.Code"]=="M"
R1 <- data[,"Census.Region.Code"]=="CENS-R1"
R2 <- data[,"Census.Region.Code"]=="CENS-R2"
R3 <- data[,"Census.Region.Code"]=="CENS-R3"
R4 <- data[,"Census.Region.Code"]=="CENS-R4"
for (i in 1:length(years_1)){
  ok <- data[,"Year"]==years_1[i] & data[,"Single.Year.Ages.Code"] %in% ages_decade[[2]]
  avg_death_rate[i] <- mean(data[ok,"Deaths"]/data[ok,"Population"])
  male_avg_death_rate[i] <- mean(data[ok&male,"Deaths"]/data[ok&male,"Population"])
  female_avg_death_rate[i] <- mean(data[ok&!male,"Deaths"]/data[ok&!male,"Population"])
  R1_avg_death_rate[i] <- mean(data[ok&R1,"Deaths"]/data[ok&R1,"Population"])
  R2_avg_death_rate[i] <- mean(data[ok&R2,"Deaths"]/data[ok&R2,"Population"])
  R3_avg_death_rate[i] <- mean(data[ok&R3,"Deaths"]/data[ok&R3,"Population"])
  R4_avg_death_rate[i] <- mean(data[ok&R4,"Deaths"]/data[ok&R4,"Population"])
  R1_male_avg_death_rate[i] <- mean(data[ok&R1&male,"Deaths"]/data[ok&R1&male,"Population"])
  R2_male_avg_death_rate[i] <- mean(data[ok&R2&male,"Deaths"]/data[ok&R2&male,"Population"])
  R3_male_avg_death_rate[i] <- mean(data[ok&R3&male,"Deaths"]/data[ok&R3&male,"Population"])
  R4_male_avg_death_rate[i] <- mean(data[ok&R4&male,"Deaths"]/data[ok&R4&male,"Population"])
  R1_female_avg_death_rate[i] <- mean(data[ok&R1&!male,"Deaths"]/data[ok&R1&!male,"Population"])
  R2_female_avg_death_rate[i] <- mean(data[ok&R2&!male,"Deaths"]/data[ok&R2&!male,"Population"])
  R3_female_avg_death_rate[i] <- mean(data[ok&R3&!male,"Deaths"]/data[ok&R3&!male,"Population"])
  R4_female_avg_death_rate[i] <- mean(data[ok&R4&!male,"Deaths"]/data[ok&R4&!male,"Population"])
}

plot(years_1, avg_death_rate/avg_death_rate[1], xaxt="n", yaxt="n", ylim=range(.65,1.25), type="n", bty="n", xaxs="i", yaxs="i", xlab="", ylab=if (j==1) "Relative death rate" else "", main=paste("Age-adj, ", min(ages_decade[[j]]), "-", max(ages_decade[[j]]), sep=""))
lines(years_1, R1_avg_death_rate/R1_avg_death_rate[1], col="blue")
lines(years_1, R2_avg_death_rate/R2_avg_death_rate[1], col="red")
lines(years_1, R3_avg_death_rate/R3_avg_death_rate[1], col="green")
lines(years_1, R4_avg_death_rate/R4_avg_death_rate[1], col="orange")
axis(1, seq(1990,2020,5))
axis(2, seq(.6,1.2,.2))

plot(years_1, avg_death_rate/avg_death_rate[1], xaxt="n", yaxt="n", ylim=range(.65,1.25), type="n", bty="n", xaxs="i", yaxs="i", xlab="", ylab=if (j==1) "Relative death rate" else "", main=paste("Age-adj, ", min(ages_decade[[j]]), "-", max(ages_decade[[j]]), sep=""))
lines(years_1, R1_male_avg_death_rate/R1_male_avg_death_rate[1], col="blue")
lines(years_1, R1_female_avg_death_rate/R1_female_avg_death_rate[1], col="red")
axis(1, seq(1990,2020,5))
axis(2, seq(.6,1.2,.2))
