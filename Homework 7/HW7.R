library(rstan)
library(dplyr)
library(ggplot2)
setwd("~/Documents/BDA/Homework 7")
naes04 <- read.csv("naes04.csv")
naes04$ageBin <- cut(naes04$age, 16, labels = c(seq(20, 95, 5)))
by_age_gender <- group_by(naes04, ageBin, gender)
pp_naes <- summarise(by_age_gender,
          count = n(),
          gks = mean(as.numeric(gayKnowSomeone) - 1, na.rm = TRUE))
pp_naes$sigma_sq <- (pp_naes$gks * (1 - pp_naes$gks))/pp_naes$count
ageBin_data <- as.numeric(as.character(pp_naes$ageBin))[1:32]
gks <- pp_naes$gks[1:32]
gks_data <- gks - mean(gks)
sigma_sq <- pp_naes$sigma_sq[1:32]
gender_data <- as.numeric(pp_naes$gender)[1:32] - 1
N_data <- length(gks_data)
fit <- stan(file="gp.stan")

ageBin_sim <- (0:100)/1;
N_sim <- length(ageBin_sim);

tau_sq <- 0.19
l_sq <- 1090.32
fit_sim <- stan(file="gp-predict.stan")
df <- data.frame(x=ageBin_sim, y_sim=colMeans(fit_sim_ss$gks)+mean(gks));
plot <- qplot(x,y_sim, data=df);