library(rstan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(mvtnorm)

log_posterior <- function(age_data, gender_data, tau_sq_f, tau_sq_m, l_sq_f, l_sq_m, sigma_sq) {
  N_data <- length(ages)
  Sigma_m <- matrix(, nrow = N_data, ncol = N_data)
  Sigma_f <- matrix(, nrow = N_data, ncol = N_data)
  Sigma <- matrix(, nrow = N_data, ncol = N_data)
  for (i in 1:N_data-1) {
    for (j in i+1:N_data) {
      Sigma_m[i,j] <- (1 - gender_data[i]) * (1 - gender_data[j]) * 
        (tau_sq_m * exp((-(ageBin_data[i] - ageBin_data[j])^2)/l_sq_m))
      Sigma_f[i,j] <- gender_data[i] * gender_data[j] * 
        (tau_sq_f * exp((-(ageBin_data[i] - ageBin_data[j])^2)/l_sq_f))
    }
  }
  Sigma <- diag(sigma_sq)
  Sigma <- Sigma + Sigma_m + Sigma_f
  dmvnorm(age_data, mean = rep(0, N_data), sigma = Sigma, log = TRUE)
}

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

samps <- rstan::extract(fit, permuted = TRUE)
tau_sq_m <- mean(samps$tau_sq_m)
tau_sq_f <- mean(samps$tau_sq_f)
l_sq_m <- mean(samps$l_sq_m)
l_sq_f <- mean(samps$l_sq_f)

ageBin_sim <- c(18:97, 18:97);
N_sim <- length(ageBin_sim);
gender_sim <- c(rep(0, N_sim/2), rep(1, N_sim/2));

fit_predict <- stan(file="gp-predict.stan");
fit_predict_ss <- rstan::extract(fit_predict, permuted=TRUE);
df <- data.frame(x_m=ageBin_sim, y_sim_m=colMeans(fit_predict_ss$gks)+mean(gks));
sims <- as.data.frame(t(fit_predict_ss$gks+mean(gks)));
sims$ageBin <- ageBin_sim;
sims$gender <- as.factor(gender_sim);
sims_df <- gather(sims, sim, gks, -ageBin, -gender)
data_df <- data.frame(gks=gks_data+mean(gks), ageBin=ageBin_data, gender=gender_data)
p <- ggplot()
p <- p + geom_line(data=sims_df, aes(x=ageBin, y=gks, color=gender, group=interaction(sim, gender), alpha=.05))
p <- p + geom_point(data=data_df, aes(x=ageBin, y=gks))
p
