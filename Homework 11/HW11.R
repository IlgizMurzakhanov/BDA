library(tidyr)
library(rstan)
library(ggplot2)
setwd("~/Documents/BDA/Homework 11")
y1 <- read.table("dogs.txt", header = TRUE, skip = 1)
y <- ifelse (y1[,]=="S",1,0)
n_dogs <- nrow(y)
n_trials <- ncol(y)

fit <- stan("dogs.stan")
fit2 <- stan("dogs2.stan")

post <- rstan::extract(fit)
beta <- colMeans(post$beta)

invlogit <- function(x){
  exp(x)/(1+exp(x))
}

n_sims <- 50
rep_data <- function(n_sims, n_dogs, n_trials, beta)
{
  y_rep <- array (NA, c(n_sims, n_dogs, n_trials))
  for (j in 1:n_dogs){
    n_avoid_rep <- rep (0, n_sims)
    n_shock_rep <- rep (0, n_sims)
    n_trials_since_shock_rep <- rep (0, n_sims)
    for (t in 1:n_trials){
      if(length(beta) == 4)
        p_rep <- invlogit (beta[j, 1] + beta[j, 2]*n_avoid_rep + beta[j, 3]*n_shock_rep + beta[j, 4]*n_trials_since_shock_rep)
      else
        p_rep <- invlogit (beta[j, 1] + beta[j, 2]*n_avoid_rep + beta[j, 3]*n_shock_rep)
      y_rep[,j,t] <- rbinom (n_sims, 1, p_rep)
      n_avoid_rep <- n_avoid_rep + 1 - y_rep[,j,t] 
      n_shock_rep <- n_shock_rep + y_rep[,j,t]
      for(s in 1:n_sims){
        if (y_rep[s,j,t] == 0) {
          n_trials_since_shock_rep[s] <- n_trials_since_shock_rep[s] + 1;
        }
        else {
          n_trials_since_shock_rep[s] <- 0;
        }
      }
    }
  }
  y_rep
}

rep_data_full <- function(n_sims, n_dogs, n_trials, tau, alpha)
{
  y_rep <- array (NA, c(n_sims, n_dogs, n_trials))
  for (j in 1:n_dogs){
    n_avoid_rep <- rep (0, n_sims)
    n_shock_rep <- rep (0, n_sims)
    n_trials_since_shock <- rep (0, n_sims)
    for (t in 1:n_trials){
      beta = rnorm(alpha, tau)
      if(length(beta) == 4)
        p_rep <- invlogit (beta[1] + beta[2]*n_avoid_rep + beta[3]*n_shock_rep + beta[4]*n_trials_since_shock)
      else
        p_rep <- invlogit (beta[1] + beta[2]*n_avoid_rep + beta[3]*n_shock_rep)
      y_rep[,j,t] <- rbinom (n_sims, 1, p_rep)
      n_avoid_rep <- n_avoid_rep + 1 - y_rep[,j,t] 
      n_shock_rep <- n_shock_rep + y_rep[,j,t]
      for(s in 1:n_sims){
        if (y_rep[s,j,t] == 0) {
          n_trials_since_shock[s] <- n_trials_since_shock[s] + 1;
        }
        else {
          n_trials_since_shock[s] <- 0;
        }
      }
    }
  }
  y_rep
}

y_rep <- rep_data(n_sims, n_dogs, n_trials, beta)

get_means_df <- function(y, y_rep) {
  means <- data.frame(colMeans(y), row.names = 1:25)
  colnames(means) <- "data"
  for (i in 1:50)
  {
    means[, toString(i)] <- colMeans(y_rep[i,,])
  }
  means <- gather(means, sim, percent)
  means$trial <- rep(1:25, 51)
  means
}

means <- function(y, y_rep)

plot <- ggplot(means, aes(x = trial, y = percent, group=sim, color = "red", alpha = .1)) + geom_line() + 
  xlab('Trial') + ylab('Percent Jump') +
  labs(title = "Percent of dogs jumping by trial") +
  geom_line(data=subset(means, sim == "data"), alpha = 1, color = "black", size=1.5)
plot + guides(colour = FALSE) + guides(alpha = FALSE)

## Direct comparison of simulated to real data
sort <- function (y){
  n_dogs <- nrow(y)
  n_trials <- ncol(y)
  last_shock <- rep (NA, n_dogs)
  for (j in 1:n_dogs){
    last_shock[j] <- max ((1:n_trials)[y[j,]==1])
  }
  y[order(last_shock),]
}