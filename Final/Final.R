setwd("~/Documents/BDA/Final")
library("foreign")
library("rstan")
library("dplyr")
library("boot")
library("ggplot2")
library("tidyr")

#2
n <- 100
a <- 2
b <- .3
gen_fake_data <- function(n, a, b){
  x <- exp(rnorm(n))
  y <- rep(NA, n)
  for(i in 1:n)
    y[i] <- rgamma(1, a, rate = b * x[i])
  return (list(x = x, y = y))
}

log_p_th <- function(th, x, y){
  a <- th[1]
  b <- th[2]
  log_a_prior <- dlnorm(a, meanlog = log(5), sdlog = log(2), log = TRUE)
  log_b_prior <- dlnorm(b, meanlog = log(0.1), sdlog = log(10), log = TRUE)
  log_lik <- rep(NA, length(x))
  for(i in 1:n)
    log_lik[i] <- dgamma(y[i], a, rate = b * x[i], log = TRUE)
  log_likelihood <- sum(log_lik)
  return (log_likelihood + log_a_prior + log_b_prior)
}

log_p_th2 <- function(th, x, y){
  a <- th[1]
  b <- th[2]
  log_a_prior <- - log(sqrt(2 * pi) * log(2)) - log(a) - (1/(2 * log(2)^2) * (log(a) - log(5))^2) 
  log_b_prior <- - log(sqrt(2 * pi) * log(10)) - log(b) - (1/(2 * log(10)^2) * (log(b) - log(0.1))^2)
  log_lik <- rep(NA, length(x))
  for(i in 1:n)
    log_lik[i] <- a * log(b * x[i]) - log(gamma(a)) + (a - 1) * log(y[i]) - b * x[i] * y[i]
  log_likelihood <- sum(log_lik)
  return (log_likelihood + log_a_prior + log_b_prior)
}

gradient_th <- function(th, x, y){
  a <- th[1]
  b <- th[2]
  d_a_lik <- rep(NA, n)
  d_b_lik <- rep(NA, n)
  for(i in 1:n){
    d_a_lik[i] <- log(b * x[i]) - digamma(a) + log(y[i])
    d_b_lik[i] <- (a / b) - (y[i] * x[i])
  }
  d_a_prior <-  1/(2 * log(2) ^ 2) * (2 * log(a/5) / a) + (1 / a)
  d_a <- sum(d_a_lik) - d_a_prior
  d_b_prior <- 1/(2 * log(10) ^ 2) * (2 * log(b/0.1) / b) + (1 / b)
  d_b <- sum(d_b_lik) - d_b_prior
  return (c(d_a, d_b))
}

gradient_th_numerical <- function(th, x, y){
  d <- length(th)
  e <- .0001
  diff <- rep(NA, d)
  for (k in 1:d){
    th_hi <- th
    th_lo <- th
    th_hi[k] <- th[k] + e
    th_lo[k] <- th[k] - e
    diff[k] <- (log_p_th(th_hi, x, y) - log_p_th(th_lo, x, y))/(2 * e)
  }
  return (diff)
}

gradient_th_numerical2 <- function(th, x, y){
  d <- length(th)
  e <- .0001
  diff <- rep(NA, d)
  for (k in 1:d){
    th_hi <- th
    th_lo <- th
    th_hi[k] <- th[k] + e
    th_lo[k] <- th[k] - e
    diff[k] <- (log_p_th2(th_hi, x, y) - log_p_th2(th_lo, x, y))/(2 * e)
  }
  return (diff)
}

fround <- function (x, digits) {
  format (round (x, digits), nsmall=digits)
}

hmc_iteration <- function(th, x, y, epsilon, L, M) {
  M_inv <- 1/M
  d <- length(th)
  phi <- rnorm (d, 0, sqrt(M))
  th_old <- th
  log_p_old <- log_p_th (th, x, y) - 0.5 * sum(M_inv * phi ^ 2)
  phi <- phi + 0.5 * epsilon * gradient_th(th, x, y)
  for (l in 1:L) {
    th <- th + epsilon * M_inv * phi
    phi <- phi + (if (l == L) 0.5 else 1) * epsilon * gradient_th(th, x, y)
  }
  phi <- -phi
  log_p_star <- log_p_th(th, x, y) - 0.5 * sum(M_inv * phi ^ 2)
  r <- exp(log_p_star - log_p_old)
  if(is.nan(r)) r <- 0
  p_jump <- min(r, 1)
  th_new <- if (runif(1) < p_jump) th else th_old
  return (list (th = th_new, p_jump = p_jump))
}

hmc_run <- function(starting_values, iter, epsilon_0, L_0, M) {
  chains <- nrow(starting_values)
  d <- ncol (starting_values)
  sims <- array(NA, c(iter, chains, d), dimnames = list(NULL, NULL, colnames(starting_values)))
  warmup <- 0.5 * iter
  p_jump <- array(NA, c(iter, chains))
  for (j in 1:chains) {
    th <- starting_values[j,]
    for (t in 1:iter) {
      epsilon <- runif(1, 0, 2 * epsilon_0)
      L <- ceiling(2 * L_0 * runif(1))
      temp <- hmc_iteration(th, x, y, epsilon, L, M)
      p_jump[t, j] <- temp$p_jump
      sims[t, j, ] <- temp$th
      th <- temp$th
    }
  }
  print(cat("Avg acceptance probs:", fround(colMeans(p_jump[(warmup + 1):iter,]), 2), "\n"))
  return(monitor(sims, warmup, print=FALSE))
}

data <- gen_fake_data(100, 2, .3)
x <- data$x
y <- data$y

parameter_names <- c("a", "b")
d <- length(parameter_names)
chains <- 4
mass_vector <- c(1 / (log(2) ^ 2), 1 / (log(10) ^ 2))
starts <- array(NA, c(chains, d), dimnames = list(NULL, parameter_names))
for (j in 1:chains) {
  starts[j,] <- c(rlnorm(1, meanlog = log(5), sdlog = log(2)), rlnorm(1, meanlog = log(0.1), sdlog = log(10)))
}

M1 <- hmc_run(starting_values = starts, iter = 250, epsilon_0 = .05, L_0 = 20, M = mass_vector)

#3
sigma = 30
sim_mse <- function(sigma, n) {
  theta_0 <- runif(n)
  mse_mle <- rep(NA, n)
  mse_post <- rep(NA, n)
  theta <- seq(from = 0, to = 1, length.out = 1000)
  for(i in 1:n) {
    y <- rnorm(1, mean = theta_0[i], sd = sigma)
    mle <- max(min(y, 1), 0)
    norm <- sum(dnorm(y, mean = theta, sd = sigma))
    post_mean <- sum(theta * (dnorm(y, mean = theta, sd = sigma)/norm))
    mse_mle[i] <- (mle - theta_0[i])^2
    mse_post[i] <- (post_mean - theta_0[i])^2
  }
  list(mse_mle = mse_mle, mse_post = mse_post)
}
count <- 0
for(i in 1:20){
  sims <- sim_mse(sigma, 100)
  if(mean(sims$mse_post) > mean(sims$mse_mle))
    count <- count + 1
}

#4
data <- read.dta("pew_research_center_june_elect_wknd_data.dta")
data$heat <- data$heat2
data$heat[is.na(data$heat2)] <- data$heat4[is.na(data$heat2)]
levels(data$heat)[3] <- NA
data$dem <- as.numeric(data$heat) - 1
data$married <- data$marital == "married"
state_abbs <- state.abb
state_abbs <- append(state_abbs, "DC", after = 7)
data$state_abbs <- data$state
levels(data$state_abbs) <- state_abbs
ok <- !is.na(data$dem) & !is.na(data$married) & !is.na(data$state) & data$state != "alaska" & data$state != "hawaii"
clean_data <- droplevels(subset(data, ok))
by_state_married <- group_by(clean_data, state, married, state_abbs)
counts_data <- summarise(by_state_married, count = n(), n_dem = sum(dem), percent = mean(dem)*100)
n_states <- nlevels(counts_data$state)
state <- as.numeric(counts_data$state)
married <- as.numeric(counts_data$married)
n_dem <- counts_data$n_dem
n_data <- dim(counts_data)[1]
count <- counts_data$count
pct_married <- rep(NA, n_states)
for(i in 1:n_states){
  pct_married[i] <- count[state==i & married==1]/(count[state==i & married==1]+count[state==i & married==0])
}

fit <- stan("Final.stan")
monitor <- as.data.frame(monitor(fit))
y_pred <- data.frame(state_abbs = counts_data$state_abbs, married = counts_data$married)
p_mean <- rep(NA, length(state))
p_sd <- rep(NA, length(state))
for(i in 1:length(state)){
  p_mean[i] <- monitor[paste("p_pred[", i, "]", sep = ""), "mean"]
  p_sd[i] <- monitor[paste("p_pred[", i, "]", sep = ""), "sd"]
}
y_pred$percent <- p_mean
y_pred$sd <- p_sd
y_pred$ground_truth <- rep(FALSE, length(y_pred$percent))
plot_data <- y_pred
plot_data$sd <- rep(NA, length(plot_data$sd))
plot_data$percent <- counts_data$percent
plot_data$ground_truth <- rep(TRUE, length(plot_data$percent))
plot_data <- rbind(y_pred, plot_data)

p <- ggplot(data=plot_data, aes(x=state_abbs, y=percent, group=interaction(married, ground_truth), color=married, shape=ground_truth, ymax=percent + sd, ymin=percent - sd))
p + geom_point() +
  geom_errorbar(width=0.2) +
  xlab("State") + 
  ylab("Percent") +
  ggtitle("") +
  theme_bw()

results <- read.csv("~/Documents/BDA/Final/2008ElectionResult.csv")
results$state_abbs <- state_abbs
ok <- results$state != "Alaska" & results$state != "Hawaii"
clean_results<- droplevels(subset(results, ok))
obama_plot <- clean_results[, c("state_abbs", "vote_Obama_pct")]
obama_plot$sd <- rep(NA, length(obama_plot$state_abbs))
obama_plot$ground_truth <- rep(TRUE, length(obama_plot$state_abbs))
pred_mean <- rep(NA, length(obama_plot$state_abbs))
pred_sd <- rep(NA, length(obama_plot$state_abbs))
for(i in 1:length(clean_results$state_abbs)){
  pred_mean[i] <- monitor[paste("obama_pct[", i, "]", sep = ""), "mean"]
  pred_sd[i] <- monitor[paste("obama_pct[", i, "]", sep = ""), "sd"]
}
obama_preds <- data.frame(state_abbs=obama_plot$state_abbs, vote_Obama_pct=pred_mean, sd=pred_sd)
obama_preds$ground_truth <- rep(FALSE, length(obama_preds$state_abbs))
obama_plot <- rbind(obama_plot, obama_preds)

p <- ggplot(data=obama_plot, aes(x=state_abbs, y=vote_Obama_pct, group=ground_truth, color=ground_truth, ymax=vote_Obama_pct + sd, ymin=vote_Obama_pct - sd))
p + geom_point() +
  geom_errorbar(width=0.2) +
  xlab("State") + 
  ylab("Percent") +
  ggtitle("") +
  theme_bw()

fit_nostate <- stan("final_nostate.stan")
monitor_nostate <- as.data.frame(monitor(fit_nostate))
y_pred <- data.frame(state_abbs = counts_data$state_abbs, married = counts_data$married)
p_mean <- rep(NA, length(state))
p_sd <- rep(NA, length(state))
for(i in 1:length(state)){
  p_mean[i] <- monitor_nostate[paste("p_pred[", i, "]", sep = ""), "mean"]
  p_sd[i] <- monitor_nostate[paste("p_pred[", i, "]", sep = ""), "sd"]
}
y_pred$percent <- p_mean
y_pred$sd <- p_sd
y_pred$ground_truth <- rep(FALSE, length(y_pred$percent))
plot_data <- y_pred
plot_data$sd <- rep(NA, length(plot_data$sd))
plot_data$percent <- counts_data$percent
plot_data$ground_truth <- rep(TRUE, length(plot_data$percent))
plot_data <- rbind(y_pred, plot_data)

p <- ggplot(data=plot_data, aes(x=state_abbs, y=percent, group=interaction(married, ground_truth), color=married, shape=ground_truth, ymax=percent + sd, ymin=percent - sd))
p + geom_point() +
  geom_errorbar(width=0.2) +
  xlab("State") + 
  ylab("Percent") +
  ggtitle("") +
  theme_bw()

obama_plot <- clean_results[, c("state_abbs", "vote_Obama_pct")]
obama_plot$sd <- rep(NA, length(obama_plot$state_abbs))
obama_plot$ground_truth <- rep(TRUE, length(obama_plot$state_abbs))
pred_mean <- rep(NA, length(obama_plot$state_abbs))
pred_sd <- rep(NA, length(obama_plot$state_abbs))
for(i in 1:length(clean_results$state_abbs)){
  pred_mean[i] <- monitor_nostate[paste("obama_pct[", i, "]", sep = ""), "mean"]
  pred_sd[i] <- monitor_nostate[paste("obama_pct[", i, "]", sep = ""), "sd"]
}
obama_preds <- data.frame(state_abbs=obama_plot$state_abbs, vote_Obama_pct=pred_mean, sd=pred_sd)
obama_preds$ground_truth <- rep(FALSE, length(obama_preds$state_abbs))
obama_plot <- rbind(obama_plot, obama_preds)

p <- ggplot(data=obama_plot, aes(x=state_abbs, y=vote_Obama_pct, group=ground_truth, color=ground_truth, ymax=vote_Obama_pct + sd, ymin=vote_Obama_pct - sd))
p + geom_point() +
  geom_errorbar(width=0.2) +
  xlab("State") + 
  ylab("Percent") +
  ggtitle("") +
  theme_bw()