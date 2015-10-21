setwd("~/Documents/BDA/Homework 6")
bioassay <- read.table("bioassay_data.txt", header=TRUE)
x <- bioassay$x
y <- bioassay$y
n <- bioassay$n

inv.logit <- function(x){
  exp(x)/(1+exp(x))
}

log_p_th <- function(th, y, n, x){
  alpha <- th[1]
  beta <- th[2]
  log_prior <- 0
  log_likelihood <- sum(log((inv.logit(alpha + beta * x) ^ y) * (1 - inv.logit(alpha + beta * x)) ^ (n-y)))
  return (log_likelihood + log_prior)
}

log_p_th2 <- function(th, y, n, x){
  alpha <- th[1]
  beta <- th[2]
  log_prior <- 0
  log_likelihood <- sum(y * (alpha + beta * x) - n * log(1 + exp(alpha + beta * x)))
  return (log_likelihood + log_prior)
}

gradient_th <- function(th, y, n, x){
  alpha <- th[1]
  beta <- th[2]
  d_alpha <- sum(y - n * inv.logit(alpha + beta * x))
  d_beta <- sum(x * (y - n * inv.logit(alpha + beta * x)))
  return (c(d_alpha, d_beta))
}

gradient_th_numerical <- function(th, y, n, x){
  d <- length(th)
  e <- .0001
  diff <- rep(NA, d)
  for (k in 1:d){
    th_hi <- th
    th_lo <- th
    th_hi[k] <- th[k] + e
    th_lo[k] <- th[k] - e
    diff[k] <- (log_p_th(th_hi, y, n, x) - log_p_th(th_lo, y, n, x))/(2 * e)
  }
  return (diff)
}

hmc_iteration <- function(th, y, n, x, epsilon, L, M) {
  M_inv <- 1/M
  d <- length(th)
  phi <- rnorm (d, 0, sqrt(M))
  th_old <- th
  log_p_old <- log_p_th (th, y, n, x) - 0.5 * sum(M_inv * phi ^ 2)
  phi <- phi + 0.5 * epsilon * gradient_th(th, y, n, x)
  for (l in 1:L) {
    th <- th + epsilon * M_inv * phi
    phi <- phi + (if (l == L) 0.5 else 1) * epsilon * gradient_th(th, y, n, x)
  }
  phi <- -phi
  log_p_star <- log_p_th(th, y, n, x) - 0.5 * sum(M_inv * phi ^ 2)
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
      temp <- hmc_iteration(th, y, n, x, epsilon, L, M)
      p_jump[t, j] <- temp$p_jump
      sims[t, j, ] <- temp$th
      th <- temp$th
    }
  }
  monitor (sims, warmup)
  cat("Avg acceptance probs:", fround(colMeans(p_jump[(warmup + 1):iter,]), 2), "\n")
  #return (list(sims = sims, p_jump = p_jump))
}

parameter_names <- c("alpha", "beta")
d <- length(parameter_names)
chains <- 4
mass_vector <- rep(1/15 ^ 2, d)
mass_vector <- c(1, 1 / (5.5 ^ 2))
starts <- array(NA, c(chains, d), dimnames = list(NULL, parameter_names))
for (j in 1:chains) {
  starts[j,] <- rnorm(d, 0, 15)
}

hmc_run(starting_values = starts, iter = 100, epsilon_0 = .5, L_0 = 2, M = mass_vector)