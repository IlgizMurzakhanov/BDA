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