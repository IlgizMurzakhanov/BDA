# 1

J <- 10
x <- runif(J)
alpha <- 2 * rt(1, 4)
beta <- rt(1, 4)
rtpois <- function(n, lambda)
  qpois(runif(n, dpois(0, lambda), 1), lambda)
n <- rtpois(J, 5)
inv_logit <- function(x)
  exp(x)/(1 + exp(x))
theta <- inv_logit(alpha + beta * x)
y <- rbinom(J, n, theta)

rejection_sampling <- function(samps, y, x, n)
{  
  s <- 1
  alphas <- numeric(length = 1000)
  betas <- numeric(length = 1000)
  m <- 100000
  save <- FALSE
  max_lik <- 0
  while(save != TRUE)
  {
    while(s <= samps)
    {
      alpha <- 2 * rt(1, 4)
      beta <- rt(1, 4)
      theta <- inv_logit(alpha + beta * x)
      lik <- prod(dbinom(y, n, theta))
      accept_prob <- m * lik
      if(lik > max_lik)
      {
        max_lik <- lik
      }
      if(runif(1) < accept_prob)
      {
        alphas[s] <- alpha
        betas[s] <- beta
        s <- s + 1
      }
    }
    if(m * max_lik > 1)
    {
      m <- 1/ max_lik
    }
    else
    {
      save <- TRUE
    }
  }
  list(alphas, betas)
}
samps <- rejection_sampling(1000, y, x, n)
alphas <- unlist(samps[1])
betas <- unlist(samps[2])
plot(alphas, betas)

log_post <- function(alpha, beta)
{
  theta <- inv_logit(alpha + beta * x)
  (sum(dbinom(y, n, theta, log = TRUE)) + dt(beta, 4, log = TRUE) + dt(alpha/2, 4, log = TRUE))
}

vect_post <- Vectorize(log_post)

nl_post <- function(params)
{
  alpha <- params[1]
  beta <- params[2]
  - log_post(alpha, beta)
}

mode <- nlm(nl_post, c(0, 0), hessian = TRUE)

library(ellipse)
plot(ellipse(mode$hessian, centre = mode$estimate), type = 'l', xlab="alpha", ylab="beta",)
points(alpha, beta)

imp_samps <- rmvt(1000, sigma = mode$hessian, df = 4, delta = mode$estimate)
g_theta <- dmvt(imp_samps, sigma = mode$hessian, df = 4, delta = mode$estimate)
q_theta <- vect_post(imp_samps[, 1], imp_samps[, 2])
imp_weights <- exp(q_theta - g_theta)
exp_alpha <- sum(imp_samps[, 1] * imp_weights)/sum(imp_weights)
exp_beta <- sum(imp_samps[, 2] * imp_weights)/sum(imp_weights)

# 2
log_post <- function(beta, y, X)
{
  log_lik <- sum(dpois(y, exp(X %*% beta), log = TRUE))
  log_prior <- sum(dcauchy(beta, scale = 2.5))
  log_lik + log_prior
}
init_beta <- rcauchy(3, scale = 2.5)
metropolis <- function(init_beta, n_samps, y, X)
{
  samps <- matrix(nrow = n_samps, ncol = length(init_beta))
  beta <- init_beta
  post <- log_post(beta, y, X) 
  for(i in 1:n_samps)
  {
    beta_star <- beta + rnorm(length(beta))
    post_star <- log_post(beta_star, y, X)
    r <- min(exp(post_star - post), 1)
    if(rbinom(1, 1, r))
    {
      beta <- beta_star
      post <- post_star
    }
    samps[i, ] <- beta
  }
  samps
}

x1 <- runif(50)
x2 <- runif(50)
x3 <- runif(50)
X <- as.matrix(cbind(x1, x2, x3))
y <- rpois(50, exp(X %*% true_beta))

