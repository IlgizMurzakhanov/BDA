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

post_mode <- nlm(nl_post, c(0, 0), hessian = TRUE)
cov <- solve(post_mode$hessian)

library(ellipse)
plot(ellipse(cov, centre = mode$estimate), type = 'l', xlab="alpha", ylab="beta",)
points(mode$estimate)

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
true_beta <- rcauchy(3, scale = 2.5)
y <- rpois(50, exp(X %*% true_beta))
init_beta <- rcauchy(3, scale = 2.5)
met_samps1 <- metropolis(init_beta, 1000, y, X)
init_beta <- rcauchy(3, scale = 2.5)
met_samps2 <- metropolis(init_beta, 1000, y, X)
init_beta <- rcauchy(3, scale = 2.5)
met_samps3 <- metropolis(init_beta, 1000, y, X)
data.frame(cbind(met_samps1[, 1], met_samps2[, 1], met_samps3[, 1]))
plot(met_samps)

yrange <- range(c(met_samps1[, 1], met_samps2[, 1], met_samps3[, 1]))
plot(met_samps1[, 1], type="l", ylim=yrange, ylab="samples")
lines(met_samps2[, 1], type="l", ylim=yrange)
lines(met_samps3[, 1], type="l", ylim=yrange)

good_samples <- cbind(met_samps1[501:1000,], met_samps2[501:1000,], met_samps3[501:1000,])
colMeans(good_samples)

n = 50
p = 3
fit <- stan("metropolis.stan")
=======
fit1 <- stan("metropolis.stan")
>>>>>>> origin/master
