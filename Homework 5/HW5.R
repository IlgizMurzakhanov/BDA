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
    while(s <= samps)
    {
      alpha <- 2 * rt(1, 4)
      beta <- rt(1, 4)
      theta <- inv_logit(alpha + beta * x)
      accept_prob <- prod(dbinom(y, n, theta))
      if(runif(1) < accept_prob)
      {
        alphas[s] <- alpha
        betas[s] <- alpha
        s <- s + 1
      }
      print(accept_prob)
      print(s)
    }
  }

# 2
log_post <- function(beta, y, X)
{
  log_lik <- sum(dpois(y, exp(X %*% beta), log = TRUE))
  log_prior <- sum(dcauchy(beta, scale = 2.5))
  log_lik + log_prior
}
init_beta <- rcauchy(3, scale = 2.5)
function <- metropolis(init_beta, n_samps, y, X)
{
  samps <- numeric(length = n_samps)
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
    samps[i] <- beta
  }
}

