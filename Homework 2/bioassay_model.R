library ("rstan")

bioassay <- read.table("bioassay_data.txt", header=TRUE)
x <- bioassay$x
y <- bioassay$y
n <- bioassay$n
J <- length(x)

fit1 <- stan("bioassay_model.stan")
print(fit1)

## Post-processing

sims1 <- extract(fit1)
plot(sims1$alpha, sims1$beta)
hist(sims1$LD50)

sd_a <- 4
sd_b <- 10
var_a <- sd_a * sd_a
var_b <- sd_b * sd_b
corr <- .5
cov <- corr * sd_a * sd_b
Sigma <- matrix(c(var_a,cov,
                  cov,var_b),nrow=2)
mu <- c(0, 10)

fit2 <- stan("bioassay_model_prior.stan")
print(fit2)

## Post-processing

sims2 <- extract(fit2)
plot(sims2$alpha, sims2$beta)
hist(sims2$LD50)