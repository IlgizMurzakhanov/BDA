## Set up the data

golf <- read.table("golf.txt", header=TRUE, skip=2)
x <- golf$x
y <- golf$y
n <- golf$n
J <- length(y)
r <- (1.68/2)/12
R <- (4.25/2)/12

## Fit the model

library("rstan")
fit1 <- stan("golf1.stan", iter=100)
print(fit1)

## Post-processing

sims1 <- extract(fit1)

## Posterior mean
print(mean(sims1$sigma))

## Post median
print(median(sims1$sigma))

## Post 50% interval
print(quantile(sims1$sigma, c(.25, .75)))

## Look at sims

library("shinystan")
launch_shinystan(fit1)
