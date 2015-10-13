percentiles <- numeric(10000)
for(i in 1:length(percentiles))
{
  samps <- rnorm(714)
  percentiles[i] <- quantile(samps, probs = .975)
}
sd(percentiles)