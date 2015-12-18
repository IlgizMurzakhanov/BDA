library("rstan")
library("boot")
library("ggplot2")
setwd("~/Documents/BDA/Homework 12")
data <- read.csv("choices.csv", stringsAsFactors = FALSE)

# Drop rows with missing responses
data <- subset(data, !is.na(LaterOptionChosen))

condition.number = 0
# Select the subset of data for this condition, where 0 is pooled over all.
if (condition.number != 0) {
  data <- subset(data, Condition == condition.number)
} else {
  data <- data
}

# Add additional features required by ITCH model
data <- transform(data, XStar=(X1 + X2) / 2)
data <- transform(data, TStar=(T1 + T2) / 2)
data <- transform(data, G=scale(X2 - XStar))
data <- transform(data, R=scale((X2 - X1) / XStar))
data <- transform(data, D=scale(T2 - TStar))
data <- transform(data, T=scale((T2 - T1) / TStar))

# Add additional features required by DRIFT model
data <- transform(data, DriftD=scale(X2 - X1))
data <- transform(data, DriftR=scale((X2 - X1) / X1))
data <- transform(data, DriftI=scale((X2 / X1)^(1 / (T2 - T1)) - 1))
data <- transform(data, DriftT=scale(T2 - T1))

# Rescale data when working with "raw" numbers
data <- transform(data, X1=X1 / max(X2))
data <- transform(data, X2=X2 / max(X2))

it_data <- data
#it_data <- data[data$Subject %in% 1:50,]

G <- drop(it_data$G)
R <- drop(it_data$R)
D <- drop(it_data$D)
T <- it_data$T
LaterOptionChosen <- it_data$LaterOptionChosen
Subject <- as.numeric(as.factor(it_data$Subject))
Condition <- it_data$Condition
n_subjects <- nlevels(as.factor(Subject))
n_data <- nrow(it_data)
conds <- rep(NA, n_subjects)
for(d in 1:n_data) {
  conds[Subject[d]] <- Condition[d]
}
n_conds <- nlevels(as.factor(conds))
X1 <- it_data$X1
X2 <- it_data$X2
T1 <- it_data$T1
T2 <- it_data$T2
epsilon <- 0.01

fit_itch <- stan("ITCH.stan")
m_itch <- as.data.frame(monitor(fit_itch))
fit_exp <- stan("exponential.stan")
#fit_exp <- stan("exponential.stan", control = list(adapt_delta = 0.87))
m_exp <- as.data.frame(monitor(fit_exp))
fit_hyp <- stan("hyperbolic.stan")
m_hyp <- as.data.frame(monitor(fit_hyp))

log_lik_itch <- extract_log_lik(fit_itch)
log_lik_exp <- extract_log_lik(fit_exp)
log_lik_hyp <- extract_log_lik(fit_hyp)
loo_exp <- loo(log_lik_exp)
loo_hyp <- loo(log_lik_hyp)
loo_itch <- loo(log_lik_itch)
waic_exp <- waic(log_lik_exp)
waic_hyp <- waic(log_lik_hyp)
waic_itch <- waic(log_lik_itch)

df <- data.frame(
  waics <- c(waic_exp$elpd_waic, waic_hyp$elpd_waic, waic_itch$elpd_waic),
  se_waics <- c(waic_exp$se_elpd_waic, waic_hyp$se_elpd_waic, waic_itch$se_elpd_waic),
  models <- c("Exponential", "Hyperbolic", "ITCH")
)

limits <- aes(ymax = waics + se_waics, ymin=waics - se_waics)
p <- ggplot(df, aes(x=models, y=waics))
p + geom_point() + 
  geom_errorbar(limits, width=0.2) + 
  xlab("Model") + 
  ylab("WAIC") +
  ggtitle("") +
  theme_bw()

mu <- m_itch[0:5, c("mean")]
sigma <- m_itch[6:10, c("mean")]
mle_params <- itch_glm$coefficients
limits <- aes(ymax=bayes_params + bayes_sigma, ymin=bayes_params - bayes_sigma)
df <- data.frame(
  mle_params <- mle_params,
  bayes_params <- mu,
  bayes_sigma <- sigma,
  params <- c("I", "G", "R", "D", "T")
)
p <- ggplot(df, aes(x=params, y=bayes_params))
p + geom_point() + 
  geom_errorbar(limits, width=0.2) + 
  geom_point(y=mle_params, color="red")
  xlab("Param") + 
  ylab("Value") +
  ggtitle("") +
  theme_bw()
  
mle_params <- exponential.fit.function(it_data, epsilon)
bayes_params <- c(m_exp[1, "mean"], m_exp[3, "mean"])
bayes_sigma <- c(m_exp[2, "mean"], m_exp[4, "mean"])
limits <- aes(ymax=bayes_params + bayes_sigma, ymin=bayes_params - bayes_sigma)
df <- data.frame(
  mle_params <- mle_params,
  bayes_params <- bayes_params,
  bayes_sigma <- bayes_sigma,
  params <- c("a", "$\delta$")
)
p <- ggplot(df, aes(x=params, y=bayes_params))
p + geom_point() + 
  geom_errorbar(limits, width=0.2) + 
  geom_point(y=mle_params, color="red") + 
  xlab("Param") + 
  ylab("Value") +
  ggtitle("") +
  theme_bw()

#Old fits:
glm(LaterOptionChosen ~ G + R + D + T, data=data, family=binomial(link="logit"))
exponential.log.likelihood.function <- function(data, theta, epsilon) {
  a <- theta[1]
  delta <- theta[2]
  
  z <- with(data, X2 * delta^T2 - X1 * delta^T1)
  p <- inv.logit(a * z)
  p <- epsilon * 0.5 + (1 - epsilon) * p
  
  l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
  ll <- sum(log(l))
  
  return(ll)
}

exponential.fit.function <- function(data, epsilon) {
  # a in [0.0001, 500.00]
  # delta in [0.01, 0.99]
  f <- function (theta) {
    exponential.log.likelihood.function(data, theta, epsilon)
  }
  
  results <- optim(
    c(1.0, 0.5),
    f,
    method="L-BFGS-B",
    lower=c(0.0000001, 0.01),
    upper=c(500.00, 0.99),
    control=list(fnscale=-1)
  )
  
  if (results$convergence != 0) {
    warning("Fitting function failed to converge")
  }
  
  return(results$par)
}
