#1
#(a)
n <- 10
x <- 1:10
u <- rep(c(0.1,0.5),c(5,5))
alpha <- 1
beta <- 0.3
y <- rpois(10, u * exp(alpha + (beta * x)))
plot(x, y/u)

#(b)
alphas <- numeric(10000)
betas <- numeric(10000)
posts <- numeric(10000)
i <- 1
norm <- 0
for(alpha in seq(0, 5, .05))
{
  for(beta in seq(0, 1, .01))
  {
    alphas[i] <- alpha
    betas[i] <- beta
    posts[i] <- exp(sum(log(dpois(y, u * exp(alpha + (beta * x))))))
    norm <- norm + posts[i]
    i <- i + 1
  }
}
df <- data.frame(alphas, betas, posts/norm)
head(df[order(-posts),])

samp_alphas <- numeric(1000)
samp_betas <- numeric(1000)
post <- data.frame(alphas, betas, posts)
for(draw in 1:1000)
{
  prob = runif(1)
  p = 0
  i = 1
  while(p <= prob)
  {
    p = p + post[i,]$posts
    i = i + 1
  }
  samp_alphas[draw] <- post[i,]$alphas
  samp_betas[draw] <- post[i,]$betas
}

library(numDeriv)
library(pryr)
post <- function(x, y){
  x_data <- 1:10
  u <- rep(c(0.1,0.5),c(5,5))
  sum(log(dpois(y, u * exp(x[1] + (x[2] * x_data)))))
}
alpha = df[order(-posts),][1,]$alphas
beta = df[order(-posts),][1,]$betas
part_post <- partial(post, y=y)
I_theta <- - hessian(part_post, x=c(alpha, beta))
cov <- solve(I_theta)
print(cov)

library(ellipse)
plot(ellipse(cov, centre = c(alpha, beta)), type = 'l', xlab="alpha", ylab="beta",)
points(alpha, beta)