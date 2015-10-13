post <- function (th){
  y <- c(-2, -1, 0, 1.5, 2.5)
  p <- prod (cauchy_lik (y, th))/0.003391051
  p}
cauchy_lik <- function (y, th){
  cl <- (1 + (y - th)^2)^(-1)
  cl}
post <- Vectorize(post)
curve(post, 0, 1, xname = "theta")

d_y <- function (y, th){
  d <- (2 * (y - th))/(1 + (y - th)^2)
  d}
first_deriv <- function(th){
  y <- c(-2, -1, 0, 1.5, 2.5)
  d <- sum (d_y (y, th))
  d}
sd_y <- function (y, th){
  d <- (2 * (y - th)^2 - 2)/((1 + (y - th)^2)^2)
  d}
second_deriv <- function(th){
  y <- c(-2, -1, 0, 1.5, 2.5)
  sd <- sum (sd_y (y, th))
  sd}
curve(dnorm, mean=0.18, sd=0.73, 0, 1, xname = "theta")