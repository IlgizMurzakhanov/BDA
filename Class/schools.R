schools <- read.csv("schools.csv", header=TRUE)
J <- nrow(schools)
y <- schools$estimate
sigma <- schools$sd

library("rstan")

schools_fit <- stan(file="schools.stan",
                    data=c("J","y","sigma"), iter=1000, chains=4)
print(schools_fit)
plot(schools_fit)