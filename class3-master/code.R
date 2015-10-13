#data <- read_rdump('data_file.data.R')
#fit <- stan("model_file.stan", data = data)
#print(fit)
#plot(fit)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(width = 160)

data <- read_rdump('normal1.data.R')
fit <- stan("normal.stan", data = data)
print(fit)
plot(fit)

data <- read_rdump('normal2.data.R')
fit <- stan(fit = fit, data = data)
print(fit) #Not enough data or add priors

fit <- stan("compiletime-error1.stan", data = data) #Needed semicolon
fit <- stan("compiletime-error2.stan", data = data) #Need to declare all variables before use

#real x[N, M]; x[1]: real[M]
#matrix[N, M] x; x[1]: vector[M]
#vector[N] x[M]; 
#All same dims

fit <- stan("compiletime-error3.stan", data = data) #Samples from Bernoulli are ints not reals

fit <- stan("runtime-error1.stan", data = read_rdump('runtime-error1.data.R')) #Y data is greater than 1
fit <- stan("runtime-error2.stan", data = read_rdump("runtime-error2.data.R")) #J is missing from data
fit <- stan("runtime-error3.stan", data = read_rdump("runtime-error3.data.R")) #Use sqrt(sigma) instead so not sampling wrong values

data <- read_rdump('normal1.data.R')
fit <- stan("normal2.stan", data = data)