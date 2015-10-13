library ("rstan")

## Sebastian's function to run stan with caching of compiled Stan models
stan_run <- function(stanModel, ...) {
  if(class(stanModel) == "stanfit") {
    stanExe <- stanModel
  } else {
    stanModel.rda <- gsub("stan$", "rda", stanModel)
    if(!file.exists(stanModel.rda) || file.info(stanModel.rda)$mtime < file.info(stanModel)$mtime) {
      cat("Model",stanModel,"needs recompilation.\n")
      args <- modifyList(list(...), list(file=stanModel, iter=0, warmup=0, chains=0))
      stanExe <- do.call(stan, args)
      saveRDS(stanExe, file=stanModel.rda)
    } else {
      cat("Loading cached stan model", stanModel, ".\n")
      stanExe = readRDS(stanModel.rda)
    }
  }
  # This bit with the seed is for debugging purposes; once we figure out why Stan is crashing R we can remove it.
  seed <- sample.int(.Machine$integer.max, 1)
  write (seed, file="stan_seed.txt")
  stan(fit=stanExe, seed=seed, ...)
}

putting_data <- read.table ("putting_data.txt", header=TRUE)
x <- as.vector(putting_data[[1]])
n <- as.vector(putting_data[[2]])
y <- as.vector(putting_data[[3]])
I <- nrow(putting_data)
r <- 1.68/24
R <- 4.25/24
data <- c("I","x","n","y","R","r")

fit <- stan_run("putting_model.stan", data=data, chains=4, iter=2000)
print(fit, pars = "sigma")