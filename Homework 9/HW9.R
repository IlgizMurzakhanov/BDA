samples <- 10000
y_rep <- vector(mode = "numeric", length = samples)
for(s in 1:samples) {
  theta <- rbeta(1, 8, 14)
  nzeros <- 0
  nswitches <- 0
  n <- 0
  prev <- 0
  while(nzeros < 13) {
    samp <- rbinom(1, 1, theta)
    if(samp == 0){
      nzeros <- nzeros + 1
    }
    if(samp != prev & n > 0)
    {
      nswitches <- nswitches + 1
    }
    prev <- samp
    n <- n + 1
  }
  y_rep[s] <- nswitches
}

hist(y_rep)