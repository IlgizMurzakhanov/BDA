chickens <- read.table("chickens_data.txt", header=TRUE)
hz <- c(chickens$X.Hz, chickens$X.Hz)
N <- c(chickens$S_N, chickens$E_N)
mean <- c(chickens$S_Mean, chickens$E_Mean)
se <- c(chickens$S_SE, chickens$E_SE)
sham <- array(0, length(chickens$X.Hz))
electro <- array(1, length(chickens$X.Hz))
treatment <- c(sham, electro)
J <- length(hz)

hz <- chickens$X.Hz
S_N <- chickens$S_N
E_N <- chickens$E_N
S_mean <- chickens$S_Mean
E_mean <- chickens$E_Mean
S_se <- chickens$S_SE
E_se <- chickens$E_SE
J <- length(hz)

fit1 <- stan("chickens.stan")