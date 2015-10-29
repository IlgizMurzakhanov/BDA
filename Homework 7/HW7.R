setwd("~/Documents/BDA/Homework 7")
naes04 <- read.csv("naes04.csv")
naes04$ageBin <- cut(naes04$age, 16, labels = c(seq(20, 95, 5)))
by_age_gender <- group_by(naes04, ageBin, gender)
pp_naes <- summarise(by_age_gender,
          count = n(),
          gks = mean(as.numeric(gayKnowSomeone) - 1, na.rm = TRUE))
pp_naes$sigma_sq <- (pp_naes$gks * (1 - pp_naes$gks))/pp_naes$count
ageBin <- as.numeric(as.character(pp_naes$ageBin))[1:32]
gks <- pp_naes$gks[1:32]
gks <- gks - mean(gks)
sigma_sq <- pp_naes$sigma_sq[1:32]
gender <- as.numeric(pp_naes$gender)[1:32] - 1
N <- length(gks)
fit <- stan(file="gp.stan", data=list(ageBin=ageBin, N=N, gks=gks, gender=gender, sigma_sq=sigma_sq))

ageBin <- (0:500)/5;
N <- length(ageBin);

tau_sq <- 0.19
l_sq <- 1090.32
fit_sim <- stan(file="gp-sim.stan")
fit_sim_ss <- extract(fit_sim, permuted=TRUE)
df <- data.frame(x=ageBin, y_sim=colMeans(fit_sim_ss$gks)+mean(pp_naes$gks[1:32]));
plot <- qplot(x,y_sim, data=df);