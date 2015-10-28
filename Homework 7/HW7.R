setwd("~/Documents/BDA/Homework 7")
naes04 <- read.csv("naes04.csv")
naes04$ageBin <- cut(naes04$age, 16, labels = c(seq(20, 95, 5)))
by_age_gender <- group_by(naes04, ageBin, gender)
pp_naes <- summarise(by_age_gender,
          count = n(),
          gks = mean(as.numeric(gayKnowSomeone) - 1, na.rm = TRUE))
ageBin <- as.numeric(as.character(pp_naes$ageBin))[1:32]
gks <- pp_naes$gks[1:32]
N <- length(gks)
fit <- stan(file="HW7.stan", data=list(ageBin=ageBin,N=N,gks=gks),
            iter=200, chains=3);