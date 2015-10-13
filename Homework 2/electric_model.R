electric_data <- read.table ("electric_data.txt", header=TRUE)
city <- c(electric_data$City, electric_data$City)
grade <- as.integer(c(electric_data$Grade, electric_data$Grade))
pre <- c(electric_data$T_Pre, electric_data$C_Pre)
post <- c(electric_data$T_Post, electric_data$C_Post)
control <- array(0, length(electric_data$C_Pre))
treat <- array(1, length(electric_data$T_Pre))
treatment <- c(treat, control)
N <- length(pre_test)
G <- 4
C <- 2

fit1 <- stan("electric_model.stan")

print(fit, pars = "b", "mu_b")

#Treatment Effect
tot <- (electric_data$T_Post - electric_data$T_Pre)
baseline <- (electric_data$C_Post - electric_data$C_Pre)
electric_data$Treatment_eff <- tot - baseline

#checking grade
grade1 <- subset(electric_data, electric_data$Grade == 1)
grade2 <- subset(electric_data, electric_data$Grade == 2)
grade3 <- subset(electric_data, electric_data$Grade == 3)
grade4 <- subset(electric_data, electric_data$Grade == 4)
boxplot(grade1$Treatment_eff, grade2$Treatment_eff, grade3$Treatment_eff, grade4$Treatment_eff, names = c(1, 2, 3, 4))

fresno <- subset(electric_data, as.numeric(electric_data$City) == 1)
youngstown <- subset(electric_data, as.numeric(electric_data$City) == 2)
boxplot(fresno$Treatment_eff, youngstown$Treatment_eff, names = c("F", "Y"))

#checking replacement
R <- subset(electric_data, as.numeric(electric_data$Replacement) == 1)
S <- subset(electric_data, as.numeric(electric_data$Replacement) == 2)

#Plot Pre-test scores split by R and S
boxplot(R$C_Pre, S$C_Pre, R$T_Pre, S$T_Pre, names = c("R_control", "S_control", "R_treat", "S_treat"))

#Can also regress
replacement <- electric_data$Replacement
city <- electric_data$City
grade <- electric_data$Grade
pre_test <- electric_data$T_Pre
lm(replacement ~ city + grade + pre_test)