setwd("~/Documents/BDA/Homework 8")
id <- read.table("incentives_data_clean.txt", skip = 13, header = T)

J <- as.integer(as.factor(id$sid))
N <- id$basen
n <- as.integer(id$r * N)
y <- id$r
V <- y * (1 - y) / N
N_J <- nlevels(as.factor(J))
N_I <- length(N)

I <- id$I
m <- id$m
t <- id$t
f <- id$f
v <- id$v
b <- id$b

dataList <- list(I=I, J=J, N=N, y=y, V=V, N_J=N_J, N_I=N_I, m=m, t=t, f=f, v=v, b=b)
fit_3 <- stan(file="incentives_model3.stan", data=dataList, iter=1000)
fit_4 <- stan(file="incentives_model4.stan", data=dataList, iter=1000)
fit_3b <- stan(file="incentives_model3b.stan", data=dataList, iter=1000)
  
monitor_4 <- as.data.frame(monitor(fit))
monitor_4 <- monitor_4[1:22,]
estimates_4 <- round(select(monitor_4, `50%`) * 100, digits = 2)
hiq_4 <- round((select(monitor_4, `75%`) - select(monitor_4, `25%`))*50, digits = 2)
reg_table_4 <- as.data.frame(cbind(estimates_4, hiq_4))
colnames(reg_table_4) <- c("estimates", "hiq")
reg_table_4$param <- rownames(reg_table_4)

monitor_3b <- as.data.frame(monitor(fit_3b))
monitor_3b <- monitor_3b[1:16,]
estimates_3b <- round(select(monitor_3b, `50%`) * 100, digits = 2)
hiq_3b <- round((select(monitor_3b, `75%`) - select(monitor_3b, `25%`))*50, digits = 2)
reg_table_3b <- as.data.frame(cbind(estimates_3b, hiq_3b))
colnames(reg_table_3b) <- c("estimates", "hiq")
reg_table_3b$param <- rownames(reg_table_3b)

monitor_3 <- as.data.frame(monitor(fit_3))
monitor_3 <- monitor_3[1:14,]
estimates_3 <- round(select(monitor_3, `50%`) * 100, digits = 2)
hiq_3 <- round((select(monitor_3, `75%`) - select(monitor_3, `25%`))*50, digits = 2)
reg_table_3 <- as.data.frame(cbind(estimates_3, hiq_3))
colnames(reg_table_3) <- c("estimates", "hiq")
reg_table_3$param <- rownames(reg_table_3)

reg_table <- merge(reg_table_3, reg_table_3b, by.x = "param", by.y = "param", all = TRUE)
reg_table <- merge(reg_table, reg_table_4, by.x = "param", by.y = "param", all = TRUE)

#Low burden/prepay
x_mode <- -.5
x_burden <- -.5
x_form <- .5
x_timing <- .5

#Low burden/postpay
x_mode <- -.5
x_burden <- -.5
x_form <- .5
x_timing <- -.5

#High burden/prepay
x_mode <- -.5
x_burden <- .5
x_form <- .5
x_timing <- .5

#High burden/postpay
x_mode <- -.5
x_burden <- .5
x_form <- .5
x_timing <- -.5

estimates_3["beta_I",] + estimates_3["beta_Im",] * x_mode + estimates_3["beta_If",] * x_form + estimates_3["beta_Ib",] * x_burden + estimates_3["beta_It",] * x_timing
estimates_3["beta_Iv",] + estimates_3["beta_Ivb",] * x_burden + estimates_3["beta_Ivt",] * x_timing

estimates_3b["beta_I",] + estimates_3b["beta_Im",] * x_mode + estimates_3b["beta_If",] * x_form + estimates_3b["beta_Ib",] * x_burden + estimates_3b["beta_It",] * x_timing
estimates_3b["beta_Iv",] + estimates_3b["beta_Ivb",] * x_burden + estimates_3b["beta_Ivt",] * x_timing

estimates_4["beta_I",] + estimates_4["beta_Im",] * x_mode + estimates_4["beta_If",] * x_form + estimates_4["beta_Ib",] * x_burden + estimates_4["beta_It",] * x_timing + estimates_4["beta_Itb",] * x_timing * x_burden + estimates_4["beta_Itf",] * x_timing * x_form + estimates_4["beta_Itm",] * x_timing * x_mode + estimates_4["beta_Ifb",] * x_form * x_burden
estimates_4["beta_Iv",] + estimates_4["beta_Ivb",] * x_burden + estimates_4["beta_Ivt",] * x_timing + estimates_4["beta_Ivf",] * x_form + estimates_4["beta_Ivm",] * x_mode