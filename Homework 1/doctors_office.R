sim_doc_office <- function() {
  patients <- rexp(1, rate = 1/10)
  while(sum(patients) < 540)
  {
    patients <- append(patients, rexp(1, rate = 1/10))
  }
  patients <- patients[-length(patients)]
  num_patients <- length(patients)
  wait_times <- vector(mode = "numeric", length = num_patients)
  doctors <- c(0, 0, 0)
  arrival <- 0
  for(p in 1:num_patients)
  {
    arrival <- arrival + patients[p]
    d <- which.min(doctors)
    wait_times[p] <- max(doctors[d] - arrival, 0)
    doctors[d] <- arrival + wait_times[p] + runif(1, min = 5, max = 20)
  }
  num_patients <- length(patients)
  num_wait <- sum(wait_times > 0)
  mean_wait <- mean(wait_times)
  close <- max(max(doctors) - 540, 0)
  return(list("num_patients" = num_patients, "num_wait" = num_wait, "mean_wait" = mean_wait, "close" = close))
}
results <- sim_doc_office()
cat("Number of patients is: ", results$num_patients)
cat("Number of patients who had to wait is: ", results$num_wait)
cat("Average wait time in minutes: ", results$mean_wait)
cat("Time the doctors office closed: ", results$close, " minutes after 4PM")

samples <- 100
num_patients <- vector(mode = "numeric", length = samples)
num_waits <- vector(mode = "numeric", length = samples)
mean_waits <- vector(mode = "numeric", length = samples)
closes <- vector(mode = "numeric", length = samples)
for(i in 1:samples)
{
  results <- sim_doc_office()
  num_patients[i] <- results$num_patients
  num_waits[i] <- results$num_wait
  mean_waits[i] <- results$mean_wait
  closes[i] <- results$close
}
cat("Median number of patients: ", median(num_patients))
cat("50% interval is: ", c(quantile(num_patients)[2], quantile(num_patients)[4]))
cat("Median number of waits is : ", median(num_waits))
cat("50% interval is: ", c(quantile(num_waits)[2], quantile(num_waits)[4]))
cat("Median average wait time is : ", median(mean_waits))
cat("50% interval is: ", c(quantile(mean_waits)[2], quantile(mean_waits)[4]))
cat("Median closing time: ", median(closes), " minutes after 4PM")
cat("50% interval is: ", c(quantile(closes)[2], quantile(closes)[4]))