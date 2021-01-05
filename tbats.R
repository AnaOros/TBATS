library(forecast)
library(doParallel)

file="cpt_log10"

y.msts <- msts(read.csv(file = paste('set_date_2/log10_electrica/', file, '.txt', sep='' ), header = FALSE),seasonal.periods=c(24))
x <- NULL

lengths <- c(2)
no_cores <- detectCores() - 1

print(paste("start computation...",date()))

foreach (current_length = lengths[1:length(lengths)]) %do%{
  print(paste("start computation for length ",current_length," at ",date()))
  k<-1
  end_index <- length(y.msts) - current_length + 1
  print(end_index)
  sum <- 0
  while (k < end_index) {
    
    subset_series <- subset(y.msts, start=k, end=k+current_length-1)
    
    fit <- tbats(subset_series, use.box.cox=NULL, use.parallel=TRUE, num.cores = no_cores,
                 use.trend=NULL, use.damped.trend=NULL, use.arma.errors=TRUE,
                 model=NULL)
    fc <- forecast(fit, h=1)
    forecasted_value <- (fc$mean[1:1])
    
    diff <- abs(y.msts[k+current_length] - forecasted_value)
    sum <- sum + diff
    
    x[k] = forecasted_value
    k <- k+1
  }
  mae <- sum/end_index
  
  filename <-paste(file,"mae_", toString(current_length),".txt", sep = "")
  results <- paste("results/",file, "_results_", toString(current_length),".csv", sep = "")
  write.csv(x,file=results)
  print(paste("end computation for length ",current_length," at ",date()))
  write.table(mae, file =filename, sep = "\t",
              row.names = FALSE)
}

print("Done")
print(paste("Finish computation at ",date()))