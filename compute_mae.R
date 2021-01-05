predicted <- read.csv(file="B:/diploma/Proiect_disertatie/results/electrica_200_168/cc_results_200.csv", header=TRUE, sep=",")
original <- read.table("B:/diploma/Proiect_disertatie/set_date_2/electrica/cc.txt", header = FALSE)
file1 <- "cc"
sum <- 0
length1 <-200
predicted_2 <- vector()
real <- vector()
k<-1
mae <-0

compute_mae <- function(operation){
  while (k < length(predicted$x))
  {
    if(predicted$x[k] < 0)
    {
      predicted_2[k] <- 0
      
    }
    else
    {
      predicted_2[k] <- (predicted$x[k])
    }
    real[k] <- original$V1[k+length1]
    
    if(operation == "ln"){
      
      diff <- abs(exp(predicted_2[k]) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    else if(operation == "log10"){
      
      diff <- abs((10 ^ predicted_2[k]) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    else if(operation == "sqrt"){
      
      diff <- abs((predicted_2[k] ^ 2) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    else if(operation == "electrica"){
      
      diff <- abs((predicted_2[k]) - real[k])
      file <- paste(file1,operation,sep = "_")
    }   
    else if(operation == "electrica_sqrt"){
      
      diff <- abs((predicted_2[k]^2) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    else if(operation == "electrica_ln"){
      
      diff <- abs(exp(predicted_2[k]) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    else if(operation == "electrica_lg"){
      
      diff <- abs((10 ^ predicted_2[k]) - real[k])
      file <- paste(file1,operation,sep = "_")
    }
    sum <-sum + diff
    k<-k+1
  }
  mae <- sum/length(predicted$x)
  
  result <- paste("results_mae/electrica_200_168/",file, "_results_200",".txt", sep = "")
  write.table(mae,file=result,row.names = FALSE, col.names = TRUE)
  
}

compute_mae("electrica")