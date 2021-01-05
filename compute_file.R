original_file <- read.table("B:/diploma/Proiect_disertatie/set_date_2/electrica/cpt.txt", header = FALSE)
file1 <- "cpt"
k<-1
predicted<- vector()

compute_file <- function(operation){
  
  while (k <= length(original_file$V1))
  {
    if(original_file$V1[k] <= 0)
    {
      predicted[k] <- 0
      
    }
    else
    {
      if(operation == "sqrt"){
        predicted[k] <- sqrt(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      
      else if(operation == "ln"){
        predicted[k] <- log(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "log10"){
        predicted[k] <- log10(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "sqrt_4000"){
        if(original_file$V1[k] > 4000){
          original_file$V1[k] <- 4000
        }
        predicted[k] <- sqrt(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "sqrt_3000"){
        if(original_file$V1[k] > 3000){
          original_file$V1[k] <- 3000
        }
        predicted[k] <- sqrt(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "sqrt_2000"){
        if(original_file$V1[k] > 2000){
          original_file$V1[k] <- 2000
        }
        predicted[k] <- sqrt(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "electrica"){
        predicted[k] <- (original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "sqrt_electrica"){
        predicted[k] <- sqrt(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "ln_electrica"){
        predicted[k] <- log(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
      else if(operation == "log10_electrica"){
        predicted[k] <- log10(original_file$V1[k])
        file <- paste(file1,operation,sep = "_")
      }
    }
    k<- k+1
  }
  
  result <- paste("set_date_2/log10_electrica/",file,".txt", sep = "")
  write.table(predicted,file=result,row.names = FALSE, col.names = FALSE)
  
}
compute_file("log10_electrica")