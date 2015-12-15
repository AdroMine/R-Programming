# A function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and 
# nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold.
corr <- function(directory, threshold = 0){
  id <- 1:332
  j <- 0
  corr <- vector(mode = "numeric")
  for(i in id){
    path <- paste(directory,"/",sprintf("%03d",i),".csv",sep="")
    sample <- read.table(path,header=T,sep=",")
    complete_cases <- sum(complete.cases(sample))
    if(complete_cases > threshold){
      corr[j+1] <- cor(sample[,2],sample[,3],use = "complete")
      j <- j + 1
    }
  }
  corr
}