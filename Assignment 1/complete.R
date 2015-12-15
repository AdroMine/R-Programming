#program to calculate the number of complete cases for each monitor
complete <- function(directory,id=1:332){
  nobs <- numeric(length(id)) #empty vector to hold number of observations
  j <- 1
  for(i in id) {
    path <- paste(directory,"/",sprintf("%03d",i),".csv",sep="") #creating path for each file
    samp <- read.csv(path,T) 
    nobs[j] <- sum(complete.cases(samp)) #the number of complete cases
    j <- j+1
  }
  d <- data.frame(cbind(id,nobs))
  d
}