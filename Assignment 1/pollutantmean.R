#calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
pollutantmean<- function(directory,pollutant,id=1:332){
  means <- numeric(length(id))
  num_obs <- 0
  index <- 1
  for(i in id) {
    path <- paste(directory,"/",sprintf("%03d",i),".csv",sep="") #all files have 3 digits number padded with zeros if not enough numbers
    sample <- read.table(path,header=T,sep=",")
    sum_sample <- sum(sample[,pollutant],na.rm=T)
    num_obs <- num_obs + sum(complete.cases(sample[,pollutant]))
    means[index] <- sum_sample
    index = index+1
  }
  round(sum(means)/num_obs,digits=3)
}

