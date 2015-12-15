rankall <- function(outcome,num="best"){
     
     hospital <- read.csv("Assignment3//outcome-of-care-measures.csv",colClasses="character")
     switch(substr(outcome,7,7),a={col=11},f={col=17},n={col=23},stop("Invalid Outcome"))
     
     states <- unique(hospital[,7])
     n <- length(states)
     states <- states[order(states)]
     hosp_names <- vector("character")
     hospital[,col] <- as.numeric(hospital[,col])
     split_data <- split(hospital,hospital[,7])
     if(num == "best") num <- 1
     else if(num == "worst") {}
     else num <- as.numeric(num)
     print(num)
     for(i in 1:n){
          Dt <- split_data[[states[i]]]
          L1 <- Dt[,col]
          L2 <- Dt[,2]
          ranking <- order(L1,L2)
          sorted <- Dt[ranking,2]
          if(num == "worst") k <- sum(complete.cases(Dt[ranking,col]))
          else k <- num
          hosp_names[i] <- sorted[k]
     }
     Ranked_List <- cbind.data.frame(hospital=hosp_names,state=states)
     Ranked_List
}