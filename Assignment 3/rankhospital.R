rankhospital <- function(state,outcome,num = "best"){
     outcomes <- c("heart attack","heart failure","pneumonia")
     hospital <- read.csv("Assignment3//outcome-of-care-measures.csv", colClasses = "character")
     if(!(state %in% hospital[,7])) stop("invalid state")
     if(!(outcome %in% outcomes)) stop("invalid outcome")
               
     if(outcome == "heart attack") col <- 11
     else if(outcome == "heart failure") col <-17
     else col <- 23
     hospital[,col] <- as.numeric(hospital[,col])
     
     split_data <- split(hospital,hospital[,7])
     Dt <- split_data[[state]]
     L1 <- Dt[,col]
     L2 <- Dt[,2]
     ranking <- order(L1,L2)
     sorted <- Dt[ranking,2]
     
     if(num == "best") num <- 1
     else if (num == "worst") num <- sum(complete.cases(Dt[ranking,col]))
     else num <- as.numeric(num)
     if(table(hospital[,7])[state] < num ) return(NA)
     
     sorted[num]     
}