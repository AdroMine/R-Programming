best <- function(state,outcome){
     outcomes <- c("heart attack","heart failure","pneumonia")
     hospital <- read.csv("Assignment3//outcome-of-care-measures.csv", colClasses = "character")
     if(!(state %in% hospital[,7])) stop("invalid state")
     if(!(outcome %in% outcomes)) stop("invalid outcome")
     
     if(outcome == "heart attack") col <- 11
     else if(outcome == "heart failure") col <-17
     else col <- 23
     hospital[,col] <- as.numeric(hospital[,col])
     curr_rate <- 100
     curr_hosp <- vector("character")
     for(i in 1:nrow(hospital)){
          if(state == hospital[i,7] && !is.na(hospital[i,col])){
               if(hospital[i,col] < curr_rate){
                    curr_rate <- hospital[i,col]
                    curr_hosp <- hospital[i,2]
               }
               else if(hospital[i,col] == curr_rate && hospital[i,2]<curr_hosp){
                    curr_hosp <- hospital[i,2]
               }
          }
     }
     curr_hosp
}