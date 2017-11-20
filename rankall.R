rankall <- function(routcome, num = "best") {
  library(dplyr)
  ## read outcome data
  foutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- sort(unique(foutcome[, 7]))
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  rvalues <- c("best","worst")
  ## Check state and outcome are valid
  if ((routcome %in% valid_outcomes) == FALSE) {
      stop("invalid outcome")
  }
  
  if(num > nrow(foutcome) & (num %in% rvalues) == FALSE ) {
      #print("bad value in num")
      return(NA)
  }
  if(num == "best") {
      num <- 1
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  results <- data.frame(hospital=character(),state=character(),stringsAsFactors = FALSE)
  
  
  if(routcome == "heart attack") {
    for(rstate in states) {
      required_state <- filter(foutcome, State == rstate)
      #print(rstate)
      fstate_data <- required_state[,c(2,11)]
        tst1 <- fstate_data[order(suppressWarnings(as.numeric(fstate_data[,2])),fstate_data[,1],na.last = NA),]
        if(num == "worst") {
          hname <- tst1$Hospital.Name[nrow(tst1)]
          
        } else if(num > nrow(tst1)) {
          #print("setting to NA")
          hname = "<NA>"
        } else {
          hname <- tst1$Hospital.Name[num]
        }
        newrow <- data.frame(hname,rstate)
        results <- rbind(results,newrow)
        # add to data frame
    }
    colnames(results) <- c("hospital", "state")
    return(results)
  }
 
  if(routcome == "heart failure") {
    for(rstate in states) {
      required_state <- filter(foutcome, State == rstate)
      #print(rstate)
      fstate_data <- required_state[,c(2,17)]
      tst1 <- fstate_data[order(suppressWarnings(as.numeric(fstate_data[,2])),fstate_data[,1],na.last = NA),]
      if(num == "worst") {
        hname <- tst1$Hospital.Name[nrow(tst1)]
        
      } else if(num > nrow(tst1)) {
        #print("setting to NA")
        hname = "<NA>"
      } else {
        hname <- tst1$Hospital.Name[num]
      }
      newrow <- data.frame(hname,rstate)
      results <- rbind(results,newrow)
      
    }
    colnames(results) <- c("hospital", "state")
    return(results)
  }
  
  if(routcome == "pneumonia") {
    for(rstate in states) {
      required_state <- filter(foutcome, State == rstate)
      #print(rstate)
      fstate_data <- required_state[,c(2,23)]
      tst1 <- fstate_data[order(suppressWarnings(as.numeric(fstate_data[,2])),fstate_data[,1],na.last = NA),]
      hname <- tst1$Hospital.Name[num]
      if(num == "worst") {
        hname <- tst1$Hospital.Name[nrow(tst1)]
        
      } else if(num > nrow(tst1)) {
        #print("setting to NA")
        hname = "<NA>"
      } else {
        hname <- tst1$Hospital.Name[num]
      }
      newrow <- data.frame(hname,rstate)
      results <- rbind(results,newrow)
    
      # add to data frame
    }
    colnames(results) <- c("hospital", "state")
    return(results)
  }
  
  
}