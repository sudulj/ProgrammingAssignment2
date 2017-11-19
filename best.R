best <- function(rstate, routcome) {
  library(dplyr)
  ## read outcome data
  foutcome <-
    read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(foutcome[, 7])
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  ## Check state and outcome are valid
  if ((routcome %in% valid_outcomes) == FALSE) {
    stop("invalid outcome")
  }
  if ((rstate %in% states) == FALSE) {
    stop("invalid state")
  }
  required_state <- filter(foutcome, State == rstate)
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  if(routcome == "heart attack") {
      fstate_data <- required_state[,c(2,11)]
      tst1 <- fstate_data[order(as.numeric(fstate_data[,2]),fstate_data[,1]),]
      return(tst1$Hospital.Name[1])
  }
  if(routcome == "heart failure") {
    fstate_data <- required_state[,c(2,17)]
    tst1 <- fstate_data[order(as.numeric(fstate_data[,2]),fstate_data[,1]),]
    return(tst1$Hospital.Name[1])
    #return(tst1)
  }
  if(routcome == "pneumonia") {
    fstate_data <- required_state[,c(2,23)]
    tst1 <- fstate_data[order(as.numeric(fstate_data[,2]),fstate_data[,1]),]
    return(tst1$Hospital.Name[1])
  }
  ## Return hospital name in that state with lowest 30-day death rate
  
}
