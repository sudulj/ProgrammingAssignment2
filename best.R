best <- function(state, outcome) {
        ## read outcome data
  foutcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(foutcome[,7])
  valid_outcomes <- c("heart attack","heart failure","pneumonia")
  ## Check state and outcome are valid
  if((outcome %in% valid_outcomes) == FALSE) {
    stop("invalid outcome")
  }
  if((state %in% states) == FALSE) {
    stop("invalid state")
  }
        
        ## Return hospital name in that state with lowest 30-day death rate
  
}
