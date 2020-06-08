## Melvin Lopez - Coursera - Programming Assignment 3

rankhospital <- function(instate, outcome, num) { ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate

  # Read Outcome data from working directory where the file and script reside
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  instate<-toupper(instate)
  
  # Create vector of unique state values 
  states <- levels(factor(outcomes[, 7]))
  
  # Check state input is a valid state code and stop if invalid and print error
  if(!instate %in% states) stop("invalid state",call. = FALSE) 
  
  # Create vector of valid outcome values
  outcomecode<-c("heart failure","pneumonia","heart attack")
  
  # Check condition input is a valid value and stop if invalid and print error
  if(!outcome %in% outcomecode) stop("invalid outcome",call. = FALSE)
  
  # Check rank argument is a valid value and stop if invalid and print error
  if(is.character(num)){
    if (num %in% c('best','worst')==FALSE) {
      stop('Invalid Rank')
    }
  }

  # Use column indexing and Hospital_Revised_Flatfile.pdf to identify column numbers 
  # to use only reqiured columns into new dataframe 
  outcomes <- outcomes[c(2,7,11,17,23)]
  
  # Subset data by input state
  stateoutcome<-outcomes[outcomes$State %in% instate,]
  
  # Create mapping of outcomes to columns 
  if (outcome == "heart failure") {outcomecol <- 4
  } else {
    if (outcome == "pneumonia") {
      outcomecol <- 5
    } else {
      outcomecol <-3
    }
  }
  
  # Select data based upon outcome number and convert to numeric value
  stateoutcome[,outcomecol]<-suppressWarnings(as.numeric(stateoutcome[,outcomecol]))
  
  # Subset data on selected outcome and remove na values
  stateoutcome <- stateoutcome[!is.na(stateoutcome[,outcomecol]),]
  
  # Sort stateoutcomes by outcome and hospital 
  stateoutcome <- stateoutcome[order(stateoutcome[, outcomecol], stateoutcome[,1]),]
  
  if(num == "best") { # Convert "best" to an numeric value to get top rank
    num <- 1 
  }
  
  if (num == "worst") { # Convert "worst" to the last row in the stateoutcome dataframe, otherwise its a numeric value
    num <- nrow(stateoutcome) 
  }

  stateoutcome[num,1] ## Return the name of the hospital
}
  
  
  
                                     
                                     