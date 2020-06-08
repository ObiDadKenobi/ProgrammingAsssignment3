## Melvin Lopez - Coursera - Programming Assignment 3

best <- function(instate, outcome) { ## Read outcome data
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
  
  # Use column indexing and Hospital_Revised_Flatfile.pdf to identify column numbers 
  # to use only reqiured columns into new dataframe 
  outcomes <- outcomes[c(2,7,11,17,23)]
  
  # Subset data by input state
  outcomes<-outcomes[outcomes$State %in% instate,]
 
  # Create mapping of outcomes to columns 
  if (outcome == "heart failure") {outcomecol <- 4
  } else {
  if (outcome == "pneumonia") {
    outcomecol <- 5
  } else {
    outcomecol <-3
  }
  }

  # Select data based upon outcome number 
   selectedoutcome<-suppressWarnings(as.numeric(outcomes[,outcomecol]))
  
  # Subset data on selected outcome and remove na values
  outcomes <- outcomes[!(is.na(selectedoutcome)), ]
  
  # Subset data on outcome using clean data
  selectedoutcome <- as.numeric(outcomes[, outcomecol])

  # Select the minimum selected outcome as selection criteria
  selectedHospitals <- which(selectedoutcome  == min(selectedoutcome))
  
  # Subset outcomes to only the selected hospitals with the minimum outcome
  selectedHospitals <- outcomes[selectedHospitals, 1]
  
  # Sort the results alphabetically if more than one result
  BestHospital <- sort(selectedHospitals)
  
  # Show the best hospitals on the screen
  print(BestHospital)
  
}


  
  

                                                                  
  
  
  


