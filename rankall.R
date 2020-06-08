rankall <- function(outcome, num=1) {
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate
  
  ## Read data
  alloutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Create vector of valid outcome values
  outcomecode<-c('heart attack', 'heart failure', 'pneumonia')
  
  # Check condition input is a valid value and stop if invalid and print error
  if(!outcome %in% outcomecode) stop("invalid outcome",call. = FALSE)
  
  # Sort by hospital names
  alloutcomes <- alloutcomes[order(alloutcomes[2]),]
  
  # Create named vector of columns for lookup
  mortality <- c(11, 17, 23)
  names(mortality) <- outcomecode
  col <- mortality[[outcome]]
  
  # convert all columns to numeric using the numeric vector 
  for(i in mortality) {
    suppressWarnings(alloutcomes[,i] <- as.numeric(as.character(alloutcomes[,i])))
  }
  
  # sort 30-day mortality rates for the outcome specified
  alloutcomes <- alloutcomes[order(alloutcomes[col]),]
  alloutcomes <- alloutcomes[order(alloutcomes$State),]
  
  ## Add loop to go through states and add rank for all hospitals for the outcome 
  ranked <- alloutcomes[,c(2, 7, col)]
  for(i in unique(ranked$State)) {
    for(j in 1:nrow(ranked[ranked$State == i,])) {
      ranked$Rank[ranked$State == i][j] <- j
    }
  }
  # Create new Rate column to hold rankings
  colnames(ranked)[3] <- 'Rate'
  statecount <- length(unique(alloutcomes$State))
  x <- 0
  
  # Find ranking for each state based in input argument
  # including "best", "worst" or an integer
  if(num == 'best') { x <- as.list(rep(1, statecount))
  } else if(num == 'worst') {
    j <- 1
    for(i in unique(ranked$State)) { x[j] <- which.max(ranked$Rate[ranked$State == i])
      j = j + 1
    }
  } else {
    x <- as.list(rep(num, statecount))
  }
  
  # Find all matching hospital ranks by state
  rankedhospital <- ranked[1,]
  j <- 1
  for(i in unique(ranked$State)) {
    
    # If a rank is not found replace with NA value
    
    if(length(ranked[ranked$Rank == x[j] & ranked$State == i,'State']) > 0) {
      rankedhospital[j,] <- ranked[ranked$Rank == x[j] & ranked$State == i,]
    } else {
      rankedhospital[j,] <- c("<NA>", i, "<NA>", "<NA>")
    }
    j <- j + 1
  }
  # Create rownames for output
  rownames(rankedhospital) <- rankedhospital[,2]
  
  # Create column names for output
  colnames(rankedhospital) <- c('hospital', 'state')
  
  # create data frame with result
  rankedhospital <- rankedhospital[,1:2]
  
  # return result
  return(rankedhospital)
}