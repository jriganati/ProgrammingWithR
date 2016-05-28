## This function will read data from the outcome-of-care.csv file
## and return the hospital name with the lowest 30-day deat rate
## for a specified state and outcome.

best <- function(state, outcome) {
     # Create a matrix that contains the column number for each of our allowed outcomes
     # For example, if they ask for heart attack, the rsults are in column 11
     colsOutcomes <- c(11,17,23)
     okOutcomes <- c("heart attack", "heart failure", "pneumonia")
     matOutcomes <-matrix (colsOutcomes, nrow=3, ncol=1, dimnames = list(okOutcomes, c("colNum")) )

     ## Before we bother reading in data, validate our inputs
     # Is the state in the R list of state abbreviations?
     if(sum(state.abb == state) < 1){
          stop("invalid state")
     }
     
     # Is the outcome one of our okOutcomes
     if(sum(row.names(matOutcomes) == outcome) < 1){
          stop("invalid outcome")
     }
     
     # both inputs are good, so Read the data file
     myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     # which column has the data we want
     myCol <- matOutcomes[outcome,1]
     
     # get the Hospital.Name as well as the data values for the State specified
     myVals <- myData[myData$State == state, c(2,myCol)]
     
     # Sort by data (low to high) and then alphabetically by hospital name
     myVals <- myVals[order(as.numeric(myVals[,2]),myVals[,1]),]
     
     # output the Hospital Name for the first object
     print(myVals[1,1])

}
### For Testing
# best("TX", "heart attack")
# ##[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# 
# best("TX", "heart failure")
# ##[1] "FORT DUNCAN MEDICAL CENTER"
#
# best("MD", "heart attack")
# ##[1] "JOHNS HOPKINS HOSPITAL, THE"
# 
# best("MD", "pneumonia")
# ##[1] "GREATER BALTIMORE MEDICAL CENTER"
# 
# best("BB", "heart attack")
# ##Error in best("BB", "heart attack") : invalid state
# 
# best("NY", "hert attack")
# ##Error in best("NY", "hert attack") : invalid outcome
