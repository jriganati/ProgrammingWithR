## This function will read data from the outcome-of-care.csv file and
## returns a character vector with the name of the hospital that has the 
## ranking specified by the num argument for for a specified state and outcome.

rankhospital <- function(state, outcome, num = "best") {
     # Create a matrix that contains the column number for each of our allowed outcomes
     # For example, if they ask for heart attack, the results are in column 11
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
     # We'll also filter out observations where the data value is "Not Available"
     myVals <- myData[myData$State == state & myData[myCol] != "Not Available", c(2,myCol)]
     
     # Sort by data (low to high) and then alphabetically by hospital name
     myVals <- myVals[order(as.numeric(myVals[,2]),myVals[,1]),]
     
     # How many rows are there
     maxRows <- dim(myVals)[1]
     
     # determine which row we want
     if(num == "best"){
          myRet <- myVals[1,1]
     }
     else if(num == "worst"){
          myRet <- myVals[maxRows,1]
     }
     else if(is.numeric(num) & num < maxRows){
          myRet <- myVals[num,1]
     } else {
          myRet <- NA
     }
     
     # output the Hospital Name for the first object
     print(myRet)

}
### For Testing
# rankhospital("TX", "heart failure", "best")
# #[1] "FORT DUNCAN MEDICAL CENTER"
# 
# rankhospital("TX", "heart failure")
# #[1] "FORT DUNCAN MEDICAL CENTER"
# 
# rankhospital("TX", "heart failure", 4)
# #[1] "DETAR HOSPITAL NAVARRO"
# 
# rankhospital("MD", "heart attack", "worst")
# #[1] "HARFORD MEMORIAL HOSPITAL"
# 
# rankhospital("MN", "heart attack", 5000)
# #[1] NA
