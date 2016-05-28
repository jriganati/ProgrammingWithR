## This function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the ranking specified in num.
## The function takes two arguments: an outcome name (outcome) and a hospital rank- ing (num). 

rankall <- function(outcome, num = "best") {
     # Create a matrix that contains the column number for each of our allowed outcomes
     # For example, if they ask for heart attack, the results are in column 11
     colsOutcomes <- c(11,17,23)
     okOutcomes <- c("heart attack", "heart failure", "pneumonia")
     matOutcomes <-matrix (colsOutcomes, nrow=3, ncol=1, dimnames = list(okOutcomes, c("colNum")) )

     # initialize an empty data frame for our results
     results <- data.frame("hospital" = character(), "state" = character(), stringsAsFactors = FALSE)
     
     ## Before we bother reading in data, validate our outcome input
     # Is the outcome one of our okOutcomes
     if(sum(row.names(matOutcomes) == outcome) < 1){
          stop("invalid outcome")
     }
     
     # outcome is good, so Read the data file
     myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     # which column has the data we want
     myCol <- matOutcomes[outcome,1]
     
     # We'll be getting results for every state, so we need a loop
     # Interestingly, state.abb gives us AL before AK for some reason, so we'll sort
     for(myState in sort(state.abb)){
          # get the Hospital.Name as well as the data values for the State specified
          # We'll also filter out observations where the data value is "Not Available"
          myVals <- myData[myData$State == myState & myData[myCol] != "Not Available", c(2,myCol)]
          
          # Sort by data (low to high) and then alphabetically by hospital name
          myVals <- myVals[order(as.numeric(myVals[,2]),myVals[,1]),]
          
          # How many rows are there
          maxRows <- dim(myVals)[1]
          
          # determine which row we want and get the hospital name
          if(maxRows > 0 & num == "best"){
               hosp <- myVals[1,1]
          }
          else if(maxRows > 0 & num == "worst"){
               hosp <- myVals[maxRows,1]
          }
          else if(is.numeric(num) & num < maxRows){
               hosp <- myVals[num,1]
          } else {
               hosp <- NA
          }
          
          # add results to the end of the results data frame
          results[nrow(results) + 1,] <- c(hosp,myState)
     } ## end of myState loop
     
     # give the rows the names of the states and then return
     row.names(results)<- results$state
     results

}
### For Testing
# head(rankall("heart attack", 20), 10)
# 
# tail(rankall("pneumonia", "worst"), 3)
# 
# tail(rankall("heart failure"), 10)

