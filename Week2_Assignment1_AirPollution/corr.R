corr <- function(directory, threshold = 0){
     require(stringi)
     #
     # directory - char vector of length 1 indicating the location of the CSV files
     # 
     # threshold - a numberic vector of length 1 indicating the number
     # of completely observed observations (on all variables) required to compute 
     # the correlation between nitrate and sulfate; the default is 0
     # 
     # Returns:
     #      a numveric vector of correlations
     
     # Initialize the vector that will hold our correlation results
     corrResults <- vector(mode = "numeric")

     # Loop through the files in the specified directory. 
     # For each, find the number of complete cases.
     # If complete cases is greater than the specified threshold,
     # compute the correlation between sulfate and nitrate, and add the correlation
     # value to the results vector.
     for(filename in list.files(directory)){
          # Build the fullfilenaem from the directory and filename
          fullfilename <- paste( c(directory,"/",filename),collapse="" )
          
          # read the file into a data frame
          df <- read.csv(fullfilename,header=TRUE)
          
          # get the number of complete records
          compcount <- sum(complete.cases(df) == TRUE)
          
          # if count of complete records is greater than threshold
          # correlate and add to the results vector
          if (compcount > threshold) {
               myCorr = cor(df$sulfate, df$nitrate, use="complete.obs")
               corrResults <- c(corrResults, myCorr)
          }
     }

     # return the results vector
     corrResults
}
