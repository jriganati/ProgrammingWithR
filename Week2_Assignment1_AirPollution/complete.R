complete <- function(directory, id=1:332){
     require(stringi)
     #
     # directory - char vector of length 1 indicating the location of the CSV files
     # 
     # 'id' - int vector indicating the monitor ID numbers to be Used.
     # Defaults to 1:332
     # 
     # Returns:
     #      A data frame with two columns
     #         id: the monitor ID (which is also the file name)
     #         nobs: the count of complete records
 
     # Initialize the data frame that will hold our combined data
     completeobs <- data.frame(id=integer(),nobs=integer())

     # Loop through each file, load it into a data frame, 
     # get the number of complete records, and add the info to the combined data frame      
     for(index in seq_along(id)){
          # Build the file name from the directory and ID 
          # The file names are three characters left padded with zeros
          filename <- paste( c(directory,"/",stri_pad_left(id[[index]],width=3,pad="0"),".csv"),collapse="" )
          
          # read the file into a data frame
          df <- read.csv(filename,header=TRUE)
          
          # get the number of compelte records
          obscount <- sum(complete.cases(df) == TRUE)
          
          # Add the file's data frame to our combined data frame
          completeobs <- rbind(completeobs, data.frame(id=id[[index]],nobs=obscount))
     }
     
     # return the combined data frame
     completeobs
}
