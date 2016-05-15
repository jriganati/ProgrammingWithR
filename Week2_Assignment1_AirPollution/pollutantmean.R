pollutantmean <- function(directory, pollutant, id=1:332){
     #
     # directory - char vector of length 1 indicating the location of the CSV files
     # 
     # pollutant - "sulfate" or "nitrate"
     # char vector of length 1 indicating the name of the polluant
     # for which we will calculate the mean; 
     # 
     # 'id' - int vector indicating the monitor ID numbers to be Used.
     # Defaults to 1:332
     # 
     # Returns:
     #      The mean of the pollutant across all monitors in the 'id' vector
     #      (ignoring NA values)
     #
     # Requires:
     #    library(stringi)
 
     # Initialize the data frame that will hold our combined data
     combined <- data.frame(Date=character(),sulfate=numeric(),nitrate=numeric(),ID=integer())

     # Loop through each file and add its records to the 'combined' data frame      
     for(index in seq_along(id)){
          # Build the file name from the directory and ID 
          # The file names are three characters left padded with zeros
          filename <- paste( c(directory,"/",stri_pad_left(id[[index]],width=3,pad="0"),".csv"),collapse="" )
          
          # read the file into a data frame
          df <- read.csv(filename,header=TRUE)
          
          # Add the file's data frame to our combined data frame
          combined <- rbind(combined,df)
     }
     
     # Now get the !NA values of the pollutant
     pollutantGoodVals <- combined[!is.na(combined[pollutant]),pollutant]
     
     # And finally return the mean
     mean(pollutantGoodVals)
}
