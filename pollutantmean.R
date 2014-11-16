pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
		
		getFileData <- function(monitorID) {
			scale <- 100

			fileName <- paste(10*scale + monitorID,'.csv',sep='')
		 
			fileName <- paste(directory,"\\",substr(fileName,2,nchar(fileName)),sep='')
		 
			fileContent <- read.csv(fileName)
			
##			goodData <- complete.cases(fileContent)
			
##			fileContent[goodData,]
		}
		
		getPollutantData <- function(fileContent) {
			if(pollutant == 'sulfate') {
				fileContent[,2]
			}
			else {
				fileContent[,3]
			}
		}
		
		
		mean(unlist(lapply(lapply(id,getFileData),getPollutantData)),na.rm=TRUE)
}