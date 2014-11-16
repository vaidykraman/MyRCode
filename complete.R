complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
		
		getObservations <- function(monitorID) {
			scale <- 100

			fileName <- paste(10*scale + monitorID,'.csv',sep='')
		 
			fileName <- paste(directory,"\\",substr(fileName,2,nchar(fileName)),sep='')
		 
			fileContent <- read.csv(fileName)
			
			goodData <- complete.cases(fileContent)
			
			c(monitorID,nrow(fileContent[goodData,]))
		}
		
	
		resultFrame <- data.frame(t(sapply(id,getObservations)))
		
		colnames(resultFrame) <- c('id','nobs')
		
		resultFrame
}