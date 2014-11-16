corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

		getCorrelation <- function(monitorID) {
			scale <- 100

			fileName <- paste(10*scale + monitorID,'.csv',sep='')
		 
			fileName <- paste(directory,"\\",substr(fileName,2,nchar(fileName)),sep='')
		 
			fileContent <- read.csv(fileName)
			
			goodData <- complete.cases(fileContent)
			
			goodContent <- fileContent[goodData,]

			cor(goodContent[,2], goodContent[,3])
		}
		
		resultFrame <- complete(directory)
		
		match <- resultFrame[,2] > threshold
		
		matchObs <- nrow(resultFrame[match,])
		
		if(matchObs > 0) {
			as.vector(unlist(sapply(resultFrame[match,][,1],getCorrelation)))
		}
		else {
			vector("numeric")
		}
		
}