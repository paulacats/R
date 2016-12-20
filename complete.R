#this function calculates the statistics for only complete cases
complete<- function(directory, id = 1:332){
  
  theData = NULL
  totalData = data.frame()
  tempData = NULL
  
  #get the file list
  files <-list.files(directory, full.names = TRUE)

  
  #open each file in dataset
  for (i in id)  {
    tempData <- read.csv(files[i])
    
    #get row count, i.e. number observations
    numRows <- nrow(tempData)
    
    #get incomplete cases
    noSulfate <-length(which(is.na(tempData$sulfate)))
    noNitrate <-length(which(is.na(tempData$nitrate)))
    
    if (noSulfate > noNitrate){
      totalIncompleteCases <- noSulfate
    }else{
      totalIncompleteCases <-noNitrate
    }
    
    #calculate complete cases
    finishedCases <- numRows - totalIncompleteCases
    #calculate percentage
    percentage <-sprintf("%.0f%%", 100*(finishedCases / numRows))
    
   
    #add info to data frame
    theData <- data.frame(Monitor= i, NumObs = finishedCases, NAs = totalIncompleteCases, Complete = percentage)
    
    #add to the final data frame
    totalData <- rbind(totalData,theData)
   
    #reset values
    tempData = NULL
    theData = NULL
    
    
    }
    
  return(totalData)

}

complete("specdata", 2:12)
complete("specdata", 175:223)
complete("specdata", 300:312)
