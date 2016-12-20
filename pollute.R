#this function calculates statistics for pollution data
pollute_stat<- function(directory, pollutant, id = 1:332){
  
  myData = NULL
  files <-list.files(directory, full.names = TRUE)
  
  #get the subset
  myFiles <-files[c(id)]
  
  #combine all files into a dataset
  for (i in length(myFiles))  {
   myData <- rbind(myData, read.csv(myFiles[i]))
   #print(myData)
  }

  #set the column value
  if (pollutant =="nitrate"){ 
    pollute <- myData$nitrate
    }else{
      pollute <- myData$sulfate
  }

  #calculate the std. dev.
  stdDev <- sd(pollute, na.rm = TRUE)
  #calculate the mean
  myMean <- mean(pollute, na.rm=TRUE)
  
  #create the dataframe
  dataSum <- data.frame(mean_pol = myMean, mean_std = stdDev)
  
  return(dataSum)

}

pollute_stat("specdata", "nitrate", 2:120)
pollute_stat("specdata", "sulfate", 2:120)
pollute_stat("specdata", "nitrate", 135:145)
pollute_stat("specdata", "sulfate", 135:145)
pollute_stat("specdata", "nitrate", 234:300)
pollute_stat("specdata", "sulfate", 234:300)
