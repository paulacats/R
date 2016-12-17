install.packages("ggplot2")
install.packages("jsonlite")
install.packages("lubridate")

"This function assumes that the values for the start date and end date 
are valid, i.e. they take for format YYYYMM, 
and that the end date is greater than or equal to the start date."



wikiWordPageViews <- function(start, end, word){
  library("ggplot2")
  library("jsonlite")
  library("lubridate")

  theStart = start
  theEnd = end
  theWord = word
  theURL = "http://stats.grok.se/json/en/"
  allViews <-NULL

  while (theEnd >= theStart) 
  {
    myUrl <- paste(theURL,as.character(theEnd),"/", theWord, sep="")
    urlData <- readLines(myUrl, warn="F") 
    urlDataJson<-fromJSON(urlData)
    urlViews <- unlist(urlDataJson$daily_views)
    theViews <-as.data.frame(urlViews)
    allViews = rbind(allViews, theViews)
    theEnd = theEnd - 1
    #check to see if the month is 0, if so, make it 12
    if (theEnd %% 100 ==0) 
      {
        theEnd = theEnd - 88
      }
  }
  #call the plotting function and pass the data to it
  plot_ggsmooth(allViews)

}

plot_ggsmooth <- function(allViews){
  allViews$date <-as.Date(rownames(allViews))
  colnames(allViews)<-c("Views", "Date")
  ggplot(allViews, aes(Date,Views))+geom_line()
}



