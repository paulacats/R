#this program opens an csv file, locates specific country data and creates a plot with the data

#remove scientific notation
options(scipen = 999)

#install the packages and libraries
install.packages("tidyr")
install.packages("ggplot2")
library("tidyr")
library("ggplot2")
library("scales")
library("reshape2")

#set working directory
#setwd("D:/WorkSpace/CIT137/assignments/ggplotGraph")
setwd("F:/CIT137/assignments/ggplotGraph")
tour <-read.csv("GlobalTourism.csv")

#verify data
head(tour)

#get row values
which(tour$Country.Name =="Haiti") #96
which(tour$Country.Name =="Australia") #12
which(tour$Country.Name =="Tajikistan") #221

#make subset of desired countries
tourism <- tour[c(12,96,221),]

#get rid of the x before the year
names(tourism) <- sub("X", "", names(tourism))

#convert to long
tourismLong <- gather(tourism, Country.Name, Year)

#verify data
head(tourismLong)

#Change the name of the variable values
names(tourismLong)[3]<-"Value"
names(tourismLong)[2]<-"Year"
names(tourismLong)[1] <-"Country"
head(tourismLong)

#melt the data, to remove empty year values
tourismLongMelt <- melt(tourismLong, id.vars = "Year")
tourismLongMelt <- na.omit(tourismLong)

#Build the plot
myplot <- ggplot(data=tourismLongMelt, aes(x=Year, y=Value, group = Country, color = Country))+geom_line()

#add points to indicate the missing years
myplot <- myplot + geom_point()

#add title and style
myplot <- myplot+ggtitle("Global Tourism")
myplot <- myplot + theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), axis.text.x = element_text(color = "white"), axis.text.y = element_text(color = "white"), plot.background=element_rect(fill="darkgray"))

#adjust the scale
myplot <- myplot + scale_y_log10(labels = dollar)

#display the plot
myplot


