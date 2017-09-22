#This script removes the first record for each location in a set of data.

#Libraries:
library(dplyr)

#####################################################Begin

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data
WaterTables = read.csv("Munge1_InputsTable.csv")

#Get unique IDs for Wells
id = distinct(WaterTables, SiteNo)
listWells = c(id$SiteNo)
listWells = sort(listWells, decreasing = FALSE)

#Create a data frame based on the input data
startI = listWells[1]
df<-WaterTables[WaterTables$SiteNo==startI,]
df<-data.frame(df[1:dim(df)[1],]) #does NOT skip the first year

#Add the rest of the data
for(i in 2:length(listWells)){ #first well was used to make starting data frame
  myI = listWells[i]
  
  myWell<-WaterTables[WaterTables$SiteNo==myI,]
  myWell<-myWell[order(myWell$WaterYear),]
  myWell<-myWell[1:dim(myWell)[1],] #does NOT skip the first year
  
  #Add to data frame
  idata<-data.frame(myWell)
  df<-rbind(df,idata)
}

#save
write.csv(df,file="Munge2_InputsTable.csv")
