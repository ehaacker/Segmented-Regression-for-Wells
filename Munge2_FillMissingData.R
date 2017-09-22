#
#
#This script fills in missing data in a time series using linear interpolation.

#Library
library(imputeTS)
library(dplyr)
library(zoo)

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data
rawData = read.csv("Munge2_InputsTable.csv")
rawData$DateDummy = as.Date(rawData$DateDummy)

#Get list of unique well IDs
id = distinct(rawData, SiteNo)
listWells = c(id$SiteNo)
listWells = sort(listWells, decreasing = FALSE)

#Create a data frame based on the input data
startI = listWells[1]
df<-data.frame(matrix(,nrow=0, ncol=ncol(rawData)))
colnames(df)=colnames(rawData)

#Loop through wells to fill in time

for(i in (1:length(listWells))){
  #Get the well
  myI = listWells[i]
  myWell<-rawData[rawData$SiteNo==myI,]
  myWell<-myWell[order(myWell$DateDummy),]
  
  #Get all years
  dataLength = length(myWell$DateDummy)
  timeMin = myWell$DateDummy[1]
  timeMax = myWell$DateDummy[dataLength]
  allYears = seq(from=timeMin, to=timeMax, by="year")
  
  #Fill in data frame with NA
  allYearsFrame = data.frame(list(time=allYears))
  colnames(allYearsFrame) = "DateDummy"
  merged = merge(allYearsFrame,myWell, all=T)
  
  #Perform linear interpolation
  merged$WTelev = na.interpolation(merged$WTelev, option="linear")
  
  #Add to final data frame
  merged<-data.frame(merged)
  df<-rbind(df,merged)
}
#Fill in site number and water year fields for interpolated years
df$SiteNo<-na.locf(df$SiteNo)
df$WaterYear<-na.interpolation(df$WaterYear, option="linear")

#save
write.csv(df,file="Wells_2_Interpolated_WT_Elev.csv")
