
#This script creates a set of linear regressions around breakpoints for a continuous set of data.

#Your input table will need: 
# A field with a distinct ID for each entity (management area, well, et cetera)
# A water year field
# A field for water table change (or whatever the variable of interest is)

#Libraries for analysis and plotting: 
library(segmented)
library(ggplot2)
library(reshape)
library(dplyr)
library(broom)

#####################################################Begin essential functionality

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data
WTelev = read.csv("Wells_2_Interpolated_WT_Elev.csv")
WTelev$DateDummy = as.Date(WTelev$DateDummy)

#Min and max years
minYear = 1929
maxYear = 2017
colno = 1 + maxYear - minYear

#Get unique IDs for Wells
id = distinct(WTelev, SiteNo)
listWells = c(id$SiteNo)

#Create empty dataframe for segmented regression parameters
df = data.frame(Intercept=double(),x=double(),U1.x=double(), EstYear=double(), StErr=double(), R2=double(), 
                AdjR2=double(), sigma=double())
namesdf = names(df)

#Get total span of years
yearOrder<-distinct(WTelev, DateDummy)
yearOrder<-yearOrder[order(as.Date(yearOrder$DateDummy)),]
#timeMin = yearOrder$DateDummy[1]
#timeMax = yearOrder$DateDummy[nrow(yearOrder)]
#allYears = seq(from=timeMin, to=timeMax, by="year")
allYears = seq(from=minYear, to=maxYear, by=1)

#Residuals - for GMAs
#resyrs = list()
#for(yr in minYear:maxYear){
#  name = (paste("X",yr))
#  resyrs = c(resyrs,name)
#}

#Set up residuals dataframe
#resdf = data.frame(matrix(nrow=0,ncol=colno))
#colnames(resdf)=resyrs

#Dummy
allYearsFrame = data.frame(list(time=allYears))
colnames(allYearsFrame) = "DateDummy"

#Loop through the Wells
segreg<-function(listWells, e) {
  for(i in 1:length(listWells)){
    result<-tryCatch({
      #this is the "try" part
      myI = listWells[i]
      
      #Get the data for this well
      myWell<-WTelev[WTelev$SiteNo==myI,]
      myWell<-myWell[1:dim(myWell)[1],]

      #Create linear regressions
      x = myWell$WaterYear
      y = myWell$WTelev
      linmod<-lm(y~x)

      #Create segmented regression
      seggo<-segmented(linmod,seg.Z=~x)
      print(paste(i,"found a breakpoint"))
      
      #Data frame stuff - parameters
      idata<-t(data.frame(c(seggo$coefficients,0,0,0,0)))
      print("data frame made")
      
      #seggo$coefficients: Intercept, x, U1.x, psi1.x (psi1.x is always null/zero and is overwritten by EstYear below)
      idata[4]=summary(seggo)$psi[2] #estimated year
      idata[5]=summary(seggo)$psi[3] #standard error
      idata[6]=summary(seggo)$r.squared
      idata[7]=summary(seggo)$adj.r.squared
      idata[8]=summary(seggo)$sigma
      
      idata = data.frame(idata)
      row.names(idata)=myI
      names(idata)=namesdf
      
      #Update dataframe
      df<-rbind(df,idata)
      print(summary(seggo)$psi[2])
      
      #Dataframe stuff - residuals
      #ires = data.frame(summary(seggo)$residuals)
      #ires[2] = myWell$DateDummy
      #colnames(ires)=c("residuals","DateDummy")
      
      #Merge with dataframe of complete dates
      #merged = merge(allYearsFrame,ires, all=T)
      
      #merged = t(merged$residuals)
      #row.names(merged)=myI
      
      #Update dataframe
      #resdf<-rbind(resdf,merged)

    },
    error=function(err) {
      message(paste(i," did not find a breakpoint"))
      message(err)
      return(NA)
    },
    warning=function(war) {
      message(paste(i," caused a warning"))
      return(NA)
    },
    finally={
      print(paste(i," done"))
    }) 
  } #END loop
  colnames(df)<-namesdf
  #colnames(resdf)<-resyrs
  write.csv(df,file=paste("Well_SegmentedRegression_WTelev",e,".csv"))
  #write.csv(resdf,file=paste("Well_SegmentedRegression_Residuals_WTelev",e,".csv"))
} #END segreg

#Run the function - a hundred times for each well to make sure breakpoints are consistent
for(e in 1:100){
  segreg(listWells, e)
}

#######################################End essential functionality

#Plot and save in table
plot(x,y, main=i)
plot(seggo, add=T)

#save plot
plotname = paste(i,"plot.jpg",sep='')
jpeg(plotname)
dev.off()
