#This script takes a group of .csv files, puts them together into one super data frame, and then performs
#operations by well ID. It uses the split-apply-combine technique described by Hadley Wickham (2011).

#The inputs for this script are the .csv outputs from SegReg_loop_Wells_MonteCarlo R scripts: sets of 100
#text files with results from segmented regression runs. 

#The following libraries are required:
library(plyr)
library(moments)

####################################################################################Begin essential functionality
#Set environments

#For water table elevation directly
pathData = "E:/manuscripts/Management_Manuscript/Analysis/Rstuff/MonteCarloSegReg_Outputs"
pathOut = "E:/manuscripts/Management_Manuscript/Analysis/Rstuff/MC_Summary_WTelev.csv"

setwd(pathData)

#Load data
file.names <- dir(pathData, pattern = ".csv")

data <- read.csv(file.names[1], header = TRUE)
names(data)[1] <- "SiteID"
data$happens <- 1

#Define the getmode helper function
getmode <- function(x) names(which.max(table(round(x))))

#Do some preparatory cleanup
for(i in 2:length(file.names)){
  file <- read.csv(file.names[i], header = TRUE, stringsAsFactors = FALSE)
  names(file)[1] <- "SiteID"
  file$happens <- 1
  data <- rbind(data,file)
}

#Get number of times the well had a breakpoint, means of year estimate and adjusted R2
dataOut = ddply(data, .(SiteID), summarize,
                meanYr = mean(EstYear),
                modeYr = get("getmode", ".GlobalEnv")(EstYear),
                meanAdjR2 = mean(AdjR2),
                StDev = sd(EstYear),
                happens = sum(happens),
                meanX = mean(x),
                meanU1X = mean(U1.x),
                deflect = meanU1X - meanX
                )

write.csv(dataOut, pathOut)
