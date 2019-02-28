#The purpose of this script is to create a classification and regression tree model
#for the proportion of breakpoints in a given year, as it relates to climate and 
#crop prices. 

#load in appropriate libraries
library(rpart)
library(MASS)
library(cluster)
library(maptree)
library (RODBC)

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

input = read.csv("CART_inputs2.csv")

input$StateCode = "na"
input$StateCode[input$StateID==8]="CO"
input$StateCode[input$StateID==20]="KS"
input$StateCode[input$StateID==31]="NE"
input$StateCode[input$StateID==35]="NM"
input$StateCode[input$StateID==40]="OK"
input$StateCode[input$StateID==46]="SD"
input$StateCode[input$StateID==48]="TX"
input$StateCode[input$StateID==56]="WY"

input$Deflect = 'na'
input$Deflect[input$meanU1X>0] = "POS"
input$Deflect[input$meanU1X<0] = "NEG"

inputNHP = input[input$RegionName=="NHP",]
inputCHP = input[input$RegionName=="CHP",]
inputSHP = input[input$RegionName=="SHP",]

inputBracket = input[input$Bracketeer=="Bracketeer",]
inputBracketNHP = inputBracket[inputBracket$RegionName=="NHP",]
inputBracketCHP = inputBracket[inputBracket$RegionName=="CHP",]
inputBracketSHP = inputBracket[inputBracket$RegionName=="SHP",]

# assign rpart controls
# 'xval' is for number of cross-validations
slm= rpart.control( xval=10)

# CART models
#name of model = rpart ( response variable ~ predictor1 + predictor2 + ... + predictor999, dataset name, various rpart controllers)
#AllHPA#
cartmodel = rpart(Deflect ~ YearsFromGMA + StateCode + RegionName + CornPrice_BeforeAfter + PDSI_BeforeAfter, input, x=TRUE, y=TRUE, control=slm)

# take a look at your trees
# these trees are fully grown and un-pruned
draw.tree(cartmodel, nodeinfo=TRUE)
title("initial exploration")


# use this plot to determine proper pruning.
# tree size is shown across the top of the plot, complexity parameter (cp) along the bottom
# the dotted lne indicates where the 1-SE rule falls
# pruning rules:
#               1) if all tree sizes are below the 1-SE line, use "cp=0.05";
#               2) prune tree to smallest # of splits below the 1-SE line using "best=#" where # is user defined after looking at the cp plot
plotcp(cartmodel)

# prune your trees. Make sure the package 'maptree' has been loaded. Rpart (which is different than rpart) needs maptree.
# had to change ciode from 'prune.Rpart' to 'clip.rpart' b/c I could not get it to run with the old code... darned updates!
pruned_tree = clip.rpart(cartmodel,cp=0.013)

# take a look at your PRUNED trees
draw.tree(pruned_tree, nodeinfo=TRUE)
#title("Avg WL Change w.r.t. State, Irrigated Acreage, Pumping, Growing Season Temp and Precip, Annual Temp and Precip, Mean Hydraulic Conductivity")

# learn about competitor and surrogate splits
# competitors = splitting options that are very close in cp to the primary split
# surrogates = splitting options that split the sampleset into very similar groups, percent similarity is given
summary(cartmodel)


