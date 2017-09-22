#Libraries for analysis and plotting: 
library(segmented)
library(ggplot2)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data
input = read.csv("Wells_2_Interpolated_WT_Elev.csv")
input$DateDummy = as.Date(input$DateDummy)

#Get unique IDs for Wells
id = distinct(input, SiteNo)
listWells = c(id$SiteNo)

#Build a dataframe
key = c(1:length(listWells))
index<-data.frame(i = key, SiteNo = listWells)
write.csv(index,"E:/manuscripts/Management_Manuscript/Analysis/Rstuff/WellIndex.csv")
                  
########################################################################
#Create graph for one well at a time

#Consistent breakpoint, close mean and mode, bad R2: big jump(s)
i = 6206
mySiteNo = 382458097362001

#Just generally messy (also note very small water level range) - no breakpoint
i = 10152
mySiteNo = 411309100534901

#Thrown off by an outlier
i=102261
mySiteNo = 344016103541801

#OK fit for Seg Reg; R2 = 0.93
i = 187033
mySiteNo = 372541102260800

#
i = 297429
mySiteNo = 400014097581101

#Alt well "D" for Figure 5
i = 72410
mySiteNo = 340959101414505

#Get the data for this well
myWell<-input[input$SiteNo==mySiteNo,]
myWell<-myWell[1:dim(myWell)[1],]
  
#Create linear regression
x = myWell$WaterYear
y = myWell$WTelev
linmod<-lm(y~x)

slopex<- summary(linmod)$coefficients[2,1]
intercepty<-summary(linmod)$coefficients[1,1]
adjR2<-summary(linmod)$adj.r.squared
  
#Create segmented regression
#seggo<-segmented(linmod,seg.Z=~x, psi = mean(x)) #comment out for non segreg example
#summary(seggo) #comment out for non segreg example

########################################################################
#Plot
#seggoDF <- data.frame(x = seggo[["model"]]$x, y = as.data.frame(seggo[["fitted.values"]])) #comment out for non segreg example
i1 = 17533
mySiteNo1 = 340959101414505 

#Get the data for this well
myWell1<-input[input$SiteNo==mySiteNo1,]

#Create linear regression
x1 = myWell1$WaterYear
y1 = myWell1$WTelev
linmod1<-lm(y1~x1)

slopex1<- summary(linmod1)$coefficients[2,1]
intercepty1<-summary(linmod1)$coefficients[1,1]
adjR21<-summary(linmod1)$adj.r.squared

plot1 <- ggplot() + #geom_line(data = myWell1, aes(x = WaterYear, y = WTelev)) +
  geom_point(data = myWell1, aes(x = WaterYear, y = WTelev)) +
#  geom_line(data = seggoDF1, aes(x = x1, y = seggo1...fitted.values...)) + #comment out for non segreg examples
  geom_abline(mapping=NULL, data=NULL, slope=slopex1, intercept=intercepty1) + #comment out for segreg examples
  #scale_x_continuous(limits=c(1950,1980)) + 
  ggtitle(paste("Well",mySiteNo1, sep=' ')) +
  xlab("Year") +
  ylab("Water Level") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot1
############
i2 = 229406
mySiteNo2 = 375910097405801

#Get the data for this well
myWell2<-input[input$SiteNo==mySiteNo2,]

#Create linear regression
x2 = myWell2$WaterYear
y2 = myWell2$WTelev
linmod2<-lm(y2~x2)

slopex2<- summary(linmod2)$coefficients[2,1]
intercepty2<-summary(linmod2)$coefficients[1,1]
adjR22<-summary(linmod2)$adj.r.squared

plot2 <- ggplot() + geom_line(data = myWell2, aes(x = WaterYear, y = WTelev)) +
  geom_point(data = myWell2, aes(x = WaterYear, y = WTelev)) +
  #  geom_line(data = seggoDF, aes(x = x, y = seggo...fitted.values...)) + #comment out for non segreg examples
  #geom_abline(mapping=NULL, data=NULL, slope=slopex2, intercept=intercepty2) + #comment out for segreg examples
  ggtitle(paste("Well",mySiteNo2, sep=' ')) +
  #scale_x_continuous(limits=c(1950,1980)) + 
  xlab("Year") +
  ylab("Water Level") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot2
############
i3 = 324148
mySiteNo3 = 402110099140001

#Get the data for this well
myWell3<-input[input$SiteNo==mySiteNo3,]

#Create linear regression
x3 = myWell3$WaterYear
y3 = myWell3$WTelev
linmod3<-lm(y3~x3)

slopex3<- summary(linmod3)$coefficients[2,1]
intercepty3<-summary(linmod3)$coefficients[1,1]
adjR23<-summary(linmod3)$adj.r.squared

plot3 <- ggplot() + geom_line(data = myWell3, aes(x = WaterYear, y = WTelev)) +
  geom_point(data = myWell3, aes(x = WaterYear, y = WTelev)) +
  #  geom_line(data = seggoDF, aes(x = x, y = seggo...fitted.values...)) + #comment out for non segreg examples
  #geom_abline(mapping=NULL, data=NULL, slope=slopex3, intercept=intercepty3) + #comment out for segreg examples
  ggtitle(paste("Well",mySiteNo3, sep=' ')) +
  #scale_x_continuous(limits=c(1950,1980)) + 
  xlab("Year") +
  ylab("Water Level") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot3
############
i4 = 46835
mySiteNo4 = 334108102352001

#Get the data for this well
myWell4<-input[input$SiteNo==mySiteNo4,]

#Create linear regression
x4 = myWell4$WaterYear
y4 = myWell4$WTelev
linmod4<-lm(y4~x4)

slopex4<- summary(linmod4)$coefficients[2,1]
intercepty4<-summary(linmod4)$coefficients[1,1]
adjR24<-summary(linmod4)$adj.r.squared

plot4 <- ggplot() + geom_line(data = myWell4, aes(x = WaterYear, y = WTelev)) +
  geom_point(data = myWell4, aes(x = WaterYear, y = WTelev)) +
  #  geom_line(data = seggoDF, aes(x = x, y = seggo...fitted.values...)) + #comment out for non segreg examples
  #geom_abline(mapping=NULL, data=NULL, slope=slopex4, intercept=intercepty4) + #comment out for segreg examples
  ggtitle(paste("Well",mySiteNo4, sep=' ')) + 
  #scale_x_continuous(limits=c(1950,1980)) + 
  xlab("Year") +
  ylab("Water Level") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot4

grid.arrange(plot1, plot2, plot3, plot4, nrow=4)

#############################################################################################################
#Good plots - don't touch
#############################################################################################################

#Nice negative deflection - and it's a Bracketeer! EstDate = 1955, added geom_vline. MeanAdjR2 = 0.99, modeYr = 1961. 
iNeg = 143234
mySiteNoNeg = 355807101494301

#Get the data for this well
myWellNeg<-input[input$SiteNo==mySiteNoNeg,]

#Create linear regression
x = myWellNeg$WaterYear
y = myWellNeg$WTelev
linmod<-lm(y~x)

#Create segmented regression
seggoNeggo<-segmented(linmod,seg.Z=~x, psi = mean(x)) #comment out for non segreg example
summary(seggoNeggo) #comment out for non segreg example

seggoNeggoDF <- data.frame(x = seggoNeggo[["model"]]$x, y = as.data.frame(seggoNeggo[["fitted.values"]])) #comment out for non segreg example

plotNeg <- ggplot() + geom_point(data = myWellNeg, aes(x = WaterYear, y = WTelev)) +
  geom_line(data = seggoNeggoDF, aes(x = x, y = seggoNeggo...fitted.values...)) + #comment out for non segreg example
  ggtitle(paste("Well",mySiteNoNeg, sep=' ')) +
  geom_vline(xintercept=1955) + 
  xlab("Year") +
  ylab("Water Table Elevation, m") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotNeg

#-------------------------------------------------------------------------------------------------------------

#Positive deflection - and it's a Bracketeer! EstDate = 1972, added geom_vline. MeanAdjR2 = 0.73, modeYr = 1975.
iPos = 398445
mySiteNoPos = 410003097581201

#Get the data for this well
myWellPos<-input[input$SiteNo==mySiteNoPos,]

#Create linear regression
xP = myWellPos$WaterYear
yP = myWellPos$WTelev
linmod<-lm(yP~xP)

#Create segmented regression
seggoPos<-segmented(linmod,seg.Z=~xP, psi = mean(xP)) 
summary(seggoPos) 

seggoPosDF <- data.frame(xP = seggoPos[["model"]]$xP, yP = as.data.frame(seggoPos[["fitted.values"]])) 

plotPos <- ggplot() + geom_point(data = myWellPos, aes(x = WaterYear, y = WTelev)) +
  geom_line(data = seggoPosDF, aes(x = xP, y = seggoPos...fitted.values...)) + 
  ggtitle(paste("Well",mySiteNoPos, sep=' ')) +
  geom_vline(xintercept=1972) + 
  xlab("Year") +
  ylab("Water Table Elevation, m") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotPos

#-------------------------------------------------------------------------------------------------------------

#No segmented regression; kind of messy; also in Equus Beds, not NE. Adj R2 = 0.279592.
iNE = 229406
mySiteNE = 375910097405801

#Get the data for this well
myWellNE<-input[input$SiteNo==mySiteNE,]

#Create linear regression
xN = myWellNE$WaterYear
yN = myWellNE$WTelev
linmodNE<-lm(yN~xN)

slopeNE<- summary(linmodNE)$coefficients[2,1]
interceptNE<-summary(linmodNE)$coefficients[1,1]
adjR2NE<-summary(linmodNE)$adj.r.squared

plotNE <- ggplot() + geom_point(data = myWellNE, aes(x = WaterYear, y = WTelev)) +
  geom_abline(mapping=NULL, data=NULL, slope=slopeNE, intercept=interceptNE) + 
  geom_vline(xintercept=1973) + 
  ggtitle(paste("Well",mySiteNE, sep=' ')) +
  xlab("Year") +
  ylab("Water Table Elevation, m") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotNE

#-------------------------------------------------------------------------------------------------------------

#No segmented regression; nice linear trend. Adjusted R2 = 0.96.
iL = 1
mySiteL = 340959101414505

#Get the data for this well
myWellL<-input[input$SiteNo==mySiteL,]

#Create linear regression
xL = myWellL$WaterYear
yL = myWellL$WTelev
linmodL<-lm(yL~xL)

slopeL<- summary(linmodL)$coefficients[2,1]
interceptL<-summary(linmodL)$coefficients[1,1]
adjR2L<-summary(linmodL)$adj.r.squared

plotL <- ggplot() + geom_point(data = myWellL, aes(x = WaterYear, y = WTelev)) +
  geom_abline(mapping=NULL, data=NULL, slope=slopeL, intercept=interceptL) + #comment out for segreg examples
  ggtitle(paste("Well",mySiteL, sep=' ')) +
  geom_vline(xintercept=1951) + 
  xlab("Year") +
  ylab("Water Table Elevation, m") +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotL

#-------------------------------------------------------------------------------------------------------------
grid.arrange(plotPos, plotNE, plotNeg, plotL, ncol=2, nrow=2)
#-------------------------------------------------------------------------------------------------------------

##############################################################################################################


setwd('S:/Users/kingeri7/Management_Manuscript/Figures/OutputsFromR')
ggsave(paste(mySiteNo,".eps",sep=''), width = 10, height = 8, units = c("cm"))
ggsave(paste(mySiteNo,".png",sep=''), width = 10, height = 8, units = c("cm"))


##############################################################################################################

#Pie chart for number of wells with different seg reg characteristics
setwd("S:/Users/kingeri7/Management_Manuscript/StatsAnalysis")

#Bring in data for pie chart
piedata = read.csv("WellProportions.csv")

slices = piedata$NumberOfWells
labels = piedata$Category

piepalette = brewer.pal(length(slices),"Greens")

png("WellProportions.png")
piechart<-pie(slices, labels, main="Wells with 20+ Years of Data", col=piepalette, cex.main = 1.5)
dev.off()

