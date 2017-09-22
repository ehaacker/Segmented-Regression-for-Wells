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
urtable = read.csv("UrSegRegTable.csv")

meas = read.csv("Munge1_InputsTable.csv")

precip = read.csv("Precip_Monthly_HPA_years.csv")
precipAnn <- aggregate(Precip~YEAR, precip, FUN=sum)
#Get mean precip for figure hline
precipPost1929 <- precipAnn[precipAnn$YEAR>1928,]
meanPrecip <- mean(precipPost1929[["Precip"]])

urtable$BreakMinusMin <- urtable$modeYr - urtable$MinOfWaterYear
urtable$MaxMinusBreak <- urtable$MaxOfWaterYear - urtable$modeYr
urtable$midbreak <- urtable$BreakMinusMin - trunc(((urtable$BreakMinusMin + urtable$MaxMinusBreak + 1)/2))

#Get GMA bracketeers
bracketeers<-urtable[urtable$Bracketeer=="Bracketeer",]

bracketeers$modeminusGMA <- bracketeers$modeYr - bracketeers$EstYear

#Get positive and negative deflectors
posdeflect <- urtable[urtable$meanU1X>0,]
negdeflect <- urtable[urtable$meanU1X<0,]

posbrack <- bracketeers[bracketeers$meanU1X>0,]
negbrack <- bracketeers[bracketeers$meanU1X<0,]

#write.csv(bracketeers, "E:/manuscripts/Management_Manuscript/Analysis/Rstuff/Bracketeers.csv")

#Create linear regression
#x = myWell$WaterYear
#y = myWell$WTelev
#linmod<-lm(y~x)

########################################################################
#SCATTERPOINT
plot <- ggplot() + geom_point(data = brast, aes(x = SatThickBreak, y = deflect)) +
#  ggtitle(paste("Well",mySiteNo, sep=' ')) +
#  xlab("Year") +
#  ylab("Water Level") +
#  geom_abline() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot

########################################################################
#HISTOGRAMALAMABANANA
plot <- ggplot() + 
  geom_histogram(data = brast, binwidth=2, col="black", fill="ivory1", aes(x = SatThickBreak)) + 
#  geom_freqpoly(data=posbrack, binwidth=1, col="dodgerblue4", size = 1.5, aes(x=modeminusGMA)) + 
#  geom_freqpoly(data=negbrack, binwidth=1, col="orangered4", size = 1.5, aes(x=modeminusGMA)) + 
  ylab("Frequency") +
  geom_vline(xintercept=0) + 
#  scale_x_continuous(limits=c(-7,7)) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot

#-------------------------------------------------------------------------------------------------------------
#Bracketeers breakpoints, broken out for + and -, normalized to GMA year - don't touch
plot <- ggplot() + 
  geom_histogram(data = bracketeers, binwidth=1, col="black", fill="ivory1", aes(x = modeminusGMA)) + 
  geom_freqpoly(data=posbrack, binwidth=1, col="dodgerblue4", size = 1.5, aes(x=modeminusGMA)) + 
  geom_freqpoly(data=negbrack, binwidth=1, col="orangered4", size = 1.5, aes(x=modeminusGMA)) + 
  ylab("Frequency") +
  geom_vline(xintercept=0) + 
  #  scale_x_continuous(limits=c(-7,7)) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot

#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#Comparison of normal distribution and distribution of breakpoints around midpoints of well records
normal<-data.frame(rnorm(10000000, mean=0, sd=10.53607))
colnames(normal)<- "normal"

normal<-data.frame(rnorm(10000000, mean=0, sd=10.53607))
colnames(normal)<- "normal"
plot <- ggplot() + 
  #  geom_histogram(data = urtable, binwidth=1, col="black", fill="steelblue3", aes(x = midbreak)) +
  #  geom_freqpoly(data=posdeflect, col="steelblue4", size = 1.5, aes(x=modeYr)) + 
  #  geom_freqpoly(data=negdeflect, col="orangered4", size = 1.5, aes(x=modeYr)) + 
  geom_density(data=normal, col="darkgoldenrod2", size = 1, aes(x=normal)) + 
  #  geom_density(data=urtable, col="black", size = 1, aes(x=midbreak)) + 
  #  geom_density(data=bracketeers, col="steelblue4", size = 1.5, aes(x=midbreak)) + 
  geom_density(data=posdeflect, col="steelblue4", size = 1.5, aes(x=midbreak)) + 
  geom_density(data=negdeflect, col="orangered4", size = 1.5, aes(x=midbreak)) + 
  geom_vline(xintercept=0) + 
  ylab("Frequency") +
  #  scale_x_continuous(limits=c(-7,7)) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plot


#ggsave(paste(mySiteNo,".eps",sep=''), width = 10, height = 8, units = c("cm"))
#ggsave(paste(mySiteNo,".png",sep=''), width = 10, height = 8, units = c("cm"))

