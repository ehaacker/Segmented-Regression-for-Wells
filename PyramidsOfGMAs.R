library(segmented)
library(ggplot2)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")
#WELLS#
urtable = read.csv("UrSegRegTable_DeflectDirect.csv")
#PRECIP#
#Breakdown of precip by region - parent table
precipR = read.csv("MeanMonthlyPrecip_HPA_Regions_years.csv")

precipNHPmonthly = precipR[precipR$ID==111,]
precipNHP = aggregate(Precip~YEAR, precipNHPmonthly, FUN=sum)
precipNHPpost1940 <- precipNHP[precipNHP$YEAR>1940,]
meanPrecipNHP <- mean(precipNHPpost1940[["Precip"]])
#
precipCHPmonthly = precipR[precipR$ID==111,]
precipCHP = aggregate(Precip~YEAR, precipCHPmonthly, FUN=sum)
precipCHPpost1940 <- precipCHP[precipCHP$YEAR>1940,]
meanPrecipCHP <- mean(precipCHPpost1940[["Precip"]])
#
precipSHPmonthly = precipR[precipR$ID==111,]
precipSHP = aggregate(Precip~YEAR, precipSHPmonthly, FUN=sum)
precipSHPpost1940 <- precipSHP[precipSHP$YEAR>1940,]
meanPrecipSHP <- mean(precipSHPpost1940[["Precip"]])

#------------------------------------------------------------------------------------------------------------------------------
###################################TEXAS###################################
#------------------------------------------------------------------------------------------------------------------------------
#TX Permian Basin

PermBasin<-urtable[urtable$GMA_Name=="Permian Basin UWCD",]

pyr2 <- ggplot(PermBasin, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=PermBasin$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(PermBasin$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015), breaks=seq(from=1940, to=2010, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()
  
pyr2

#------------------------------------------------------------------------------------------------------------------------------
#TX UWCD1

UWCD1 <- urtable[urtable$GMA_Name=="High Plains UWCD No.1",]

pyr7 <- ggplot(UWCD1, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=UWCD1$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(UWCD1$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015), breaks=seq(from=1940, to=2010, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr7

#------------------------------------------------------------------------------------------------------------------------------
#TX SandLand

SandLand <- urtable[urtable$GMA_Name=="Sandy Land UWCD",]

pyr6 <- ggplot(SandLand, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=SandLand$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(SandLand$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015), breaks=seq(from=1940, to=2010, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr6


#------------------------------------------------------------------------------------------------------------------------------
#TX SPlains

SPlains <- urtable[urtable$GMA_Name=="South Plains UWCD",]

pyr1 <- ggplot(SPlains, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=SPlains$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(SPlains$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015), breaks=seq(from=1940, to=2010, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr1
#------------------------------------------------------------------------------------------------------------------------------
#TX Llano

Llano <- urtable[urtable$GMA_Name=="Llano Estacado UWCD",]

pyr4 <- ggplot(Llano, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=Llano$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(Llano$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015), breaks=seq(from=1940, to=2010, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr4

plotSHP <- ggplot(precipSHP, aes(YEAR)) +
  geom_ribbon(aes(ymin=meanPrecipSHP, ymax=Precip),fill="lightsteelblue3") + 
  geom_line(data=precipSHP, col="black", aes(x=YEAR, y=Precip)) + 
  xlab("Year") + 
  ylab("Precip, mm") + 
  geom_hline(yintercept=meanPrecipSHP) + 
  #  scale_y_continuous(limits=c(200,800)) + 
  scale_x_continuous(breaks=seq(from=1940, to=2015, by=10), limits=c(1940,2015)) + 
  theme_bw()
  #  theme(axis.text.x = element_text(size = 12), 
  #      axis.text.y = element_text(size = 12), 
  #      axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
  #      title = element_text(size = 14), 
        #        text=element_text(family="Arial"),
  #      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #      panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotSHP

grid.arrange(pyr7, pyr1, pyr4, pyr2, plotSHP, nrow=5)
grid.arrange(pyr7, pyr1, pyr4, nrow=3)

#------------------------------------------------------------------------------------------------------------------------------
###################################KANSAS###################################
#------------------------------------------------------------------------------------------------------------------------------
#KS GMD2

GMD2 <- urtable[urtable$GMA_Name=="Equus Beds GMD #2",]

pyr37 <- ggplot(GMD2, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=GMD2$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(GMD2$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1955,2017)) +  
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr37

#------------------------------------------------------------------------------------------------------------------------------
#KS GMD1

GMD1 <- urtable[urtable$GMA_Name=="Western Kansas GMD #1",]

pyr39 <- ggplot(GMD1, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=GMD1$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(GMD1$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr39

#------------------------------------------------------------------------------------------------------------------------------
#KS GMD3

GMD3 <- urtable[urtable$GMA_Name=="Southwest Kansas GMD #3",]

pyr40 <- ggplot(GMD3, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=GMD3$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(GMD3$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr40

#------------------------------------------------------------------------------------------------------------------------------
#KS GMD4

GMD4 <- urtable[urtable$GMA_Name=="Northwest Kansas GMD #4",]

pyr38 <- ggplot(GMD4, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=GMD4$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(GMD4$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr38

#------------------------------------------------------------------------------------------------------------------------------
#KS GMD5

GMD5 <- urtable[urtable$GMA_Name=="Big Bend GMD #5",]

pyr41 <- ggplot(GMD5, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=GMD5$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(GMD5$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr41
#------------------------------------------------------------------------------------------------------------------------------
#CHP PRECIP

plotCHP <- ggplot(precipCHP, aes(YEAR)) +
  geom_ribbon(aes(ymin=meanPrecipCHP, ymax=Precip),fill="lightsteelblue3") + 
  geom_line(data=precipCHP, col="black", aes(x=YEAR, y=Precip)) + 
  xlab("Year") + 
  ylab("Precip, mm") + 
  geom_hline(yintercept=meanPrecipCHP) + 
  #  scale_y_continuous(limits=c(200,800)) + 
  scale_x_continuous(breaks=seq(from=1940, to=2015, by=10), limits=c(1955,2015)) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        #        text=element_text(family="Arial"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotCHP

grid.arrange(pyr39, pyr37, pyr40, pyr38, pyr41, plotCHP, nrow=6)
#------------------------------------------------------------------------------------------------------------------------------
#KS REGIONAL

WKS = rbind(GMD1, GMD3, GMD4)

pyrWKS <- ggplot(WKS, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=WKS$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Western Kansas: GMDs 1, 3, and 4") + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyrWKS

EKS = rbind(GMD2, GMD5)

pyrEKS <- ggplot(EKS, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=EKS$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Central Kansas: GMDs 2 and 5") + 
  scale_x_continuous(limits=c(1955,2017)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyrEKS

grid.arrange(pyrWKS, pyrEKS, plotCHP, nrow=3)
grid.arrange(pyrWKS, pyrEKS, nrow=2)

#------------------------------------------------------------------------------------------------------------------------------
###################################NEBRASKA###################################
#------------------------------------------------------------------------------------------------------------------------------
#NE Central Platte
CPlat <- urtable[urtable$GMA_Name== "Central Platte",]

pyr17 <- ggplot(CPlat, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=CPlat$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(CPlat$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr17

#------------------------------------------------------------------------------------------------------------------------------
#NE Upper Big Blue
UpBigBlue <- urtable[urtable$GMA_Name== "Upper Big Blue",]

pyr16 <- ggplot(UpBigBlue, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=UpBigBlue$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(UpBigBlue$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr16

#------------------------------------------------------------------------------------------------------------------------------
#NE Little Blue
LilBue <- urtable[urtable$GMA_Name== "Little Bue",]

pyr12 <- ggplot(LilBue, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=LilBue$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(LilBue$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr12

#------------------------------------------------------------------------------------------------------------------------------
#NE Tri-Basin
TriB <- urtable[urtable$GMA_Name== "Tri-Basin",]

pyr11 <- ggplot(TriB, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=TriB$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(TriB$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr11

#------------------------------------------------------------------------------------------------------------------------------
#NE Lower Big Blue
LoBigBlue <- urtable[urtable$GMA_Name== "Lower Big Blue",]

pyr13 <- ggplot(LoBigBlue, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=LoBigBlue$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle(LoBigBlue$GMA_Name[1]) + 
  scale_x_continuous(limits=c(1940,2015)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyr13

#------------------------------------------------------------------------------------------------------------------------------
#NHP PRECIP

plotNHP <- ggplot(precipNHP, aes(YEAR)) +
  geom_ribbon(aes(ymin=meanPrecipNHP, ymax=Precip),fill="lightsteelblue3") + 
  geom_line(data=precipNHP, col="black", aes(x=YEAR, y=Precip)) + 
  xlab("Year") + 
  ylab("Precip, mm") + 
  geom_hline(yintercept=meanPrecipNHP) + 
  #  scale_y_continuous(limits=c(200,800)) + 
  scale_x_continuous(breaks=seq(from=1940, to=2015, by=10), limits=c(1940,2015)) + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.ticks = element_line(colour = 'black', size = 1, linetype = 'dashed'),
        title = element_text(size = 14), 
        #        text=element_text(family="Arial"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1), legend.position = 'none') 
plotNHP

grid.arrange(pyr17, pyr11, pyr16, pyr12, pyr13, plotNHP, nrow=6)
grid.arrange(pyr17, pyr11, pyr16, pyr12, pyr13, nrow=5)

#------------------------------------------------------------------------------------------------------------------------------
#NHP REGIONS

WNE = rbind(CPlat, TriB)

pyrWNE <- ggplot(WNE, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=WNE$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Central Platte and Tri-Basin NRDs") + 
  scale_x_continuous(limits=c(1940,2015), breaks = seq(from=1940, to=2015, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyrWNE
###########
ENE = rbind(LoBigBlue, UpBigBlue, LilBue)

pyrENE <- ggplot(ENE, aes(x=modeYr, y=meanU1X, fill=deflect)) + 
  geom_bar(stat="identity") + 
  geom_vline(xintercept=ENE$EstYear[1]) + 
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Upper Big Blue, Lower Big Blue, and Little Blue NRDs") + 
  scale_x_continuous(limits=c(1940,2015), breaks = seq(from=1940, to=2015, by=10)) + 
  ylab("Well Count") +
  xlab("Breakpoint Year") + 
  theme_bw()

pyrENE

grid.arrange(pyrWNE, pyrENE, plotNHP, nrow=3)
grid.arrange(pyrWNE, pyrENE, nrow=2)
