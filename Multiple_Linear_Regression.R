#The purpose of this script is to investigate the relationship between breakpoints and other things
#that change through time in the HPA, using multiple linear regression. 

library(ggplot2)

#Set working directory
setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data table(s)
dataNHP = read.csv("deflectNHP.csv")
dataCHP = read.csv("deflectCHP.csv")
dataSHP = read.csv("deflectSHP.csv")
#data = data[2:dim(data)[1],]

#try multiple regression
#NHP----------------------------------------------------
#Sum of deflection direction (net annual)
#CornPrice_2016USD - significant
#NHP_Precip - significant
#CornPrice_Lag1 - significant; better than no lag
fitTotesN <- lm(NHP_SumOfdeflectInt~ CornPrice_2016USD + NHP_Precip, data=dataNHP)
summary(fitTotesN)
resN <- residuals(fitTotesN)
anova(fitTotesN)
predN <- fitted(fitTotesN)
dataNHP$resN = resN
dataNHP$predN = predN

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitTotesN)

plotTotesN <- ggplot(dataNHP, aes(Year)) +
  geom_ribbon(aes(ymin=predN,ymax=NHP_SumOfdeflectInt), fill="grey87") + 
  geom_line(data=dataNHP, linetype="dotted", aes(x=Year, y=predN)) + 
  geom_line(data=dataNHP, aes(x=Year, y=NHP_SumOfdeflectInt)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotTotesN

fitMagN <- lm(NHP_SumOfmeanU1X~CornPrice_2016USD + NHP_Precip, data=dataNHP)
summary(fitMagN)
predMagN <- fitted(fitMagN)
dataNHP$predMagN = predMagN

plotMagN <- ggplot(dataNHP, aes(Year)) +
  geom_ribbon(aes(ymin=predMagN,ymax=NHP_SumOfmeanU1X), fill="grey87") + 
  geom_line(data=dataNHP, linetype="dotted", aes(x=Year, y=predMagN)) + 
  geom_line(data=dataNHP, aes(x=Year, y=NHP_SumOfmeanU1X)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotMagN

#CHP----------------------------------------------------
#Sum of deflection direction (net annual)
fitTotesC <- lm(CHP_SumOfdeflectInt~CHP_PDSI, data=dataCHP)
summary(fitTotesC)

fitMagC <- lm(CHP_SumOfmeanU1X~CornPrice_2016USD + CHP_Precip_Lag2, data=dataCHP)
summary(fitMagC)

#SHP----------------------------------------------------
#Sum of deflection direction (net annual)
fitTotesS <- lm(SHP_SumOfdeflectInt~SHP_PDSI, data=dataSHP)
summary(fitTotesS)

fitMagS <- lm(SHP_SumOfmeanU1X~CottonPrice_2016USD + SHP_Precip_Lag2, data=dataSHP)
summary(fitMagS)