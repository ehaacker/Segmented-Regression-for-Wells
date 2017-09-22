#Largely following tutorial from 
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
#See script Example_ARIMA.R

library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)

setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data table(s)
dataSHP = read.csv("deflectSHP.csv")

dataSHP$DateDummy = as.Date(dataSHP$DateDummy)

ggplot(dataSHP, aes(DateDummy, SHP_SumOfdeflectInt)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of - and + deflections") + 
  xlab("") + 
  theme_bw()

ggplot(dataSHP, aes(DateDummy, SHP_SumOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of annual deflection") + 
  xlab("") + 
  theme_bw()

ggplot(dataSHP, aes(DateDummy, SHP_AvgOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Average magnitude of deflection") + 
  xlab("") + 
  theme_bw()

netshp_ts = ts(dataSHP[, c('SHP_SumOfdeflectInt')])
netshp_ts = netshp_ts[1:83]
mag_ts = ts(dataSHP[, c('SHP_SumOfmeanU1X')])
mag_ts = mag_ts[1:83]
avg_ts = ts(dataSHP[, c('SHP_AvgOfmeanU1X')])
avg_ts = avg_ts[1:83]
#---
adf.test(netshp_ts, alternative = "stationary")

Acf(netshp_ts, main='')
Pacf(netshp_ts, main='')

adf.test(mag_ts, alternative="stationary")

Acf(mag_ts, main='')
Pacf(netshp_ts, main='')

adf.test(avg_ts, alternative="stationary")

Acf(avg_ts, main='')
Pacf(avg_ts, main='')

auto.arima(netshp_ts, seasonal = FALSE)

fit<-auto.arima(netshp_ts, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(2,0,1) Model Residuals')

#Next steps: Try to figure out how to do cross-correlation in ARIMA, 
#or go back to basic multiple linear regression

#Bring in cross-correlates - 

#Precip
precshp_ts = ts(dataSHP[, c('SHP_Precip')])
precshp_ts = precshp_ts[1:83]

ccfvalsSHP_Precip = ccf(precshp_ts, netshp_ts)

ccfvalsSHP_Precip

#Drought Index
drtshp_ts = ts(dataSHP[, c('SHP_PDSI')])
drtshp_ts = drtshp_ts[1:83]

ccfvalsSHP_Drought = ccf(drtshp_ts, netshp_ts)

ccfvalsSHP_Drought

#Crop prices
#Corn
corn_ts = ts(dataSHP[, c('CornPrice_2016USD')])
cornshp_ts = corn_ts[1:83]

ccfvalsSHP_Corn = ccf(cornshp_ts, netshp_ts)

#Cotton
cot_ts = ts(dataSHP[, c('CottonPrice_2016USD')])
cotshp_ts = cot_ts[1:83]

ccfvalsSHP_Cotton = ccf(cot_ts, netshp_ts)

ccfvalsSHP_Cotton



#From tutorial: Negative correlations indicate that a below average value of the X variable (in this case
#precip) is likely to lead to an above average response (positive breakpoints) after the given lag.


#MULTIPLE LINEAR REGRESSION MODEL###########
lmSHP = read.csv("lmSHP.csv")
lmS <- lmSHP[9:83,]

fitS <- lm(SHP_SumOfdeflect~ CotPrice_LagMinus8 + PDSI_LagMinus3 + PDSI_LagMinus4 + PDSI_LagMinus5, data=lmS)
summary(fitS)
resS <- residuals(fitS)
anova(fitS)
predS <- fitted(fitS)
lmS$resS = resS
lmS$predS = predS

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitS)

plotS <- ggplot(lmS, aes(Year)) +
  geom_ribbon(aes(ymin=predS,ymax=SHP_SumOfdeflect), fill="grey87") + 
  geom_line(data=lmS, linetype="dotted", aes(x=Year, y=predS)) + 
  geom_line(data=lmS, aes(x=Year, y=SHP_SumOfdeflect)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotS

grid.arrange(plotN,plotC,plotS, nrow=3)


###OLD ONE - with positive lags - the "right" one, which Dave won't let me use because he's intellectually lazy
lmS1 <- lmSHP[9:78,]


fitS1 <- lm(SHP_SumOfdeflect~ CotPrice_LagMinus8 + 
             PDSI_LagPlus2 + PDSI_LagPlus3 + 
             PDSI_LagMinus3 + PDSI_LagMinus4 + PDSI_LagMinus5, data=lmS1)
summary(fitS1)
resS1 <- residuals(fitS1)
anova(fitS1)
predS1 <- fitted(fitS1)
lmS1$resS1 = resS1
lmS1$predS1 = predS1

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitS1)

plotS1 <- ggplot(lmS1, aes(Year)) +
  geom_ribbon(aes(ymin=predS1,ymax=SHP_SumOfdeflect), fill="grey87") + 
  geom_line(data=lmS1, linetype="dotted", aes(x=Year, y=predS1)) + 
  geom_line(data=lmS1, aes(x=Year, y=SHP_SumOfdeflect)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotS1

grid.arrange(plotN,plotC,plotS1, nrow=3)
