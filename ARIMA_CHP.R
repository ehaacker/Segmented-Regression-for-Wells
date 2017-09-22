#Largely following tutorial from 
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
#See script Example_ARIMA.R

library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)

setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data table(s)
dataCHP = read.csv("deflectCHP.csv")

dataCHP$DateDummy = as.Date(dataCHP$DateDummy)

ggplot(dataCHP, aes(DateDummy, CHP_SumOfdeflectInt)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of - and + deflections") + 
  xlab("") + 
  theme_bw()

ggplot(dataCHP, aes(DateDummy, CHP_SumOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of annual deflection") + 
  xlab("") + 
  theme_bw()

ggplot(dataCHP, aes(DateDummy, CHP_AvgOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Average magnitude of deflection") + 
  xlab("") + 
  theme_bw()

netCHP_ts = ts(dataCHP[, c('CHP_SumOfdeflect')])
mag_ts = ts(dataCHP[, c('CHP_SumOfmeanU1X')])
avg_ts = ts(dataCHP[, c('CHP_AvgOfmeanU1X')])
#---
adf.test(netCHP_ts, alternative = "stationary")

Acf(netchp_ts, main='')
Pacf(netchp_ts, main='')

adf.test(mag_ts, alternative="stationary")

Acf(mag_ts, main='')
Pacf(netchp_ts, main='')

adf.test(avg_ts, alternative="stationary")

Acf(avg_ts, main='')
Pacf(avg_ts, main='')

auto.arima(netchp_ts, seasonal = FALSE)

fit<-auto.arima(netchp_ts, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(2,0,1) Model Residuals')

#Next steps: Try to figure out how to do cross-correlation in ARIMA, 
#or go back to basic multiple linear regression

#Bring in cross-correlates
precCHP_ts = ts(dataCHP[, c('CHP_Precip')])

ccfvalsCHP_Precip = ccf(precCHP_ts, netCHP_ts)

ccfvalsCHP_Precip

#Drought Index
drtCHP_ts = ts(dataCHP[, c('CHP_PDSI')])

ccfvalsCHP_Drought = ccf(drtCHP_ts[1:83], netCHP_ts[1:83])

ccfvalsCHP_Drought

#Change in PDSI
DdrtCHP_ts = ts(dataCHP[, c('CHP_PDSI_Change')])

ccfvalsDdrt = ccf(DdrtCHP_ts[2:83], netCHP_ts[2:83])

ccfvalsDdrt

#Crop prices
#Corn
corn_ts = ts(dataCHP[, c('CornPrice_2016USD')])

ccfvalsCHP_Corn = ccf(corn_ts, netCHP_ts)

#Sorghum
sorg_ts = ts(dataCHP[, c('SorghumPrice_2016USD')])

ccfvalsCHP_Sorghum = ccf(sorg_ts, netCHP_ts)

ccfvalsCHP_Sorghum

#Change in Sorghum Price - completely insignificant
#Dsorg_ts = ts(dataCHP[, c('SorghumPrice_Change')])

#ccfvalsDsorg = ccf(Dsorg_ts[2:84], netCHP_ts[2:84])

#ccfvalsDsorg

#From tutorial: Negative correlations indicate that a below average value of the X variable (in this case
#precip) is likely to lead to an above average response (positive breakpoints) after the given lag.
#So, low precip before = 
#average value of SOI is likely to lead to a below average value of "recruit" about 6 months later.  
#And, a below average of SOI is associated with a likely above average recruit value about 6 months later.

#MULTIPLE LINEAR REGRESSION MODEL###########
lmCHP = read.csv("lmCHP.csv")
lmC1 <- lmCHP[8:80,]
lmC <- lmCHP[8:84,]

fitC1 <- lm(CHP_SumOfdeflect~ SorgPrice_LagMinus7 + PDSI_LagMinus1 + PDSI_LagPlus2+ PDSI_LagPlus3, data=lmC1) 
summary(fitC1)
resC1 <- residuals(fitC1)
anova(fitC1)
predC1 <- fitted(fitC1)
lmC1$resC1 = resC1
lmC1$predC1 = predC1

fitC <- lm(CHP_SumOfdeflect~ SorgPrice_LagMinus7 + PDSI_LagMinus1, data=lmC) #
summary(fitC)
resC <- residuals(fitC)
anova(fitC)
predC <- fitted(fitC)
lmC$resC = resC
lmC$predC = predC

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitC)

plotC <- ggplot(lmC, aes(Year)) +
  geom_ribbon(aes(ymin=predC,ymax=CHP_SumOfdeflect), fill="grey87") + 
  geom_line(data=lmC, linetype="dotted", aes(x=Year, y=predC)) + 
  geom_line(data=lmC, aes(x=Year, y=CHP_SumOfdeflect)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotC #This is the crappy one that Dave insists on using, because there are no positive lags

plotC1 <- ggplot(lmC1, aes(Year)) +
  geom_ribbon(aes(ymin=predC1,ymax=CHP_SumOfdeflect), fill="grey87") + 
  geom_line(data=lmC1, linetype="dotted", aes(x=Year, y=predC1)) + 
  geom_line(data=lmC1, aes(x=Year, y=CHP_SumOfdeflect)) + 
  geom_hline(yintercept=0) + 
  geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotC1
