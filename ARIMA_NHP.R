#Largely following tutorial from 
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
#See script Example_ARIMA.R

library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)

setwd("E:/manuscripts/Management_Manuscript/Analysis/Rstuff")

#Bring in data table(s)
dataNHP = read.csv("deflectNHP.csv")

dataNHP$DateDummy = as.Date(dataNHP$DateDummy)

ggplot(dataNHP, aes(DateDummy, NHP_SumOfdeflectInt)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of - and + deflections") + 
  xlab("") + 
  theme_bw()

ggplot(dataNHP, aes(DateDummy, NHP_SumOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Sum of annual deflection") + 
  xlab("") + 
  theme_bw()

ggplot(dataNHP, aes(DateDummy, NHP_AvgOfmeanU1X)) +
  geom_line() + 
  geom_hline(yintercept=0) + 
  ylab("Average magnitude of deflection") + 
  xlab("") + 
  theme_bw()

netnhp_ts = ts(dataNHP[, c('NHP_SumOfdeflectInt')])
mag_ts = ts(dataNHP[, c('NHP_SumOfmeanU1X')])
avg_ts = ts(dataNHP[, c('NHP_AvgOfmeanU1X')])
#---
adf.test(netnhp_ts, alternative = "stationary")

Acf(netnhp_ts, main='')
Pacf(netnhp_ts, main='')

adf.test(mag_ts, alternative="stationary")

Acf(mag_ts, main='')
Pacf(netnhp_ts, main='')

adf.test(avg_ts, alternative="stationary")

Acf(avg_ts, main='')
Pacf(avg_ts, main='')

auto.arima(netnhp_ts, seasonal = FALSE)

fit<-auto.arima(netnhp_ts, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(2,0,1) Model Residuals')

#Next steps: Try to figure out how to do cross-correlation in ARIMA, 
#or go back to basic multiple linear regression

#Bring in cross-correlates
precnhp_ts = ts(dataNHP[, c('NHP_Precip')])

ccfvalsnhp_Precip = ccf(precnhp_ts, netnhp_ts)

ccfvalsnhp_Precip

#Drought Index
drtnhp_ts = ts(dataNHP[, c('NHP_PDSI')])

ccfvalsnhp_Drought = ccf(drtnhp_ts[1:83], netnhp_ts[1:83])

ccfvalsnhp_Drought

#Change in Drought Index
Ddrtnhp_ts = ts(dataNHP[, c('NHP_PDSI_Change')])

ccfvalsDnhp_Drought = ccf(Ddrtnhp_ts[2:83], netnhp_ts[2:83])

ccfvalsDnhp_Drought

#Change in Corn Price - completely insignificant
#Dcorn_ts = ts(dataNHP[, c('CornPriceChange')])

#ccfvalsDcorn = ccf(Dcorn_ts[2:84], netnhp_ts[2:84])

#ccfvalsDcorn

#Crop prices
#Corn
corn_ts = ts(dataNHP[, c('CornPrice_2016USD')])

ccfvalsnhp_Corn = ccf(corn_ts, netnhp_ts)

ccfvalsnhp_Corn

#Sorghum
sorg_ts = ts(dataNHP[, c('SorghumPrice_2016USD')])

ccfvalsnhp_Sorghum = ccf(sorg_ts, netnhp_ts)

ccfvalsnhp_Sorghum


#From tutorial: Negative correlations indicate that a below average value of the X variable 
# is likely to lead to an above average response (positive breakpoints) after the given lag.
# Also negative ~ negative. Positive CCF can also mean, e.g., low crop prices ~ negative breaks. 

#MULTIPLE LINEAR REGRESSION MODEL###########
lmNHP = read.csv("lmNHP.csv")
lmN <- lmNHP[6:84,]

fitN <- lm(NHP_SumOfdeflect~ Corn_LagMinus2 + PDSI_LagMinus5 + PDSI_LagMinus4 + 
             PDSI_LagMinus3 + PDSI_LagMinus2 + PDSI_LagMinus1, data=lmN)
summary(fitN)
resN <- residuals(fitN)
anova(fitN)
predN <- fitted(fitN)
lmN$resN = resN
lmN$predN = predN

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitN)

plotN <- ggplot(lmN, aes(Year)) +
  geom_ribbon(aes(ymin=predN,ymax=NHP_SumOfdeflect), fill="grey87") + 
  geom_line(data=lmN, linetype="dotted", aes(x=Year, y=predN)) + 
  geom_line(data=lmN, aes(x=Year, y=NHP_SumOfdeflect)) + 
  #geom_line(data=lmN, aes(x=Year, y=Corn_LagMinus3)) + 
  geom_hline(yintercept=0) + 
  #geom_abline(slope=1, intercept=0) + 
  theme_bw()
plotN

  

