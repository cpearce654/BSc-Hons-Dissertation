install.packages("readxl", dependencies = T)
install.packages("Rcpp")
install.packages("car")
install.packages("lmtest")
install.packages("tseries")
install.packages("sandwich")
install.packages("ecm")
install.packages("tseries")
library(readxl)
library(Rcpp)
library(car)
library(lmtest)
library(tseries)
library(sandwich)
library(ecm)
library(tseries)

mydata <- read_xlsx("Final China.xlsx")
summary(mydata)
linearreg <- lm(lnCO2 ~ lnGDP + lnGini + lnUrb + lnFDI, data=mydata)
summary(linearreg)

#Misspecification
res1 = resid(linearreg)
data:res1
#Jarque-Bera test
jarque.bera.test(res1)
#RAMSAY RESET TEST
resettest(linearreg, power=2:3, type = "fitted")

#STATIONARITY
# load required packages
library(tseries)
# detach all loaded packages
detach(package:all)
# load only tseries package
library(tseries)

# read data
mydata <- read_xlsx("Final China.xlsx")

# perform unit root test for each variable
ur.df(mydata$lnCO2)
ur.df(mydata$lnGDP)
ur.df(mydata$lnGini)
ur.df(mydata$lnUrb)
ur.df(mydata$lnFDI)







# Load the urca package
install.packages("urca")
library(urca)
# perform ADF test on the residuals
adf_test <- ur.df(res1, type="drift", lags=10)
# display the ADF test results
summary(adf_test)



CO2_ts <- ts(na.omit(mydata$lnCO2))
GDP_ts <- ts(na.omit(mydata$lnGDP))
Gini_ts <- ts(na.omit(mydata$lnGini))
Urb_ts <- ts(na.omit(mydata$lnUrb))
FDI_ts <- ts(na.omit(mydata$lnFDI))
ts_data <- ts.intersect(CO2_ts, GDP_ts, Gini_ts, Urb_ts, FDI_ts)
reg <- lm(CO2_ts ~ GDP_ts + Gini_ts + Urb_ts + FDI_ts, data=ts_data)
summary(reg)
adf.test(ts_data)

