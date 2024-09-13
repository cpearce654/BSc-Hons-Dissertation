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

mydata <- read_xlsx("India No Urb.xlsx")
summary(mydata)
linearreg <- lm(CO2 ~ GDP + FDI + Gini, data=mydata)
summary(linearreg)

#Stationarity
ts1<-ts(mydata, start = 1961, frequency=1)
CO2<-ts(mydata$CO2, start=1961, frequency=1)
GDP<-ts(mydata$GDP, start=1961, frequency=1)
Gini<-ts(mydata$Gini, start=1961, frequency=1)
FDI<-ts(mydata$FDI, start=1961, frequency=1)
adf.test(CO2)
adf.test(GDP)
adf.test(FDI)
adf.test(Gini)

#Misspecification
res1 = resid(linearreg)
data:res1
#Jarque-Bera test
jarque.bera.test(res1)
#RAMSAY RESET TEST
resettest(linearreg, power=2:3, type = "fitted")

#Multicollinearity
vif(linearreg)

#Heteroskedasticity

#Breusch-Pagan Test
residuals <- resid(linearreg)
fitted_values <- fitted(linearreg)
with(mydata,plot(fitted_values, residuals))

bptest(linearreg)

#Whites Test
bptest(linearreg, ~ + GDP*FDI + Gini*GDP + Gini*FDI + I(GDP^2) + I(FDI^2) + I(Gini^2), data=mydata)

#White's Standard Errors - Solve Hetero
coeftest(linearreg, vcov = vcovHC(linearreg))

#Autocorrelation
with(mydata, plot(time, res1))

#Create first lag

# Remove missing values
mydata <- na.omit(mydata)

# Fit linear regression model
linearreg <- lm(CO2 ~ GDP + FDI + Gini, data = mydata)

# Extract residuals
mydata$res1 <- resid(linearreg)

# Create lagged residuals
mydata$res_l1 <- c(NA, mydata$res1[-nrow(mydata)])

# Add residuals as a new column to mydata
mydata$res1 <- resid(linearreg)

# Create a new data frame containing the residuals
residuals <- data.frame(resid = resid(linearreg))
# Merge the residuals data frame with the original data frame
mydata <- cbind(mydata, residuals)
#Plot residuals against time variable
with(mydata, plot(time, res1))

#Plot residuals against their lag
with(mydata, plot(res_l1, res1))

#durbin watson test
durbinWatsonTest(linearreg)

#Breusch Godfrey LM test
bgtest(CO2 ~ GDP + FDI + Gini, order=4, data=mydata)

#durbins h test
n <- nrow(mydata)
mydata$CO2_l1 <- c(NA, mydata$CO2[-n])
reg_lag <- lm(CO2 ~ GDP + FDI + Gini + CO2_l1, data=mydata)
durbinH(reg_lag, "CO2_l1")

install.packages("dplyr")
library(dplyr)
mydata <- mydata %>% mutate(CO2_l1 = lag(CO2, 1))
n <- nrow(mydata)
reg_lag <- lm(CO2 ~ GDP + FDI + Gini + CO2_l1, data = mydata)
durbinH(reg_lag, ylag1var="CO2_l1")


# NEW ATTEMPT
# create lagged variables
mydata$CO2_l1 <- c(NA, head(mydata$CO2, -1))

# fit regression model without lagged dependent variable
reg <- lm(CO2 ~ GDP + Gini + FDI, data = mydata)

# test for autocorrelation in residuals
resid_autocorrelation <- durbinH(reg, "lnCO2_l1")
resid_autocorrelation

summary(resid_autocorrelation)

#GRANGER TESTS
library(lmtest)

# Null hypothesis: GDP does not Granger cause CO2
grangertest(CO2 ~ GDP, data = mydata, order = 2)

# Null hypothesis: Gini does not Granger cause CO2
grangertest(CO2 ~ Gini, data = mydata, order = 2)

# Null hypothesis: FDI does not Granger cause CO2
grangertest(CO2 ~ FDI, data = mydata, order = 2)
