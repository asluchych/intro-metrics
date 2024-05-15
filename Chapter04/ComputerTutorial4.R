################################################################################
#                                                                              #
#      Exercise Sheet 4: Prediction, Goodness-of-Fit and Modeling Issues      # 
#                           Assignment 3                                       #
#                                                                              #
################################################################################
# package: if it's not installed, type install.packages("tseries") in the console
library(tseries) # for the Jarque-Bera-test function


##### Assignment 3: House Prices #####

# load data
houses <- read.csv('houseprices.csv')

# a) interpretation b_2 and R^2
# model (1)
lin <- lm(PRICE ~ SQM, data = houses)
summary(lin)

# model (2)
logLin <- lm(log(PRICE) ~ SQM, data = houses)
summary(logLin)
coef(summary(logLin))

# model (3)
logLog <- lm(log(PRICE) ~ log(SQM), data = houses)
summary(logLog)

# b) comparison
xVals <- seq(20, 800, 0.1)
fittedValsLogLin <- exp(logLin$coefficients[1] + logLin$coefficients[2]*xVals)
fittedValsLogLog <- exp(logLog$coefficients[1] + logLog$coefficients[2]*log(xVals))

# (i) regression lines - scatter plot PRICE, SQM
plot(PRICE ~ SQM, data = houses, main = "Prices vs. Square Meters")
abline(lin, col = "green")
lines(fittedValsLogLin ~ xVals, col = "red")
lines(fittedValsLogLog ~ xVals, col = "blue")
legend("topleft", legend = c("Linear", "Log-Linear", "Log-Log"),
       col = c("green", "red", "blue"), lwd = 1)

# (ii) residuals
# linear model
plot(lin$residuals ~ houses$SQM, main = "Residuals Linear Model")
abline(h=0)

# log-linear model
plot(logLin$residuals ~ houses$SQM, main = "Residuals Log-Linear Model")
abline(h=0)

# log-log Model
plot(logLog$residuals ~ houses$SQM, main = "Residuals Log-Log Model")
abline(h=0)

# (iv) Jarque Bera Test: H_0 residuals are normally distributed
# linear model
jarque.bera.test(lin$residuals)

# log-linear model
jarque.bera.test(logLin$residuals)

# log-log model
jarque.bera.test(logLog$residuals)
