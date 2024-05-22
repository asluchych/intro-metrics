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


# Extra: LS estimates
x <- houses$SQM
y <- houses$PRICE

# Point estimates
b2 <- cov(x, y)/var(x)
b1 <- mean(y) - b2*mean(x)

# Point predictions
n <- nrow(houses)
y.hat <- rep(NA, n)
for (i in seq(n)) {
  y.hat[i] <- b1 + b2*x[i]
}

# Standard error of regression
sse <- sum((y - y.hat)^2)
var.reg <- sse/(n - 2)
se.reg <- sqrt(var.reg)


# Standard error of estimates
se.b2 <- sqrt(var.reg/sum((x - mean(x))^2))
se.b1 <- sqrt(var.reg * sum(x^2)/(n*sum((x - mean(x))^2)))
cov.b1.b2 <- var.reg*(-mean(x))/sum((x - mean(x))^2)

vcov(lin)

# R^2
sse <- sum((y - y.hat)^2)
sst <- sum((y - mean(y))^2)
r2 <- 1 - sse/sst


# Extra: standard normal vs t-distribution
x <- seq(-3, 3, length.out = 100)
plot(x, dnorm(x), col = 'blue', type = 'l')
points(x, dt(x, 1),  col = 'red', type = 'l')
points(x, dt(x, 5),  col = 'yellow', type = 'l')
points(x, dt(x, 20),  col = 'black', type = 'l')
points(x, dt(x, 50),  col = 'green', type = 'l')