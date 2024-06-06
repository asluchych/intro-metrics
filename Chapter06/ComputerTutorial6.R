################################################################################
#                                                                              #
#               Exercise Sheet 6: Multiple Regression Model 2                  # 
#                        Assignments 4 and 5                                   #
#                                                                              #
################################################################################

# package lmtest: if it's not installed, type install.packages('lmtest') in the console 
library(lmtest)

##### Assignments 4: Model Specification #####

# Load data
wages <- read.csv('wages.csv')

# a) model A
regWageA <- lm(WAGE ~ EDUC + AGE, data = wages)
summary(regWageA)

# b) RESET
resettest(regWageA, power = 2)

# c) model C
regWageC <- lm(WAGE ~ EDUC + I(EDUC^2) + AGE + I(AGE^2), data = wages)
summary(regWageC)

# d) RESET
resettest(regWageC, power = 2)

# e) model E
regWageE <- lm(WAGE ~ EDUC + I(EDUC^2) + AGE + I(AGE^2) + CITY, data = wages )
summary(regWageE)

##### Assignment 5: Collinearity #####

# Load data
prod <- read.csv('production.csv')

# a) model
regProdA <- lm(log(Q) ~  log(L) + log(K), data = prod)
summary(regProdA)

# b) correlation
cor(log(prod))

# c) restricted model
# define new variables for the restricted model
prod$y <- log(prod$Q) - log(prod$K)
prod$x <- log(prod$L) - log(prod$K)
# fit the model
regProdC <- lm(y ~ x, data = prod)
summary(regProdC)
# calculate SSE_R and SSE_U
sseR <- sum((regProdC$residuals - mean(regProdC$residuals))^2)
sseU <- sum((regProdA$residuals - mean(regProdA$residuals))^2)
# F-statistic and p-value
fStat <- ((sseR - sseU)/1)/(sseU/regProdA$df.residual)
pVal <- 1 - pf(fStat, 1, 30)

# alternatively
# package car: if it's not installed, type install.packages('car') in the console 
library(car)
linearHypothesis(regProdA, "log(L) + log(K) = 1")