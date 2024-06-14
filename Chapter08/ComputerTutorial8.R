################################################################################
#                                                                              #
#                   Exercise Sheet 8: Heteroskedasticity                       # 
#                                  Assignment 2                                #
#                                                                              #
################################################################################
# necessary packages: if not installed, type install.packages(c('lmtest', 'sandwich', 'nlme')) 
# in the console and press Enter

library(lmtest) # for function: bptest, vcovHC
library(sandwich) # for function: coeftest
library(nlme) # for function: gls
##### Assignment 2: Travel #####

# Load data 
travel <- read.csv('travel.csv')

# a) model 1
regA <- lm(MILES ~ INCOME + AGE + KIDS, data = travel)
summary(regA)

# b) scatterplots
# residual graphs
# age
plot(regA$residuals ~ travel$AGE, main = 'Age')
abline(h=0)
# kids
plot(regA$residuals ~ travel$KIDS, main = 'Kids')
abline(h=0)
# income
plot(regA$residuals ~ travel$INCOME, main = 'Income')
abline(h=0)

# c) Breusch-Pagan Test with INCOME as explanatory variable
# save squared residuals to be used as dependent variable in auxiliary regression
resid2 <- regA$residuals^2
# fit auxiliary regression
aux <- lm(resid2 ~ travel$INCOME)
# save summary of the auxiliary regression
sumAux <- summary(aux)
# calculate test statistic - LM test -> N*R^2
bp <- nrow(travel)*sumAux$r.squared
# calculate p-value ->  LM test is chi-squared distributed
pVal <- pchisq(bp, 1, lower.tail = FALSE)

# alternatively use bptest() function from package lmtest
bptest(regA, varformula = ~ INCOME, data = travel)


# d) OLS with correct standard errors
coeftest(regA, vcov = vcovHC(regA, type = "HC1"))

# e) GLS: assumption about variance sigma_i^2 = sigma^2*income_i^2
# add a column of 1's to data set travel
travel$C <- rep(1, 200)
# transformation: divide all variables in the data set travel by INCOME
trans <- travel/travel$INCOME
# estimate the model
gls <- lm(MILES ~ C + AGE + KIDS, trans)
summary(gls)
# alternatively, use gls() function from package nlme
gls_alt <- gls(MILES ~ INCOME + AGE + KIDS, data = travel,
                 weights = varFixed(~ INCOME^2))
summary(gls_alt)