################################################################################
#                                                                              #
#                   Exercise Sheet 7: Using Indicator Variables                # 
#                                  Assignment 2                                #
#                                                                              #
################################################################################

##### Assignment 2: Manager Salary #####

# Load data
sal <- read.csv('sal.csv')

# a) 
summary(sal)
cor(sal)
plot(SALARY ~ EDUC, data = sal)
plot(SALARY ~ GENDER, data = sal)
plot(SALARY ~ MANAGER, data = sal)

# b) model 1
regA <- lm(log(SALARY) ~ EDUC + GENDER, data = sal)
summary(regA)

# c) F-Test in summary(regA)

# d) H_0: beta_3 = 2*beta_2
# define auxiliary variable for restricted  model: x = EDUC+2*GENDER
sal$x <- sal$EDUC + 2*sal$GENDER
regRestricted <- lm(log(SALARY) ~ x, data = sal)
summary(regRestricted)
# Calculate SSE_R and SSE_U
sseR <- sum((regRestricted$residuals)^2)
sseU <- sum((regA$residuals)^2)
# F-statistic and p-value
fStat <- ((sseR - sseU)/1)/(sseU/regA$df.residual)
pVal <- 1 - pf(fStat, 1, regA$df.residual)
# alternatively
# package car: if it's not installed, type install.packages('car') in the console 
library(car)
linearHypothesis(regA, "GENDER = 2*EDUC")

# e) model e
regE <- lm(log(SALARY) ~ EDUC + GENDER + MANAGER, data = sal)
summary(regE)

# f) model f
regF <- lm(log(SALARY) ~ EDUC + GENDER + MANAGER + MANAGER*GENDER, data = sal)
summary(regF)

# g) R^2 and adjusted R^2 from summary
# Akaike Information Criterion
AIC(regE)
AIC(regF)
# additionally: as in lecture slides
log(sum(regE$residuals^2)/nrow(sal)) + 2*length(coef(regE))/nrow(sal) # for regE
log(sum(regF$residuals^2)/nrow(sal)) + 2*length(coef(regF))/nrow(sal) # for regF

# Schwarz Criterion
BIC(regE)
BIC(regF)
# additionally: as in lecture slides
log(sum(regE$residuals^2)/nrow(sal)) + length(coef(regE))*log(nrow(sal))/nrow(sal) # for regE
log(sum(regF$residuals^2)/nrow(sal)) + length(coef(regF))*log(nrow(sal))/nrow(sal) # for regF

# h) number of female managers
length(sal$GENDER[sal$GENDER == 0 & sal$MANAGER == 1])