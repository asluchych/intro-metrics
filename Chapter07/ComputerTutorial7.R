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
boxplot(SALARY ~ EDUC, data = sal)
boxplot(SALARY ~ GENDER, data = sal)
boxplot(SALARY ~ MANAGER, data = sal)

# b) model 1
regA <- lm(log(SALARY) ~ EDUC + GENDER, data = sal)
summary(regA)

# c) F-Test in summary(regA)

# d) H_0: beta_3 = 2*beta_2
# define auxiliary variable for restricted  model: x = EDUC+2*GENDER
sal$x <- sal$EDUC + 2*sal$GENDER
regRestricted <- lm(log(SALARY) ~ x, data = sal)
summary(regRestricted)
# Calculate SSE_R und SSE_U
sseR <- sum((regRestricted$residuals - mean(regRestricted$residuals))^2)
sseU <- sum((regA$residuals - mean(regA$residuals))^2)
# F-statstic and p-vale
fStat <- ((sseR - sseU)*regA$df.residual)/(sseU*1)
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

# g) R^2 und adjusted R^2 from summary
# Akaike Information Criterion
AIC(regE)
AIC(regF)
# Schwarz Criterion
BIC(regE)
BIC(regF)

# h) number of female managers
length(sal$GENDER[sal$GENDER == 0 & sal$MANAGER == 1])
# comment