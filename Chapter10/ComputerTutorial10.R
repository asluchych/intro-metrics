################################################################################
#                                                                              #
#     Exercise Sheet 10: Random Regressors and Moment-Based Estimation         # 
#                                       Assignment 4                           #             
#                                                                              #
################################################################################
# necessary packages: if not installed, type 
# install.packages(c('lmtest', 'sandwich', 'nlme', 'car', 'AER')) in the console
# and press Enter
library(lmtest) # for functions: bptest, vcovHC
library(sandwich) # for function: coeftest
library(car) # for function: lht (Test Linear Hypothesis)
library(AER) # for function: ivreg (Two Staged Least Squares)

##### Assignment 4: Inflation #####

# Load data
inf <- read.csv('inf.csv')

# a) model 1
regA <- lm(INFLAT ~ MONEY + OUTPUT, data = inf)
summary(regA)
# hypothesis (i)
lht(regA, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"))
# hypothesis (ii)
lht(regA, c("MONEY = 1", "OUTPUT = -1"))

# take into account potential heteroskedasticity
coeftest(regA, vcov. = vcovHC(regA, type = "HC1"))
# hypothesis (i)
lht(regA, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov. = vcovHC(regA, type = "HC1"))
# hypothesis (ii)
lht(regA, c("MONEY = 1", "OUTPUT = -1"), 
    vcov. = vcovHC(regA, type = "HC1"))

# b) Two-Stage Least Squares
# First stage: regress the endogenous variable on all exogenous and instrumental
# variables:
I_2SLS <- lm(OUTPUT ~ MONEY + INITIAL + SCHOOL + INV + POPRATE, data = inf)
summary(I_2SLS)
# Get fitted value for variable OUTPUT
inf$OUTPUT_FITTED <- I_2SLS$fitted.values
# Second stage: estimate model of interest by replacing exogenous variable by 
# its fitted value
II_2SLS <- lm(INFLAT ~ MONEY + OUTPUT_FITTED, data = inf)
summary(II_2SLS)
coeftest(II_2SLS, vcov. = vcovHC(II_2SLS, type = "HC1"))

# hypothesis (i)
lht(II_2SLS, c("(Intercept) = 0", "MONEY = 1", "OUTPUT_FITTED = -1"), 
    vcov. = vcovHC(II_2SLS, type = "HC1"))
# hypothesis (ii)
lht(II_2SLS, c("MONEY = 1", "OUTPUT_FITTED = -1"), 
    vcov. = vcovHC(II_2SLS, type = "HC1"))

# Alternatively, use function ivreg from package AER to perform
# Two-Stage Least Squares
regB <- ivreg(INFLAT ~ MONEY + OUTPUT | MONEY + INITIAL + SCHOOL + INV + POPRATE, 
              data = inf)
summary(regB)
# take into account potential heteroskedasticity
coeftest(regB, vcov. = vcovHC(regB, type = "HC1"))

# hypothesis (i)
lht(regB, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov. = vcovHC(regB, type = "HC1"))
# hypothesis (ii)
lht(regB, c("MONEY = 1", "OUTPUT = -1"), 
    vcov. = vcovHC(regB, type = "HC1")