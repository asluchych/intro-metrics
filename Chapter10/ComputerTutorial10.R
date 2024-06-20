################################################################################
#                                                                              #
#     Exercise Sheet 10: Random Regressors and Moment-Based Estimation         # 
#                                       Assignment 4                           #             
#                                                                              #
################################################################################
# necessary packages: if not installed, type install.packages(c('lmtest', 'sandwich', 'nlme', 'car', 'AER')) 
# in the console and press Enter
library(lmtest) #for functions: bptest, vcovHC
library(sandwich) #for function: coeftest
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
# heteroskedasticity
coeftest(regA, vcov = vcovHC(regA, type = "HC1"))
# hypothesis (i)
lht(regA, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regA, type = "HC1"))
# hypothesis (ii)
lht(regA, c("MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regA, type = "HC1"))

# b) Two Staged Least Squares
regB <- ivreg(INFLAT ~ MONEY + OUTPUT | MONEY + INITIAL + SCHOOL + INV + POPRATE, 
              data = inf)
summary(regB, vcov. = sandwich) # with vcov.=sandwich potential heteroscedasticity taken into account
# hypothesis (i)
lht(regB, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regB, type = "HC1"))
# hypothesis (ii)
lht(regB, c("MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regB, type = "HC1"))