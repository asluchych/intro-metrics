################################################################################
#                                                                              #
#                   Exercise Sheet 9: Autocorrelation                          # 
#                                  Assignment 2                                #
#                                                                              #
################################################################################
# necessary packages: if not installed, type
# install.packages(c('lmtest', 'sandwich', 'nlme', 'orcutt')) in the console and
# press Enter

library(lmtest) # for functions: bgtest, NeweyWest
library(sandwich) # for function: coeftest
library(nlme) # for function: gls
library(orcutt) # for function: cochrane.orcutt

##### Assignment 2: Job Vacancies #####

# Load data 
vacancies <- read.csv('vacancies.csv')

# a) model a
regA <- lm(log(jv) ~ log(u), data = vacancies)
summary(regA)
# 95% confidence interval for beta_2
# get coefficients
co_ols <- coef(summary(regA))
# calculate upper and lower bound 
ls_up <- co_ols[2,1] + qt(0.975, regA$df.residual)*co_ols[2,2]
ls_low <- co_ols[2,1] - qt(0.975, regA$df.residual)*co_ols[2,2]
c(ls_low, ls_up)

# b) correlogram of residuals
acf(regA$residuals)
pacf(regA$residuals)

# c) Breusch-Godfrey Test
# save residuals to use as dependent variable in the auxiliary regression
vacancies$resid <- regA$residuals
# save lagged residuals to use as explanatory variable in auxiliary regression
vacancies$resid_lag <- c(0, regA$residuals[1:nrow(vacancies)-1]) # as default in
# EViews, missing value lagged residuals are set to zero			
# fit auxiliary regression
aux <- lm(resid ~ log(u) + resid_lag, data = vacancies)
summary(aux)
# save summary of the auxiliary regression
sumAux <- summary(aux)
# calculate test statistic - LM test: N*R^2
bg <- nrow(vacancies)*sumAux$r.squared
# calculate p-value ->  LM test is chi-squared distributed
pVal <- 1 - pchisq(bg, 1)
# alternatively, use bgtest function from package lmtest
bgtest(regA, order = 1)

# d) OLS with HAC standard errors
# 95% confidence interval for beta_2
# get coefficients
co_hac <- coeftest(regA, vcov = NeweyWest(regA, lag = 3, prewhite = FALSE, 
                                          adjust = TRUE)) # as default in EViews
co_hac
# calculate upper and lower bound 
hac_up <- co_hac[2,1] + qt(0.975, regA$df.residual)*co_hac[2,2]
hac_low <- co_hac[2,1] - qt(0.975, regA$df.residual)*co_hac[2,2]
c(hac_low, hac_up)

# e) # assumption: errors follow AR(1) error model, Generalized Least Squares 
# Estimation via Model Transformation
# estimate rho via Cochrane-Orcutt Procedure
rho <- cochrane.orcutt(regA)$rho
# load data 
vac_new <- read.csv('vacancies.csv')
# log transformation
vac_new <- log(vac_new)
# lagged log(jv)
vac_new$jv_lag <- c(NA, vac_new$jv[1:nrow(vac_new)-1]) 
# lagged log(u)
vac_new$u_lag <- c(NA, vac_new$u[1:nrow(vac_new)-1]) 
# new dependent variable
vac_new$y <- vac_new$jv - rho*vac_new$jv_lag 
# new constant instead of 1s
vac_new$c <- rep(1 - rho, nrow(vac_new))
# new explanatory variable
vac_new$x <- vac_new$u - rho*vac_new$u_lag
# fit the model
regE <- lm(y ~ -1 + c + x, data = vac_new)
summary(regE)

# correlogram of residuals
acf(regE$residuals)
pacf(regE$residuals)

# 95% confidence interval for beta_2
# get coefficients
co_glm <- coef(summary(regE))
# calculate upper and lower bound 
glm_up <- co_glm[2,1] + qt(0.975, regA$df.residual)*co_glm[2,2]
glm_low <- co_glm[2,1] - qt(0.975, regA$df.residual)*co_glm[2,2]
c(glm_low, glm_up)
c(hac_low, hac_up)
c(ls_low, ls_up)