################################################################################
#                                                                              #
#                           Recap Chapters 1-5                                 # 
#                             Assignment 1                                     #
#                                                                              #
################################################################################

##### Assignment 1: Wages #####

# load data
wages <- read.csv('wages.csv')

# a) scatterplots
# scatterplot  wage vs. educ
plot(WAGE ~ EDUC, data = wages)
# scatterplot  wage vs. age
plot(WAGE ~ AGE, data = wages)

# b) model 1
linModA <- lm(WAGE ~ EDUC + AGE, data = wages)
summary(linModA)

# c) 95% confidence interval
# get coefficients
co <- coef(summary(linModA))
# calculate upper and lower bound 
up <- co[2,1] + qt(0.975, linModA$df.residual)*co[2,2]
low <- co[2,1] - qt(0.975, linModA$df.residual)*co[2,2]

# d)
# prediction
pred_med <- co[1, 1] + co[2, 1]*median(wages$EDUC) + co[3, 1]*median(wages$AGE)
# alternatively
pred_med2 <- predict(linModA, newdata = data.frame(EDUC = median(wages$EDUC), AGE = median(wages$AGE)))

# e)
wages$wageScaled <- wages$WAGE/10 # wage in 10$ 
# linear regression with rescaled wage
linModE <- lm(wageScaled ~ EDUC + AGE, data = wages)
summary(linModE)

# f) t-Test: H_0: beta_2 =< 2, H_1: beta_2 > 2
# calculate critical value
critValA <- qt(0.95, df = linModA$df.residual)
# calculate t-Statistic
tstatA <- (co[2,1] - 2)/co[2,2]
# calculate the p-value, left-sided
pValA <- 1 - pt(tstatA, df = linModA$df.residual)

# h) model 2: include squared terms
linModG <- lm(WAGE ~ EDUC + I(EDUC^2) + AGE + I(AGE^2), data = wages)
summary(linModG)
# get coefficients
coG <- coef(summary(linModG))
# marginal effects education
effectEducMin <- coG[2,1] + 2*coG[3,1]*min(wages$EDUC)
effectEducMed <- coG[2,1] + 2*coG[3,1]*median(wages$EDUC)
effectEducMax <- coG[2,1] + 2*coG[3,1]*max(wages$EDUC)
# marginal effects age
effectAgeMin <- coG[4,1] + 2*coG[5,1]*min(wages$AGE)
effectAgeMed <- coG[4,1] + 2*coG[5,1]*median(wages$AGE)
effectAgeMax <- coG[4,1] + 2*coG[5,1]*max(wages$AGE)