################################################################################
#                                                                              #
#           Exercise Sheet 5: The Multiple Regression Model                    # 
#                           Assignment 3                                       #
#                                                                              #
################################################################################

##### Assignment 3: House Prices in the Multiple Regression Model #####

# load data
houses <- read.csv('houseprices2.csv')

# data exploration
summary(houses) # data summary
cor(houses) # data correlation matrix
# scatterplot price vs. sqft
plot(PRICE ~ SQFT, data = houses)
# scatterplot price vs. age
boxplot(PRICE ~ AGE, data = houses)

# a) model 1
regA <- lm(PRICE ~ SQFT + AGE, data = houses)
summary(regA)

# b) 95% confidence interval
# get coefficients
co <- coef(summary(regA))
# calculate upper and lower bound 
up <- co[2,1] + qt(0.975, regA$df.residual)*co[2,2]
low <- co[2,1] - qt(0.975, regA$df.residual)*co[2,2]

# c) t-Test: H_0: beta_3 >=-1000
# calculate t-Statistic
tstatA <- (co[3,1] - (-1000))/co[3,2]
# calculate the p-value, left-sided
pValA <- pt(tstatA, df = regA$df.residual)

# d) model 2: include squared terms
regD <- lm(PRICE ~ SQFT + I(SQFT^2) + AGE + I(AGE^2), data = houses)
summary(regD)
# get coefficients
coD <- coef(summary(regD))
# marginal effects sqft
effectSqftMin <- coD[2,1] + 2*coD[3,1]*min(houses$SQFT)
effectSqftMax <- coD[2,1] + 2*coD[3,1]*max(houses$SQFT)
effectSqft2000 <- coD[2,1] + 2*coD[3,1]*2000

# e) marginal effects age
effectAgeMin <- coD[4,1] + 2*coD[5,1]*min(houses$AGE)
effectAgeMax <- coD[4,1] + 2*coD[5,1]*max(houses$AGE)
effectAge25 <- coD[4,1] + 2*coD[5,1]*25

# f) 95% confidence interval for a house with 2000 sqft
covD <- vcov(regD) # variance - covariance matrix of the coefficients
# calculate standar derror of the linear combination of coefficients
seSqft2000 <- sqrt(covD[2,2] + 4000^2*covD[3,3] + 2*4000*covD[2,3])
# calculate upper and lower bound
upD <- effectSqft2000 + qt(0.975, regD$df.residual)*seSqft2000
lowD <- effectSqft2000 - qt(0.975, regD$df.residual)*seSqft2000

# g) t-Test H_O: effect >=-1000 at age=25
# calculate standard error of the linear combination of coefficients
seAge25 <- sqrt(covD[4,4] + (2*25)^2*covD[5,5] + 2*(2*25)*covD[4,5])
# calculate t-Statistic: H_0: beta_4 + 2*beta_5*25 >= -1000
tstatD <- (effectAge25 - (-1000))/seAge25
# calculate the p-value, left-sided
pValD <- pt(tstatD, df = regD$df.residual)


# Extra: graphic evaluation of models

# Fitted graphs
sqft <- seq(20, 8000, 1)
age <- seq(0, 80, 0.5)
fittedValsSQFT <- regD$coefficients[1] + regD$coefficients[2]*sqft + regD$coefficients[3]*sqft^2
fittedValsAGE <- regD$coefficients[1] + regD$coefficients[4]*age + regD$coefficients[5]*age^2

plot(PRICE ~ SQFT, data = houses)
lines(fittedValsSQFT ~ sqft, col = "red")

plot(PRICE ~ AGE, data = houses)
lines(fittedValsAGE ~ age, col = "red")

# residual plots
# model 1
plot(regA$residuals ~ houses$SQFT, main = "Residuals Model 1")
abline(h=0)

# model 2
plot(regD$residuals ~ houses$SQFT, main = "Residuals Model 2")
abline(h=0)

plot(regD$residuals ~ houses$AGE, main = "Residuals Model 2")
abline(h=0)

