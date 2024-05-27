################################################################################
#                                                                              #
#           Exercise Sheet 5: The Multiple Regression Model                    # 
#                           Assignment 3                                       #
#                                                                              #
################################################################################

##### Assignment 3: House Prices in the Multiple Regression Model #####

# load data
houses <- read.csv("houseprices2.csv")

# data exploration
summary(houses) # data summary
cor(houses) # data correlation matrix
# scatterplot price vs. sqft
plot(PRICE ~ SQFT, data = houses)
# scatterplot price vs. age
plot(PRICE ~ AGE, data = houses)

# a) model 1
regA <- lm(PRICE ~ SQFT + AGE, data = houses)
summary(regA)

# b) 95% Confidence Interval
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
regB <- lm(PRICE ~ SQFT + I(SQFT^2) + AGE + I(AGE^2), data = houses)
summary(regB)
# get coefficients
coB <- coef(summary(regB))
# e) marginal effects
effectSqftMin <- coB[2,1] + 2*coB[3,1]*min(houses$SQFT)
effectSqftMax <- coB[2,1] + 2*coB[3,1]*max(houses$SQFT)
effectSqft2000 <- coB[2,1] + 2*coB[3,1]*2000

# f) marginal effects
effectAgeMin <- coB[4,1] + 2*coB[5,1]*min(houses$AGE)
effectAgeMax <- coB[4,1] + 2*coB[5,1]*max(houses$AGE)
effectAge25 <- coB[4,1] + 2*coB[5,1]*25

# g) 95% confidence interval for a house with 2000 sqft
covB <- vcov(regB) # variance - covariance matrix of the coefficients
# calculate standar derror of the linear combination of coefficients
seSqft2000 <- sqrt(covB[2,2] + 4000^2*covB[3,3] + 2*4000*covB[2,3])
# calculate upper and lower bound
upB <- effectSqft2000 + qt(0.975, regB$df.residual)*seSqft2000
lowB <- effectSqft2000 - qt(0.975, regB$df.residual)*seSqft2000

# f) t-Test H_O: effect >=-1000 at age=25
# calculate standarderror of the linear combination of coefficients
seAge25 <- sqrt(covB[4,4] + (2*25)^2*covB[5,5] + 2*(2*25)*covB[4,5])
# calculate t-Statistic: H_0: beta_4 + 2*beta_5*25 >= -1000
tstatB <- (effectAge25 - (-1000))/seAge25
# calculate the p-value, left-sided
pValB <- pt(tstatB, df = regB$df.residual)


# Extra: graphic evaluation of models

# Fitted graphs
sqft <- seq(20, 8000, 1)
age <- seq(0, 80, 0.5)
fittedValsSQFT <- regB$coefficients[1] + regB$coefficients[2]*sqft + regB$coefficients[3]*sqft^2
fittedValsAGE <- regB$coefficients[1] + regB$coefficients[4]*age + regB$coefficients[5]*age^2

plot(PRICE ~ SQFT, data = houses)
lines(fittedValsSQFT ~ sqft, col = "red")

plot(PRICE ~ AGE, data = houses)
lines(fittedValsAGE ~ age, col = "red")

# residual plots
# model 1
plot(regA$residuals ~ houses$SQFT, main = "Residuals Model 1")
abline(h=0)

# model 2
plot(regB$residuals ~ houses$SQFT, main = "Residuals Model 2")
abline(h=0)

plot(regB$residuals ~ houses$AGE, main = "Residuals Model 2")
abline(h=0)

