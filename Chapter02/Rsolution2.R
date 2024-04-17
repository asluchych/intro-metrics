################################################################################
#                                                                              #
#            Exercise Sheet 2: Simple Linear Regression Model                  # 
#                       Assignments 3 and 5                                    #
#                                                                              #
################################################################################

##### Assignment 3: House Prices #####

# Load data
houses <- read.csv('houseprices.csv')

# Exploration
summary(houses)
plot(PRICE ~ SQM, data = houses)
cor(houses$PRICE, houses$SQM)

# a) Linear regression with OLS
linModA <- lm(PRICE ~ SQM, data = houses)
summary(linModA)

# Add regression line to scatterplot
plot(PRICE ~ SQM, data = houses)
abline(linModA, col="red")

# c) Predict price of a house of size 200 m^2
price_200 <- predict(linModA, data.frame(SQM = c(200)))
print(price_200) # Output value in the console

# d) Scaling
houses$priceScaled <- houses$PRICE/1000 # price in 1000$ 
# Linear regression with rescaled price
linModC <- lm(priceScaled ~ SQM, data = houses)
summary(linModC)
# Prediction
priceScaled_200 <- predict(linModC, data.frame(SQM = c(200)))
# Prediction in dollars
priceInDollar_200 <- priceScaled_200 * 1000 

##### Assignment 5: Estimation Uncertainty #####

# Load data
random <- read.csv('random.csv')

# a) Linear model y1 ~ x
regA <- lm(Y1 ~ X, data = random)
summary(regA)
# (i) Covariance matrix of estimators
vcov(regA)
# (ii) Model without intercept
regNoInt <- lm(Y1 ~ -1 + X, data = random)
summary(regNoInt)

# b) Linear model y2 ~ x
regB <- lm(Y2 ~ X, data = random)
summary(regB)

# two scatterplots in one plot
plot(Y1 ~ X, data = random, col = "blue",  xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100), main='Scatterplot') 
par(new = TRUE) 
plot(Y2 ~ X, data = random, col = "red", xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100)) 
abline(a = 1, b = 2, col = "green") 
legend("bottomright", legend=c('Y1', 'Y2', 'True Model'),
       col=c('blue', 'red', 'green'), pch = c(1, 1, NA_integer_),
       lty = c(0, 0, 1)) 


# c) Reduce variation of X
# (i) by changing scale of X
random$xScaled <- random$X/10
# Re-estimate model from b)
regScaledX <- lm(Y2 ~ xScaled, data = random)
summary(regScaledX)
# (ii) by reducing sample size
regSmallSample <- lm(Y2 ~ X, data = random[1:100,])
summary(regSmallSample)

# d) Inverse regression
# (1) to regression in a)
regInvA <- lm(X ~ Y1, data = random)
summary(regInvA)
# (2) to regression in b)
regInvB <- lm(X ~ Y2, data = random)
summary(regInvB)
