################################################################################
#                                                                              #
#            Exercise Sheet 2: Simple Linear Regression Model                  # 
#                       Additional Assignment                                  #
#                                                                              #
################################################################################

# Load data, using read.csv
food <- read.csv('food.csv')

# Exploration
summary(food)
plot(FOOD_EXP ~ INCOME, data = food)
cor(food$FOOD_EXP, food$INCOME)

# a) Linear regression with OLS
linModA <- lm(FOOD_EXP ~ INCOME, data = food)
summary(linModA)

# Add regression line to scatterplot
plot(FOOD_EXP ~ INCOME, data = food)
abline(linModA, col="red")

# c) Food expenditure of a household with income 1500$ 
83.42 + 10.21*15

# d) Scaling
food$INCOMEScaled <- food$INCOME*100 # income in $ 
# Linear regression with rescaled price
linModC <- lm(FOOD_EXP ~ INCOMEScaled, data = food)
summary(linModC)
# Prediction
83.42 + 0.1021*1500
