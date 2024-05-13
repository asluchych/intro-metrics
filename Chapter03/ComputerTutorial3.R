################################################################################
#                                                                              #
#            Exercise Sheet 3: Interval Estimation and Hypothesis Testing      # 
#                           Assignment 3                                       #
#                                                                              #
################################################################################

##### Assignment 3: Life Insurance #####

# Load data
insurance <- read.csv('insur.csv')

summary(insurance)

# a) 
linMod <- lm(INSURANCE ~ INCOME, data = insurance)
summary(linMod)

plot(INSURANCE ~ INCOME, data = insurance,  xlab='income', ylab='insurance',
     main='Scatterplot and regression line') 
abline(linMod, col = 'blue')


      
# b) H_0: beta_2 = 5 
# get coefficients and standard errors from the model
co <- coef(summary(linMod))
# calculate critical value alpha=0.05
tcrit <- qt(0.975, linMod$df.residual)
# calculate t-statistic
tstatA <- (co[2,1] - 5)/co[2,2]
# calculate the p-value, two-sided
pValA <- 2*pt(abs(tstatA), df = linMod$df.residual, lower.tail = FALSE)

# c) H_0: beta_2 = 1
# calculate t-statistic
tstatB <- (co[2,1] - 1)/co[2,2]
# calculate the p-value, two-sided
pValB <- 2*pt(abs(tstatB), df = linMod$df.residual, lower.tail = FALSE)


# Extra: LS estimates
x <- insurance$INCOME
y <- insurance$INSURANCE

# Point estimates
b2 <- cov(x, y)/var(x)
b1 <- mean(y) - b2*mean(x)

# Point predictions
n <- nrow(insurance)
y.hat <- rep(NA, n)
for (i in seq(n)) {
  y.hat[i] <- b1 + b2*x[i]
}

# Standard error of regression
sse <- sum((y - y.hat)^2)
var.reg <- sse/(n - 2)
se.reg <- sqrt(var.reg)


# Standard error of estimates
se.b2 <- sqrt(var.reg/sum((x - mean(x))^2))
se.b1 <- sqrt(var.reg * sum(x^2)/(n*sum((x - mean(x))^2)))
cov.b1.b2 <- var.reg*(-mean(x))/sum((x - mean(x))^2)

vcov(linMod)

# Extra: standard normal vs t-distribution
x <- seq(-3, 3, length.out = 100)
plot(x, dnorm(x), col = 'blue', type = 'l')
points(x, dt(x, 1),  col = 'red', type = 'l')
points(x, dt(x, 5),  col = 'yellow', type = 'l')
points(x, dt(x, 20),  col = 'black', type = 'l')
points(x, dt(x, 50),  col = 'green', type = 'l')



