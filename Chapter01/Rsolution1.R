################################################################################
#                                                                              #
#            Exercise Sheet 1: Statistical Fundamentals                        # 
#                       Assignments 4 and 5                                    #
#                                                                              #
################################################################################

##### Assignment 4: Simulations #####

# a) Generate random variable X
set.seed(321)
X <- rnorm(1500, 0, 1)

# b) Histogram and descriptive statistics
hist(X)
summary(X)
sd(X)


# c) Generate random variable Y
Y <- 5 + 3*X
# Histogram and descriptive statistics
hist(Y)
summary(Y)
sd(Y)

# d) Arithmetic mean and  sample variance
mean(Y)
var(Y)

# e) Restrict sample
# First 10 observations
hist(Y[1:10])
summary(Y[1:10])
sd(Y[1:10])
# First 100 observations
hist(Y[1:100])
summary(Y[1:100])
sd(Y[1:100])

##### Assignment 5: Data #####

# Load data
grades <- read.csv('grades.csv')

# a) Descriptive statistics
summary(grades)
sd(grades$satm)
sd(grades$fgpa)
sd(grades$fem)

# b) Covariance and correlation
cov(grades)
cor(grades)

# c) Histograms and scatterplots
hist(grades$satm)
hist(grades$fgpa)
hist(grades$fem)

plot(grades$satm, grades$fgpa)
plot(grades$fem, grades$fgpa)
plot(grades$fem, grades$satm)

# d) Comparison

# e) Conditional mean
mean(grades$fgpa[grades$fem==1])
mean(grades$satm[grades$fem==1])
