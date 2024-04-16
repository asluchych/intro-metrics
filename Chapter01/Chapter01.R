# Assignment 4: Simulations
# a) Generate 1500 observations for an independent standard normally distributed 
# random variable, X ∼ N (0, 1)

set.seed(321)
X <- rnorm(1500, 0, 1)

# b) Generate the histogram and descriptive statistics of X.

hist(X)
summary(X)

# c) Generate the random variable Y , where Y = 5 + 3X, and its histogram and 
# descriptive statistics.

Y <- 5 + 3*X

hist(Y)
summary(Y)


# d) Calculate the expected value and variance of Y and compare them with the 
# arithmetic mean and the sample variance from c).

mean(Y)
var(Y)

# e) Restrict the sample to only use the first (i) 10 and (ii) 100 observations
# and repeat c).Compare the results.


hist(Y[1:10])
summary(Y[1:10])


hist(Y[1:100])
summary(Y[1:100])

# Assignment 5*: Data
# The dataset grades.wf1/.csv contains the following variables. A sample of ten randomly
# selected Vanderbilt University students is considered.

grades <- read.csv('grades.csv')

# a) Calculate the arithmetic mean, median, and standard deviation of 
# the sample for each of the three variables.

summary(grades)

sd(grades$satm)
sd(grades$fgpa)
sd(grades$fem)

# b) Calculate the sample covariance and correlation coefficients between the three variables

cov(grades)
cor(grades)

# c) Create histograms and scatterplots for the three variables.

hist(grades$satm)
hist(grades$fgpa)
hist(grades$fem)

plot(grades$fgpa, grades$satm)
plot(grades$fem, grades$fgpa)
plot(grades$fem, grades$satm)

# d) Compare the results from a) and b) with the results in c).

# e) Calculate the conditional mean of fgpa and satm for the six female students

mean(grades$fgpa[grades$fem==1])
mean(grades$satm[grades$fem==1])
