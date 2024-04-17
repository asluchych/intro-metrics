library(tseries)

# Quick Illustration of the Simulation

# Values for the Exogenous Variable
xVals <- seq(0, 10, 0.1)

# Deterministic part of the Model with an intercept 1 and a slope of 2
det <- 1 + 2*xVals

# Simulate the stochastic error terms from a uniform distribution
set.seed(1)
noise <- runif(101, min = -2.5, max = 2.5)

# Plot of the simulated errors
plot(noise, type = "l", main = "Realizations of Simulated Errors")
abline(h = 2.5, col = "blue", lty = "dotted")
abline(h = -2.5, col = "blue", lty = "dotted")
grid()

hist(noise)

# Create Observations of the endogenous variable as sum of deterministic and 
# stochastic parts
yVals <- det + noise

# Plot the observations of the endogenous and exogenous variable 
# plus the deterministic part
plot(yVals ~ xVals, type = "p", main = "Realizations of the Simulated Model")
abline(a = 1, b = 2, col = "red", lwd = 2, lty = "dotted")  # deterministic part
grid()
legend("topleft", legend = c("Deterministic Part"), 
       col = "red", lwd = 2, lty = "dotted")



################################################################################
#                                                                              #
#                     Asymptotic Normality of Coefficients                     #
#                                                                              #
################################################################################

nSim <- 10000  # Number of Simulations 
nObs <- 30  # Number of Observations

intercept <- numeric(length = nSim)

slope <- numeric(length = nSim)

for (i in 1:nSim){
  
  # Values for the Exogenous Variable
  xVals <- seq(0, 10, 10/(nObs-1))
  
  # Deterministic part of the Model with an intercept 1 and a slope of 2
  det <- 1 + 2*xVals
  
  # Simulate the stochastic error terms from a uniform distribution
  set.seed(i)
  noise <- runif(nObs, min = -2.5, max = 2.5)
  
  # Create Observations of the endogenous variable as sum of deterministic and 
  # stochastic parts
  yVals <- det + noise
  
  # Estimate linear Model
  linMod <- lm(yVals ~ xVals)  
  
  # Store the coefficients
  intercept[i] <- linMod$coefficients[1]  # Intercept
  
  slope[i] <- linMod$coefficients[2]  # Slope
}

# Plot Histograms with results

# Range of the estimated coefficients
rangeInt <- seq(min(intercept), max(intercept), 0.01)
rangeSlope <- seq(min(slope), max(slope), 0.01)

# Plot of Intercept
hist(intercept, main = "Histogram of the estimated Intercept", 
     col = "lightblue", probability = TRUE)
lines(dnorm(rangeInt, mean = mean(intercept), sd = sd(intercept)) ~ rangeInt, 
      col = "red", lwd = "2")

# Plot of Slope Coefficient
hist(slope, main = "Histogram of the estimated Slope", col = "lightblue",
     probability = TRUE)
lines(dnorm(rangeSlope, mean = mean(slope), sd = sd(slope)) ~ rangeSlope, 
      col = "red", lwd = "2")

# Test for Normality using Jarque Bera Test

jarque.bera.test(intercept)  # Intercept

jarque.bera.test(slope)  # Slope