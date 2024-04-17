################################################################################
#                                                                              #
#                         t - tests                                            #
#                                                                              #
################################################################################

# Set significance level for test decision, Sample Size and Number of Draws
alpha <- 0.05
nSim <- 10000
sampleSize <- 500

# Empty vector to store local test decisions
tStats <- numeric(length = nSim)
pVals  <- numeric(length = nSim)
testDecision <- numeric(length = nSim)

# Loop to simulate "nSim" samples from a standard normal distribution
for (i in 1:nSim){
  # draw a sample with "sampleSize" observations
  set.seed(i)  # note that the seed has to change with every repetition
  sample <- rnorm(sampleSize)  # default setting of rnorm are mean = 0 and sd = 1
  
  # compute the test statistic to test if the mean is different from zero
  tStat <- (mean(sample) - 0)/(sd(sample)/sqrt(sampleSize))
  
  # Compute the p-value of a two-sided test
  pVal <- 2*(1-pt(abs(tStat), sampleSize - 1))
  
  # Store the test decision in the vector testDecision
  tStats[i] <- tStat
  pVals[i] <- pVal
  testDecision[i] <- ifelse(pVal <= alpha, 1, 0)  # 1 if condition is true
}

# Compute relative frequencies of test outcomes
relOutcomes <- table(testDecision)/length(testDecision)
names(relOutcomes) <- c("H0", "H1")

# Plot relative frequency of test outcomes
barplot(relOutcomes, ylim = c(0,1), main = "Relative Frequency of H0 and H1",
        xlab = "Test Result", col = "lightblue")
relOutcomes

# Histogram with t-Stat and t-Distribution
xVals <-  seq(min(tStats), max(tStats), 0.01) # nessesary to plot t-Dist
hist(tStats, main = "Histogram of t-Statistics", probability = T) #Histogram
lines(xVals, dt(xVals, sampleSize - 1), col = "red", lwd = 2) # Density t-Distribution
abline(v = qt(1 - alpha/2, sampleSize - 1), lwd = 2, col = "darkred", lty = "dashed")
abline(v = - qt(1 - alpha/2, sampleSize - 1), lwd = 2, col = "darkred", lty = "dashed")
# lines(xVals, dnorm(xVals),col = "blue", lwd = 2) # Density std-Normal Distribution

# Histogram for p-Values
hist(pVals, nclass = 20, main = "Histogram with p-Values")
abline(v = alpha, col = "red", lwd = 2)



################################################################################
#                                                                              #
#                  Confidence Interval                                         #
#                                                                              #
################################################################################
# Set significance level for test decision
alpha <- 0.10

# Empty vector to store local test decisions
nSim <- 10000
sampleSize <- 500
overlap <- numeric(length = nSim)
upperBound <- numeric(length = nSim)
lowerBound  <- numeric(length = nSim)

# Loop to simulate 10000 samples from a standard normal distribution
for (i in 1:nSim){
  # draw a sample with 500 observations
  set.seed(i)  # note that the seed has to change with every repetition
  sample <- rnorm(sampleSize)  # default setting of rnorm are mean = 0 and sd = 1
  
  # compute the confidence interval for the mean
  up <- mean(sample) + qt(1 - alpha/2, sampleSize - 1)*(sd(sample)/sqrt(sampleSize))
  low <- mean(sample) - qt(1 - alpha/2, sampleSize - 1)*(sd(sample)/sqrt(sampleSize))
  
  # does the confidence interval overlap the true value mu = 0?
  # overlap take value 1 if confidence interval overlaps the true value
  overlap[i] <- ifelse(low <= 0 & 0 <= up, 1, 0)  
  upperBound[i] <- up
  lowerBound[i] <- low
}

# Compute relative frequencies of test outcomes
relOutcomes <- table(overlap)/length(overlap)
names(relOutcomes) <- c("No", "Yes")

# Plot relative frequency of test outcomes
barplot(relOutcomes, ylim = c(0,1), 
        main = "Relative Frequency Overlaping Intervals",
        xlab = "", col = "lightblue")
relOutcomes

data <- data.frame(lowerBound, upperBound)
newData <- data[order(lowerBound),]

plot(NULL, xlim=c(min(lowerBound),max(upperBound)), ylim=c(0,nSim), ylab="", 
     xlab="Confidence Intervals")
for (i in 1:nSim) {
  segments(x0 = newData[i, 1], y0 = i, x1 = newData[i, 2], y1= i, 
           col = "steelblue")  
}
abline(v = 0)