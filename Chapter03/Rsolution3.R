################################################################################
#                                                                              #
#            Übungsblatt 3: Intervallschätzung und Hypothesentests             # 
#                           Aufgabe 3                                          #
#                                                                              #
################################################################################

##### Aufgabe 3: Lebensversicherung #####

# Daten einlesen
insurance <- read.csv("C:/Users/leasi/Desktop/Übung2020/Übung3/insur.csv")

summary(insurance)

# a) 
linMod <- lm(INSURANCE ~ INCOME, data = insurance)
summary(linMod)

# b) H_0: beta_2 = 5 
# get coefficients and standard errors from the Model
co <- coef(summary(linMod))
# calculate t-Statistic
tstatA <- (co[2,1] - 5)/co[2,2]
# calculate critical value alpha=0.05
tcrit <- qt(0.975, linMod$df.residual)
# calculate the p-value, two-sided
pValA <- 2*pt(abs(tstatA), df = linMod$df.residual, lower.tail = FALSE)

# c) H_0: beta_2 = 1
# calculate t-Statistic
tstatB <- (co[2,1] - 1)/co[2,2]
# calculate the p-value, two-sided
pValB <- 2*pt(abs(tstatB), df = linMod$df.residual, lower.tail = FALSE)
