################################################################################
#                                                                              #
#                   Exercise Sheet 8: Heteroskedasticity                       # 
#                                  Assignment 2                                #
#                                                                              #
################################################################################
# necessary packages: if not installed, type install.packages(c('lmtest', 'sandwich') 
# in the console and press Enter

library(lmtest) # for function: bptest, vcovHC
library(sandwich) # for function: coeftest

##### Assignment 2: Travel #####

# Load data 
travel <- read.csv('travel.csv')

# a) model 1
regA <- lm(MILES ~ INCOME + AGE + KIDS, data = travel)
summary(regA)

# b) scatterplots
# residuals graph
# age
plot(regA$residuals ~ travel$AGE, main = 'Age')
abline(h=0)

# Kids
plot(regA$residuals ~ travel$KIDS, main = 'Kids')
abline(h=0)

# Income
plot(regA$residuals ~ travel$INCOME, main = 'Income')
abline(h=0)

# c) Breusch-Pagan Test with INCOME as  explanatory variable

# speichere die Residuen zum Quadrat als abh?ngige Variable der Hilfsregression
resid2 <- regA$residuals^2
# F?hre die Hilfsregression durch
aux <- lm(resid2 ~ travel$INCOME + travel$AGE)
# speichere die Summary von der Hilfsregression
sumAux <- summary(aux)
# Berechne die Test Statistik - LM Test -> N*R^2
bp <- 200*sumAux$r.squared
# Berechne den p-Wert -> eine LM Test Statistik ist X^2 verteilt
pVal <- pchisq(bp, 2, lower.tail = FALSE)

# Es gibt auch ein package f?r den Breusch-Pagan Test
bptest(regA, varformula = ~ INCOME + AGE, data = travel)

# c) Scatter Plots


# d) M?gliche L?sungen des Problems:
# OLS mit White Standardfehlern
# Summary mit White Standardfehlern
coeftest(regA, vcov = vcovHC(regA, type = "HC0"))

# GLS: Annahme ?ber Varianz sigma_i^2 = sigma^2*income_i^2
# Konstante zu travel hinzuf?gen
travel$C <- rep(1, 200)
# Transformiere Variablen: Alle Variablen im Datensatz travel durch INCOME teilen
trans <- travel/travel$INCOME
# Sch?tze das Modell
gls <- lm(MILES ~ C + AGE + KIDS, trans)
summary(gls)
