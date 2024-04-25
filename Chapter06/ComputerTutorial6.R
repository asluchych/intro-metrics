################################################################################
#                                                                              #
#      Übungsblatt 6: Multiples Regressionsmodell II                           # 
#                           Aufgabe 4 + 5                                      #
#                                                                              #
################################################################################
library(lmtest)

##### Aufgabe 4: Modellspezifikation #####

# Daten einlesen
wages <- read.csv("C:/Users/leasi/Desktop/Übung2020/Übung6/wages.csv")

# a) Modell A

regWageA <- lm(WAGE ~ EDUC + AGE, data = wages)
summary(regWageA)

# b) RESET
resettest(regWageA, power = 2:3)

# c) Modell C

regWageC <- lm(WAGE ~ EDUC + I(EDUC^2) + AGE + I(AGE^2), data = wages)
summary(regWageC)

# d) RESET
resettest(regWageC, power = 2:3)

# e) Modell E

regWageE <- lm(WAGE ~ EDUC + I(EDUC^2) + AGE + I(AGE^2) + CITY, data = wages )
summary(regWageE)

##### Aufgabe 5: Kollinearität #####

# Daten einlesen
prod <- read.csv("C:/Users/leasi/Desktop/Übung2020/Übung6/production.csv")

# a) Modell

regProdA <- lm(log(Q) ~ log(K) + log(L), data = prod)
summary(regProdA)

# b) Correlation

cor(prod)

# c) Restingiertes Modell
# Definiere neue Variablen für das Restringierte Modell
prod$y <- log(prod$Q) - log(prod$K)
prod$x <- log(prod$L) - log(prod$K)
# Berechne das Modell
regProdC <- lm(y ~ x, data = prod)
summary(regProdC)
# Berechne SSE_R und SSE_U
sseR <- sum((regProdC$residuals - mean(regProdC$residuals))^2)
sseU <- sum((regProdA$residuals - mean(regProdA$residuals))^2)
# F-Statistik und p-Wert
fStat <- ((sseR - sseU)*regProdA$df.residual)/(sseU*1)
pVal <- pf(fStat, 1, 30, lower.tail = FALSE)
