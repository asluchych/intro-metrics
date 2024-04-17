################################################################################
#                                                                              #
#                   Übungsblatt 7: Indikatorvariable                           # 
#                                  Aufgabe 2                                   #
#                                                                              #
################################################################################

##### Aufgabe 2: Managergehälter #####

# Daten einlesen
sal <- read.csv("C:/Users/leasi/Desktop/Übung2020/Übung7/sal.csv")

# a) Modell 1

regA <- lm(log(SALARY) ~ EDUC + GENDER, data = sal)
summary(regA)

# b) F-Test in summary(regA)

# c) H_0: beta_3 = 2*beta_2
# definiere Hilfsvariable für restringiertes Modell x = EDUC+2*GENDER
sal$x <- sal$EDUC + 2*sal$GENDER
regRestricted <- lm(log(SALARY) ~ x, data = sal)
summary(regRestricted)
# Berechne SSE_R und SSE_U
sseR <- sum((regRestricted$residuals - mean(regRestricted$residuals))^2)
sseU <- sum((regA$residuals - mean(regA$residuals))^2)
# F-Statistik und p-Wert
fStat <- ((sseR - sseU)*regA$df.residual)/(sseU*1)
pVal <- pf(fStat, 1, regA$df.residual, lower.tail = FALSE)

# d) Modell d

regD <- lm(log(SALARY) ~ EDUC + GENDER + MANAGER, data = sal)
summary(regD)

# e) Modell e
regE <- lm(log(SALARY) ~ EDUC + GENDER + MANAGER + MANAGER*GENDER, data = sal)
summary(regE)

# f) R^2 und adjusted R^2 aus der summary
# Akaike Information Criteria
AIC(regD)
AIC(regE)
# Scharz Criterion
BIC(regD)
BIC(regE)

# g) Anzahl der weiblicher Manager
length(sal$GENDER[sal$GENDER == 0 & sal$MANAGER == 1])
