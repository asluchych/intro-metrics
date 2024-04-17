################################################################################
#                                                                              #
#                   Übungsblatt 8: Heteroskedastie                             # 
#                                  Aufgabe 2                                   #
#                                                                              #
################################################################################
# benötigte Pakete: wenn sie noch nicht installiert sind: install.package("???")
# in die Console
library(lmtest) #für Funktion: bptest, vcovHC
library(sandwich) #für Funktion: coeftest
##### Aufgabe 2: Reisen #####

# Daten einlesen
travel <- read.csv("C:/Users/leasi/Desktop/Übung2020/Übung8/travel.csv")

# a) Modell 1
regA <- lm(MILES ~ INCOME + AGE + KIDS, data = travel)
summary(regA)

# b) Test - Je nachdem wie die Diskussion in a) ausgeht, gibt es verschiedene 
# Möglichkeiten zu Testen, hier Breusch-Pagan Test mit INCOME und AGE als 
# erklärende Variablen

# speichere die Residuen zum Quadrat als abhängige Variable der Hilfsregression
resid2 <- regA$residuals^2
# Führe die Hilfsregression durch
aux <- lm(resid2 ~ travel$INCOME + travel$AGE)
# speichere die Summary von der Hilfsregression
sumAux <- summary(aux)
# Berechne die Test Statistik - LM Test -> N*R^2
bp <- 200*sumAux$r.squared
# Berechne den p-Wert -> eine LM Test Statistik ist X^2 verteilt
pVal <- pchisq(bp, 2, lower.tail = FALSE)

# Es gibt auch ein package für den Breusch-Pagan Test
bptest(regA, varformula = ~ INCOME + AGE, data = travel)

# c) Scatter Plots
# (ii) Residuen Graph
# Alter
plot(regA$residuals ~ travel$AGE, main = "Alter")
abline(h=0)

# Kinder
plot(regA$residuals ~ travel$KIDS, main = "Kinder")
abline(h=0)

# Einkommen
plot(regA$residuals ~ travel$INCOME, main = "Einkommen")
abline(h=0)

# d) Mögliche Lösungen des Problems:
# OLS mit White Standardfehlern
# Summary mit White Standardfehlern
coeftest(regA, vcov = vcovHC(regA, type = "HC0"))

# GLS: Annahme über Varianz sigma_i^2 = sigma^2*income_i^2
# Konstante zu travel hinzufügen
travel$C <- rep(1, 200)
# Transformiere Variablen: Alle Variablen im Datensatz travel durch INCOME teilen
trans <- travel/travel$INCOME
# Schätze das Modell
gls <- lm(MILES ~ C + AGE + KIDS, trans)
summary(gls)
