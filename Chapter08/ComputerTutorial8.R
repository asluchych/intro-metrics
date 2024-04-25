################################################################################
#                                                                              #
#                   �bungsblatt 8: Heteroskedastie                             # 
#                                  Aufgabe 2                                   #
#                                                                              #
################################################################################
# ben�tigte Pakete: wenn sie noch nicht installiert sind: install.package("???")
# in die Console
library(lmtest) #f�r Funktion: bptest, vcovHC
library(sandwich) #f�r Funktion: coeftest
##### Aufgabe 2: Reisen #####

# Daten einlesen
travel <- read.csv("C:/Users/leasi/Desktop/�bung2020/�bung8/travel.csv")

# a) Modell 1
regA <- lm(MILES ~ INCOME + AGE + KIDS, data = travel)
summary(regA)

# b) Test - Je nachdem wie die Diskussion in a) ausgeht, gibt es verschiedene 
# M�glichkeiten zu Testen, hier Breusch-Pagan Test mit INCOME und AGE als 
# erkl�rende Variablen

# speichere die Residuen zum Quadrat als abh�ngige Variable der Hilfsregression
resid2 <- regA$residuals^2
# F�hre die Hilfsregression durch
aux <- lm(resid2 ~ travel$INCOME + travel$AGE)
# speichere die Summary von der Hilfsregression
sumAux <- summary(aux)
# Berechne die Test Statistik - LM Test -> N*R^2
bp <- 200*sumAux$r.squared
# Berechne den p-Wert -> eine LM Test Statistik ist X^2 verteilt
pVal <- pchisq(bp, 2, lower.tail = FALSE)

# Es gibt auch ein package f�r den Breusch-Pagan Test
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

# d) M�gliche L�sungen des Problems:
# OLS mit White Standardfehlern
# Summary mit White Standardfehlern
coeftest(regA, vcov = vcovHC(regA, type = "HC0"))

# GLS: Annahme �ber Varianz sigma_i^2 = sigma^2*income_i^2
# Konstante zu travel hinzuf�gen
travel$C <- rep(1, 200)
# Transformiere Variablen: Alle Variablen im Datensatz travel durch INCOME teilen
trans <- travel/travel$INCOME
# Sch�tze das Modell
gls <- lm(MILES ~ C + AGE + KIDS, trans)
summary(gls)