################################################################################
#                                                                              #
#                   �bungsblatt 10: Stochastische Regressoren                  # 
#                                  Instrumentenvariablen                       #
#                                  Aufgabe 4                                   #
#                                                                              #
################################################################################
# ben�tigte Pakete: wenn sie noch nicht installiert sind: install.package("???")
# in die Console
library(lmtest) #f�r Funktion: bptest, vcovHC
library(sandwich) #f�r Funktion: coeftest
library(car) # Funktion: lht (Linearer Hypothesen Test)
library(AER) # Funktion: ivreg (Two Staged Least Squares)

##### Aufgabe 4 #####

# Daten einlesen
inf <- read.csv("C:/Users/leasi/Desktop/�bung2020/�bung10/inf.csv")

# a) Modell 1
regA <- lm(INFLAT ~ MONEY + OUTPUT, data = inf)
summary(regA)

# Heteroskedastizit�t
coeftest(regA, vcov = vcovHC(regA, type = "HC0"))

# Hypothesis (i)
lht(regA, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regA, type = "HC0"))

# Hypothesis (ii)
lht(regA, c("MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regA, type = "HC0"))

# Two Staged Least Squares
regB <- ivreg(INFLAT ~ MONEY + OUTPUT | MONEY + INITIAL + POPRATE + SCHOOL + INV, 
              data = inf)
summary(regB, vcov. = sandwich) # mit vcov.=sandwich ber�cksichtigt man heterosk.

# Hypothesis (i)
lht(regB, c("(Intercept) = 0", "MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regB, type = "HC0"))

# Hypothesis (ii)
lht(regB, c("MONEY = 1", "OUTPUT = -1"), 
    vcov = vcovHC(regB, type = "HC0"))