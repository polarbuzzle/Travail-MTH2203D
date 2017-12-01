
Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")
df <- Donnees[ -c(0, 3:4, 14)]

ctrI = 1
ctrJ = 1

# VERIFICATION DES CORRELATIONS ENTRE VARIABLES
for (i in df) {
  for (j in df) {
    cor <- cor(i, j)
    
      cat(cor," -> ",colnames(df)[ctrI], " avec ", colnames(df)[ctrJ], "\n")
      
      plot(i, j, main = "Relations entre deux variables",
      xlab = colnames(df)[ctrI], ylab = colnames(df)[ctrJ])
    
    ctrJ = ctrJ + 1
  }
  ctrJ = 1
  ctrI = ctrI + 1
}

# TEMP AVEC FFMC //[
temp <- df$temp
FFMC <- df$FFMC
mesDonnees <- data.frame(temp, FFMC)

linMod <- lm(FFMC ~ temp, data = mesDonnees)
summary(linMod) # estimation de la droite : FFMC = 82.89560 + 0.41024temp
# r^2 = 0.1862 -> 18,62% des res correspondent a la droite estimee

# residual plot
residuPlot <- resid(linMod)
residu <- residuals(linMod)
plot(residuPlot, main = paste("Graph de residu de ",colnames(mesDonnees)[2], " selon ",
                          colnames(mesDonnees)[1]), xlab = "temp", ylim = c(-60, 60))
abline(0, 0, col = "red")
#Ca passe

# qq plot
qqnorm(residu, ylim = c(-60, 60))
qqline(residu, col = "red")
# ca passe

# H0 : Il n'y a pas de relation lineaire entre FFMC et temp
# p-value etant tres petit, on rejete H0, alors il existe une relation lineaire

plot(temp, FFMC, main = paste("Relation entre ", colnames(mesDonnees)[2], " et température"),
     xlab = colnames(mesDonnees)[1],
      ylab = colnames(mesDonnees)[2], ylim = c(20, 150))

abline(82.89560, 0.41024, col = "red")

# //]



# DMC ET ISI //[
DMC <- df$DMC
ISI <- df$ISI
mesDonnees <- data.frame(DMC, ISI)

linMod <- lm(ISI ~ DMC, data = mesDonnees)
summary(linMod) # estimation de la droite : FFMC = 6.613285 + 0.021722DMC
# r^2 = 0.0931 -> 9.31% des res correspondent a la droite estimee

# residual plot
residuPlot <- resid(linMod)
residu <- residuals(linMod)
plot(residuPlot, main = paste("Graph de residu de ",colnames(mesDonnees)[2], " selon ",
                              colnames(mesDonnees)[1]), xlab = "temp", ylim = c(-60, 60))
abline(0, 0, col = "red")
#Ca passe

# qq plot
qqnorm(residu, ylim = c(-10, 10))
qqline(residu, col = "red")
# ca passe

# H0 : Il n'y a pas de relation lineaire entre ISI et DMC
# p-value etant tres petit, on rejete H0, alors il existe une relation lineaire

plot(DMC, ISI, main = paste("Relation entre ", colnames(mesDonnees)[2], " et ", colnames(mesDonnees)[1]),
     xlab = colnames(mesDonnees)[1],
     ylab = colnames(mesDonnees)[2], ylim = c(-10, 30))

abline(6.613285, 0.021722, col = "red")
# //]
