# Estimation de parametres
Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")

# Utilisation de la variable temperature
variable <- Donnees$temp

# Calcul des variables
moy <- mean(variable)
sd <- sd(variable)
n <- length(variable)
error <- qnorm(0.95) * sd / sqrt(n)
left <- moy - error
right <- moy + error

# Affichage des resultats
print(paste("Intervale de confiance a 95% de la variable ",colnames(df)[7], ": [", left, " < ", moy , "< ", right, "]"))






