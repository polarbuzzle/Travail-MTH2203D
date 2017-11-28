#Premiere partie, initialisation des donnes
library(plot3D)
library(moments)
#Lecture des donnees
Donnees <- read.csv("/Users/Adrimeov/Desktop/projet\ MTH/forestfires.csv", header = TRUE, sep = ",", dec = ".")
Donnees

#On ignore les données jours et mois
df <- Donnees[ -c(0, 3:4, 14)]
df

xBar = vector("numeric", 11) #Moyenne echantillonnale
s = vector("numeric", 11) #Variance echantionnale
symetry = vector("numeric", 11) #Coeff de symetrie
applat = vector("numeric", 11) #Coeff d'applatissement
sectionName = vector("numeric", 11) #Noms des donnees

ctr = 1
for (i in df) { #Boucle pour remplir les differents tableaux
  sectionName[ctr] = colnames(df)[ctr]
  xBar[ctr] = mean(i)
  s[ctr] = var(i)
  symetry[ctr] = skewness(i)
  applat[ctr] = kurtosis(i)
  ctr = ctr + 1
}

#Arrondir et limiter a trois chiffres apres la decimale
format(round(xBar, digits = 3) ,digits = 3)
xBar = format(round(xBar, digits = 3) ,digits = 3)
s = format(round(s, digits = 3) ,digits = 3)
symetry = format(round(symetry, digits = 3) ,digits = 3)
applat = format(round(applat, digits = 3) ,digits = 3)

ctr = 1
#Diagrammes en boîtes
for (i in df) {
  boxplot(i, horizontal = TRUE, col = "blue",main = sectionName[ctr])
  ctr = ctr + 1
}

#Affichage 3d de la répartition des feux selon les coordonnées
z <- table(df$X, df$Y)
hist3D(z=z, border="black", main = "Répartition des feux selon coordonnées")
image2D(z=z, border = "black", main = "Répartition des feux selon coordonnées")

#Histogrammes
ctr = 1
for (i in df) {
  hist(i, main = sectionName[ctr], xlab =  sectionName[ctr])
  legend("topright", inset=c(0, 0), legend=c(parse(text=sprintf('moyenne == %s ', xBar[ctr])), parse(text=sprintf('variance == %s ', s[ctr])), parse(text=sprintf('symetrie == %s ', symetry[ctr])), parse(text=sprintf('moyenne == %s ', applat[ctr]))), bty='n')
  ctr = ctr + 1
}
