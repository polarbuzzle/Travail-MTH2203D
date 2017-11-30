#Premiere partie, initialisation des donnes
library(plot3D)
library(moments)
#Lecture des donnees
Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")
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

#Test Normal pour temperature //[
binWidth <- 2
histTemp = hist(df$temp, breaks=seq(0, 35, by=binWidth))
bins = histTemp$counts #Nombre d'occurences dans chaque classes

xfit <- seq(min(df$temp), max(df$temp), length = 40) 
yfit <- dnorm(xfit, mean = mean(df$temp), sd = sd(df$temp)) 
yfit <- yfit * diff(histTemp$mids[1:2]) * length(df$temp) 

lines(xfit, yfit, col = "black", lwd = 2)

probs <- vector("numeric", 17)
binNormale <- vector("numeric", 17)

ctr = 1
for(i in 1:17) { #On retire les probs de chaque
  probs[ctr] <- pnorm(ctr, mean = mean(df$temp), sd=sd(df$temp)) - pnorm(ctr - 1, mean = mean(df$temp), sd=sd(df$temp))
  binNormale[ctr] <- probs[ctr] * 517 #nb frequences EXPECTED
  ctr = ctr + 1
}

chitest = 0
ctr = 1
length(bins)
length(binNormale)
chitest <- sum(((bins - binNormale)^2)/binNormale)

degLib = 35 - 2 - 1
1 - pchisq(chitest, degLib)
#//]



#Test Normal pour vent //[
binWidth = 1
histVent = hist(df$wind, breaks=seq(0, 10, by=binWidth))
bins = histVent$counts #Nombre d'occurences dans chaque classes
probs <- vector("numeric", 10)
binNormale <- vector("numeric", 10)
ctr = 1
for(i in 1:10) { #On retire les probs de chaque
  probs[ctr] <- pnorm(ctr, mean = mean(df$wind), sd=sd(df$wind)) - pnorm(ctr - 1, mean = mean(df$wind), sd=sd(df$wind))
  binNormale[ctr] <- probs[ctr] * 517
  ctr = ctr + 1
}

chitest = 0
ctr = 1
for(i in 1:10) {
  chitest = chitest + ((bins[ctr] - binNormale[ctr])^2) / binNormale[ctr]
  ctr = ctr + 1
}
#//]

#Test Normal pour RH //[
binWidth = 2
histRH = hist(df$RH, breaks=seq(0, 100, by=binWidth))
bins = histRH$counts #Nombre d'occurences dans chaque classes
probs <- vector("numeric", 50)
binNormale <- vector("numeric", 50)
ctr = 1
for(i in 1:50) { #On retire les probs de chaque
  probs[ctr] <- pnorm(ctr, mean = mean(df$RH), sd=sd(df$RH)) - pnorm(ctr - 1, mean = mean(df$RH), sd=sd(df$RH))
  binNormale[ctr] <- probs[ctr] * 517
  ctr = ctr + 1
}

chitest = 0
ctr = 1
for(i in 1:50) {
  chitest = chitest + ((bins[ctr] - binNormale[ctr])^2) / binNormale[ctr]
  ctr = ctr + 1
}
#//]


x <- rnorm(517, mean = mean(df$temp), sd=sd(df$temp))
p1 <- hist(x, breaks=35)
count <- p1$counts
length(count)


probs <- vector("numeric", 36)
binNormale <- vector("numeric", 36)
ctr = 1
for(i in 1:36) { #On retire les probs de chaque
  probs[ctr] <- pnorm(ctr, mean = mean(df$temp), sd=sd(df$temp)) - pnorm(ctr - 1, mean = mean(df$temp), sd=sd(df$temp))
  binNormale[ctr] <- probs[ctr] * 517 #nb frequences EXPECTED
  ctr = ctr + 1
}

chitest <- sum(((count - binNormale)^2)/binNormale)
qchisq(.1, df=5)
