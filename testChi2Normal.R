##TEST DE CHI2 pour verification des modeles##
library(zoo)
library(fitdistrplus)

Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")

variable <- Donnees$temp
histo <- hist(variable, breaks=10)

fit.gamma <- fitdist(variable,"gamma")
plot(fit.gamma)

classSize <- histo$breaks[2] - histo$breaks[1]
class <- histo$counts

moyEch <- mean(variable)
varEch <- sd(variable)

ctr = classSize
probaClass <- vector("numeric", length(class))
expectedClass <- vector("numeric", length(class))

descdist(Donnees$DC, discrete = FALSE)

#TEST NORMAL //[
#shapiro.test(Donnees$FFMC)
#shapiro.test(Donnees$DMC)
#shapiro.test(Donnees$DC)
#shapiro.test(Donnees$ISI)
#shapiro.test(Donnees$temp)
#shapiro.test(Donnees$RH)
#shapiro.test(Donnees$wind)
#//]

#TEST GAMMA //[
#shape <- (moyEch^2)/(varEch^2) #r
#scale <- shape / moyEch
#probaClass <- dgamma(head(histo$breaks, -1), shape, scale)
#//]

#TEST BETA //[
  estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
  }
  
  aplhbet <- estBetaParams(moyEch, varEch)
  
#//]

#vecteur e probabilites
chisq.test(histo$counts, p = probaClass, rescale.p = TRUE)

#Vecteur de valeurs expectee
#expectedClass <- null.probs * 517

#Sommation du chi2
#X2 <- sum(((class - expectedClass)^2)/ expectedClass)
#X2critique <- qchisq(.95, length(class) - 2 - 1) # alpha = 5%