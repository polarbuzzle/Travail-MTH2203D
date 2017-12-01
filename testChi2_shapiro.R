##TEST DE CHI2 pour verification des modeles##
library(zoo)
library(fitdistrplus)
library(MASS)

Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")

variable <- Donnees$temp
histo <- hist(variable, breaks=20)

classSize <- histo$breaks[2] - histo$breaks[1]
class <- histo$counts

moyEch <- mean(variable)
varEch <- sd(variable)

ctr = classSize
probaClass <- vector("numeric", length(class))
expectedClass <- vector("numeric", length(class))

#TEST NORMAL //[
shapiro.test(Donnees$FFMC)
shapiro.test(Donnees$DMC)
shapiro.test(Donnees$DC)
shapiro.test(Donnees$ISI)
shapiro.test(Donnees$temp)
shapiro.test(Donnees$RH)
shapiro.test(Donnees$wind)
#//]

#TEST GAMMA //[
shape <- (moyEch^2)/(varEch^2) #r
scale <- shape / moyEch
probaClass <- dgamma(head(histo$breaks, -1), shape, scale)
#//]

#TEST WEIBULL //[

fitdistr(variable, densfun="weibull", lower = 0)
shape = 3.6841318
scale = 20.9063320
probaClass <- dweibull(histo$breaks, shape, scale)
#//]

#vecteur e probabilites
chisq.test(histo$counts, p = probaClass, rescale.p = TRUE)
