library(fitdistrplus)
library(logspline)

Donnees <- read.csv("/home/samuel/Desktop/R/forestfires.csv", header = TRUE, sep = ",", dec = ".")
variable <- Donnees$temp

#Pour determiner quel model semble marcher
descdist(variable, discrete = FALSE)

#Pour examiner visuellement
fit.gamma <- fitdist(variable, "gamma")
plot(fit.gamma) #Comparaison avec Gamma
fit.weibull <- fitdist(variable, "weibull")
plot(fit.weibull) #Comparaison avec Weibull

#Valeur aic pour determiner lequel est le plus interessant
fit.gamma$aic
fit.weibull$aic
#On choisit Weibull car plus petit (quand meme eleve)

#Kolmogorov-Smirnov test simulation
n.sims <- 5e4
#ok je dois comprendre ca
stats <- replicate(n.sims, {
  r <- rweibull(n = length(variable)
                , shape= fit.weibull$estimate["shape"]
                , scale = fit.weibull$estimate["scale"]
  )
  as.numeric(ks.test(r
                     , "pweibull"
                     , shape= fit.weibull$estimate["shape"]
                     , scale = fit.weibull$estimate["scale"])$statistic
  )      
})

#affichage du graphique
plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()

#P-value
fit <- logspline(stats)

1 - plogspline(ks.test(variable
                       , "pweibull"
                       , shape= fit.weibull$estimate["shape"]
                       , scale = fit.weibull$estimate["scale"])$statistic
               , fit
)

