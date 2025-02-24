setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Mémoire")

#pour récupérer exoRégionsAbs : exécuter la partie 'traitement de données'
#du code 'Régression sur les régions'
leTotal <- as.data.frame(exoRégionsAbs$Total)
leTotal <- leTotal[-20,]
années <- 2004:2022
new11 <- read.csv2("pib-france.csv")
new11 <- new11[1:19,-1]
new11 <- rev(new11)
new11 <- new11 * 10000

par(mfrow = c(1,1))
matplot(années, leTotal, type = "l", col = rgb(0, 0, 0, 1), lty = 1, 
        main = "Exonérations et PIB nominaux, série temporelle.", xlab = "Années", ylab = "Euros")
lines(années, new11, col = "red", lwd = 2)
lines(années, leTotal, col = "blue", lwd = 2)
legend("topleft", 
       legend = c( "Exonérations / 1", "PIB / 100"), 
       col = c("blue", "red"), 
       lwd = c(2, 2))
