setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Post traitement de tendance")
temps <- 2004:2023

rm(list = setdiff(ls(), c("exoRégions", "partActive", "temps")))

par(mfcol = c(1, 2))

######analyse d'une tendance moyenne à travers les séries de part active###########
######le fichier part active peut être constitué avec le code "Détection des tendances.R"
#exclusion des territoires d'outre mer + de la franche-comté
priseEnCompte <- c(11, 12, 15, 19)
correction <- partActive#[, priseEnCompte]
var_cible <- rowMeans(correction, na.rm = TRUE)

#empilement
data <- data.frame(
  temps = temps,
  valeurs = var_cible
)

#modèle quadratique global
model <- lm(valeurs ~ I(temps^2) + temps, data = data)
summary(model)
#coefficients du polynôme
coefficients <- coef(model)
cat("Polynôme ajusté : ", 
    paste0("y = ", round(coefficients[1], 2), " + ", 
           round(coefficients[2], 2), "x + ", 
           round(coefficients[3], 2), "x^2"), "\n")

#récupération de la tendance
trendPop <- predict(model, newdata = data.frame(temps = temps))
#graphique
matplot(temps, correction, type = "l", col = rgb(0.7, 0.7, 0.7, 0.5), lty = 1, 
        main = "Ajustement non linéaire avec OM", xlab = "Années", ylab = "Part employée")
lines(temps, trendPop, col = "red", lwd = 2)
text(x = max(temps), y = min(correction),
     labels = sprintf("T = %.5f\nT² = %.5f\np = %.3e\nR² adj = %.3f",
                      coef(model)[2],
                      coef(model)[3],
                      pf(summary(model)$fstatistic[1],
                         summary(model)$fstatistic[2],
                         summary(model)$fstatistic[3],
                         lower.tail = FALSE),
                      summary(model)$adj.r.squared),
     adj = c(1,0))
legend("topright", legend = c("Tendance"), col = "red", lwd = 2)

summary(model)

#retrait tendances
partActive_nouv <- correction - trendPop
partActive_nouv <- partActive - trendPop
matplot(temps, partActive_nouv, type = "l", col = rgb(0.7, 0.7, 0.7, 0.5), lty = 1, 
        main = "Retrait de la tendance", xlab = "Années", ylab = "Exonérations/pop")

#écriture des données
library(writexl)
write_xlsx(partActive_nouv, "Part employée avec OM, corrigée")
write_xlsx(partActive, "Part employée avec OM, non corrigée")
write.csv2(trendPop, "Tendance part employée avec OM.csv2")

#récup des données de régression
coef_mod <- summary(model)$coefficients
anova_mod <- anova(model)
r_carré <- format(round(summary(model)$r.squared, 6), scientific = FALSE)
adj_r_carré <- format(round(summary(model)$adj.r.squared, 6), scientific = FALSE)

coef_mod1 <- summary(model)$coefficients
anova_mod1 <- anova(model)
r_carré1 <- format(round(summary(model)$r.squared, 6), scientific = FALSE)
adj_r_carré1 <- format(round(summary(model)$adj.r.squared, 6), scientific = FALSE)

#...dans un même data frame
indicateurs <- data.frame(
  Indicateur = c(
    "Intercept", "Coefficient lin.", "Coefficient non lin.", "R²", "R² ajusté"
  ),
  AvecOM = c(
    coef_mod[1, 1],
    coef_mod[2, 1],
    coef_mod[3, 1],
    r_carré,
    adj_r_carré
  ),
  SansOM = c(
    coef_mod1[1, 1],
    coef_mod1[2, 1],
    coef_mod1[3, 1],
    r_carré1,
    adj_r_carré1
  )
)

write_xlsx(indicateurs, "Indicateurs des 2 modèles")


############# VERSION EXO ##################
temps <- 2004:2023
rm(list = setdiff(ls(), c("exoRégions", "partActive", "temps")))
par(mfrow = c(1, 2))
#moyenne de toutes les séries

correction <- exoRégions[, -c(11, 12, 15, 19)]
var_cible <- rowMeans(correction, na.rm = TRUE)


#empilement
data <- data.frame(
  temps = temps,
  valeurs = var_cible
)

#modèle linéaire global
model <- lm(valeurs ~ temps, data = data)

#coefficients
coefficients <- coef(model)
cat("Modèle : ", 
    paste0("y = ", round(coefficients[1], 2), " + ", 
           round(coefficients[2], 2), "x"), "\n")

#tendance
trendExo <- predict(model, newdata = data.frame(temps = temps))

#graphique
matplot(temps, correction, type = "l", col = rgb(0.7, 0.7, 0.7, 0.5), lty = 1, 
        main = "Ajustement linéaire sans OM", xlab = "Années", ylab = "Exonérations/pop")
lines(temps, trendExo, col = "red", lwd = 2)
text(x = max(temps), y = min(correction),
     labels = sprintf("β = %.5f\np = %.3e\nR² adj = %.3f",
                      coef(model)[2],
                      summary(model)$coefficients[2,4],
                      summary(model)$adj.r.squared),
     adj = c(1,0))
legend("topright", legend = c("Tendance"), col = "red", lwd = 2)

summary(model)

#application de la tendance
exoRégions_nouv <- correction - trendExo
matplot(temps, exoRégions_nouv, type = "l", col = rgb(0.7, 0.7, 0.7, 0.5), lty = 1, 
        main = "Retrait de la tendance", xlab = "Années", ylab = "Exonérations/pop")

library(writexl)
setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Les tendances en chiffres")
write.csv2(trendExo, "Tendance linéaire exonérations sans OM.csv")
