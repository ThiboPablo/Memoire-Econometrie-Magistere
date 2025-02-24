setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Terrain")


############## Traitement de données
library(readxl)
#récup des données de pop
données <- read_excel("Données de pop par ancienne région.xlsx")

#récup de 2004 à 2023
les_colonnes <- colnames(données)[c(1, 8:27)]
popRégions <- données[, les_colonnes, drop = FALSE]

#transpos de la matrice
popRégions <- as.data.frame(t(popRégions))

#récupération des noms et suppression de la ligne correspondante
lesNoms <- as.vector(popRégions[1, ])
colnames(popRégions) <- lesNoms
rm(lesNoms)
popRégions <- popRégions[-1,]

#tri des régions par ordre alpha (le total se place nécessairement à la fin)
popRégions <- popRégions[, order(names(popRégions))]
#conversion des valeurs dans les cases en entiers intelligibles par le logiciel
popRégions <- as.data.frame(lapply(popRégions, function(col) {
  if (is.character(col)) {
    as.integer(as.numeric(gsub(",", ".", col)))
  } else {
    col
  }
}))
#noms de lignes propres
rownames(popRégions) <- 2004:2023

#récup des données d'éxonération
données <- read.csv("Exos simples - par région + caté - 04-23.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

#récup des colonnes utiles
les_colonnes <- colnames(données)[c(2, 5, 10)]
exoRégions <- données[, les_colonnes, drop = FALSE]

library(tidyr)
library(dplyr)

#agrégation par année, discrimination par ancienne région
exoRégions <- exoRégions %>%
  group_by(X.Année, Ancienne.région) %>%
  summarise(exonérations = sum(Montant.des.exonérations)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Ancienne.région,
    values_from = exonérations,
  )

exoRégions <- exoRégions %>%
  select(X.Année, everything())
exoRégions <- exoRégions[,-1]
rownames(exoRégions) <- 2004:2023
exoRégions$Total <- rowSums(exoRégions)
#rangement par ordre abc
exoRégions <- exoRégions[, order(names(exoRégions))]


#récup des données d'effectifs salariés
données <- read.csv("Bonnes données temp - E+MS - Grands secteurs - Régions - 98-24.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
#récup des colonnes importantes
les_colonnes <- colnames(données)[c(2, 4, 5, 6, 11)]
effectifsRégions <- as.data.frame(données[, les_colonnes, drop = FALSE])
rm(données, les_colonnes)

#récup des données du trimestre 4 de chaque année uniquement
effectifsRégions <- subset(effectifsRégions, trimestre == 4)

#agrégation par année et discrimination par ancienne région
effectifsRégions <- effectifsRégions %>%
  group_by(annee, ancienne_region) %>%
  summarise(effectifs = sum(effectifs_salaries_brut)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = ancienne_region,
    values_from = effectifs
  )

effectifsRégions <- effectifsRégions %>%
  select(annee, everything())

#récup des années 2004-2023
effectifsRégions <- effectifsRégions[7:26, ]
#suppression de la colonne des années
effectifsRégions<- effectifsRégions[,-1]
#ajout d'une colonne de total
effectifsRégions$Total <- rowSums(effectifsRégions)
#rangement par ordre abc
effectifsRégions <- effectifsRégions[, order(names(effectifsRégions))]
#ajout de noms aux lignes
rownames(effectifsRégions) <- 2004:2023


#création de la base de données étant l'objet de tout ce travail
partActive <- effectifsRégions / popRégions
exoRégions <- exoRégions / popRégions
colnames(partActive) <- gsub("\\.", "-", colnames(partActive))


###### Détection d'une tendance. #######################################################

années <- 2004:2023

#initialisation des listes
exonerations <- list()
donnéesPartEmployée <- list()
tempExo <- list()
tempPop <- list()
leTableau <- list()
nomsRégions <- list()


#boucle de régressions (par groupe de 6 graphiques)
for (k in 1:5) {
  pdf(paste0(k*6-5, "-", min(k*6, 27), " temp pop.pdf"), width = 11.69, height = 8.27)
  par(mfrow = c(2, 3))
  for (i in (k*6-5):(min(k*6, 27))) {
    #récup des données
    donnéesPartEmployée[[i]] <- partActive[[i]]
    nomsRégions[[i]] <- colnames(exoRégions)[i]
    
    tempPop[[i]] <- lm(donnéesPartEmployée[[i]] ~ années)
    
    #graphique pour chaque région
    plot(
      années, donnéesPartEmployée[[i]], 
      main = paste(nomsRégions[[i]]), 
      xlab = "Années", 
      ylab = "Part de la population employée", 
      pch = 19
    )
    abline(tempPop[[i]], col = "red")
    text(x = max(années), y = min(donnéesPartEmployée[[i]]),
         labels = sprintf("β = %.3f\np = %.3e", 
                          coef(tempPop[[i]])[2],
                          summary(tempPop[[i]])$coefficients[2,4]),
         adj = c(1,0))
  }
  dev.off()
}
for (k in 1:5) {
  pdf(paste0(k*6-5, "-", min(k*6, 27), " temp exo.pdf"), width = 11.69, height = 8.27)
  par(mfrow = c(2, 3))
  for (i in (k*6-5):(min(k*6, 27))) {
    #récup des données
    exonerations[[i]] <- exoRégions[[i]]
    nomsRégions[[i]] <- colnames(exoRégions)[i]
    
    tempExo[[i]] <- lm(exonerations[[i]] ~ années)
    
    plot(
      années, exonerations[[i]], 
      main = paste(nomsRégions[[i]]), 
      xlab = "Années", 
      ylab = "Exonérations", 
      pch = 19
    )
    abline(tempExo[[i]], col = "red")
    text(x = max(années), y = min(exonerations[[i]]),
         labels = sprintf("β = %.3f\np = %.3e", 
                          coef(tempExo[[i]])[2],
                          summary(tempExo[[i]])$coefficients[2,4]),
         adj = c(1,0))
  }
  dev.off()
}

summary(tempPop[[1]])

indicateursExo <- data.frame(
  Indicateur = c(
    "Intercept", "Coefficient", "log10(P-V)", "R²"
  )
)
indicateursPop <- data.frame(
  Indicateur = c(
    "Intercept", "Coefficient", "log10(P-V)", "R²"
  )
)
for (i in 1:27) {
  coef_modExo <- summary(tempExo[[i]])$coefficients
  coef_modPop <- summary(tempPop[[i]])$coefficients
  r_carréExo <- format(round(summary(tempExo[[i]])$r.squared, 6), scientific = FALSE)
  r_carréPop <- format(round(summary(tempPop[[i]])$r.squared, 6), scientific = FALSE)
  
  #remplissage
  indicateursExo[nomsRégions[[i]]] <- c(
    coef_modExo[1, 1],
    coef_modExo[2, 1],
    log10(coef_modExo[2, 4]),
    r_carréExo
  )
  indicateursPop[nomsRégions[[i]]] <- c(
    coef_modPop[1, 1],
    coef_modPop[2, 1],
    log10(coef_modPop[2, 4]),
    r_carréPop
  )
}

write.csv2(indicateursExo, "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Indicateurs temp exo.csv")
write.csv2(indicateursPop, "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Indicateurs temp pop.csv")
#rm(list = setdiff(ls(), c("indicateursExo", "indicateursPop", "partActive", "années")))

par(mfrow = c(1, 1))
plot(
  années, partActive[[16]], 
  xlab = "Années", 
  ylab = "Part Active", 
  pch = 19
)
model <- lm(partActive[[16]] ~ poly(années, 2))  # Ajustement quadratique
tendance <- predict(model, data.frame(années = années))
données_corrigees <- partActive[[16]] - tendance

# Visualisation
plot(années, partActive[[16]], type = "l", main = "Données brutes et tendance")
lines(années, tendance, col = "red", lwd = 2)
plot(années, données_corrigees, type = "l", main = "Données corrigées")

######### Correction des tendances

library(readxl)
#récup des données de pop
exoRégions <- read_excel("exoRégions.xlsx")
rownames(exoRégions) <- 2004:2023
nomsRégions <- colnames(exoRégions)

#a <- indicateursExo[[2, 2]]

# Boucle sur chaque région dans nomsRégions
j <- 1
for (nom in nomsRégions) {
  # Parcours des lignes de la table exoRégions
  for (i in 1:nrow(exoRégions)) { 
    a <- indicateursExo[[2, j + 1]]
    b <- indicateursExo[[1, j + 1]]
    tendance <- (i + 2003) * a + b
    varEnCours <- exoRégions[i, j]
    exoRégions[i, nom] <- varEnCours - tendance
  }
  j <- j + 1
}
library(writexl)
write_xlsx(exoRégions, "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Exo Temp Corrigé.xlsx")
