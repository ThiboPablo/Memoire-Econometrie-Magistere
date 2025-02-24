setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Terrain")


############## Traitement de données
library(readxl)
library(writexl)
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
exoRégionsAbs <- exoRégions
exoRégions <- exoRégions / popRégions
colnames(partActive) <- gsub("\\.", "-", colnames(partActive))

####Récupération et ajustement des tendances
setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/Détection des tendances/Les tendances")
trendExo <- read.csv2("Tendance exonérations avec OM.csv")
trendPop <- read.csv2("Tendance part employée avec OM.csv")
trendPop <- trendPop[, -1]
trendExo <- trendExo[, -1]


####Version sans/juste Outre-mers (ajuster avec un "-" dans les crochets)
lesOM <- c(11, 12, 15, 19)
exoRégionsAbs <- exoRégionsAbs[, -lesOM]
effectifsRégions <- effectifsRégions[, -lesOM]
popRégions <- popRégions[, -lesOM]
exoRégionsAbs$Total <- rowSums(exoRégionsAbs)
effectifsRégions$Total <- rowSums(effectifsRégions)
popRégions$Total <- rowSums(popRégions)
partActive <- effectifsRégions / popRégions
exoRégions <- exoRégionsAbs / popRégions
exoRégions <- exoRégions - trendExo
partActive <- partActive - trendPop


####Version décalage d'un an
sansLagPartActive <- partActive
sansLagExoRégions <- exoRégions
partActive <- partActive[-1,]
exoRégions <- exoRégions[-20,]
rownames(partActive) <- 2005:2023
rownames(exoRégions) <- 2004:2022

setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/4ème rapport (avec lag)")
##########analyse statistique

nomFichier <- " avec lag, sans OM"

#initialisation des listes
exonerations <- list()
donnéesPartEmployée <- list()
leModèle <- list()
leTableau <- list()
nomsRégions <- list()

#boucle de régressions (par groupe de 6 graphiques)
for (k in 1:4) {
  h <- min(k*6, 23)
  pdf(paste0(k*6-5, "-", h, nomFichier, ".pdf"), width = 11.69, height = 8.27)
  par(mfrow = c(2, 3))
  for (i in (k*6-5):(h)) {
    #récup des données
    exonerations[[i]] <- exoRégions[[i]]
    donnéesPartEmployée[[i]] <- partActive[[i]]
    nomsRégions[[i]] <- colnames(exoRégions)[i]
  
    leModèle[[i]] <- lm(donnéesPartEmployée[[i]] ~ exonerations[[i]])
  
    #graphique pour chaque région
    plot(
      exonerations[[i]], donnéesPartEmployée[[i]], 
      main = paste(nomsRégions[[i]]), 
      xlab = "Exonérations", 
      ylab = "Part de la population employée", 
      pch = 19
    )
    abline(leModèle[[i]], col = "red")
    text(x = max(exonerations[[i]]), y = min(donnéesPartEmployée[[i]]),
         labels = sprintf("β = %.3f\np = %.3e\nR² adj = %.3f",
                          coef(leModèle[[i]])[2],
                          summary(leModèle[[i]])$coefficients[2,4],
                          summary(leModèle[[i]])$adj.r.squared),
         adj = c(1,0))
  }
  dev.off()
}
par(mfrow = c(1, 1))

#création d'un df pour récupérer les données chiffrées des modèles
indicateurs <- data.frame(
  Indicateur = c(
    "Intercept", "Coefficient", "P-V coef", 
    "SCE", "SCR", "R²", "R² ajusté"
  )
)
for (i in 1:23) {
  coef_mod <- summary(leModèle[[i]])$coefficients
  anova_mod <- anova(leModèle[[i]])
  r_carré <- format(round(summary(leModèle[[i]])$r.squared, 6), scientific = FALSE)
  adj_r_carré <- format(round(summary(leModèle[[i]])$adj.r.squared, 6), scientific = FALSE)

#calculs pour les SC
  SCE <- sum((fitted(leModèle[[i]]) - mean(donnéesPartEmployée[[i]]))^2)
  SCR <- sum(residuals(leModèle[[i]])^2)

#remplissage
  indicateurs[nomsRégions[[i]]] <- c(
    coef_mod[1, 1],
    coef_mod[2, 1],
    coef_mod[2, 4],
    SCE,
    SCR,
    r_carré,
    adj_r_carré
  )
}

write.csv2(indicateurs, "C:/Users/thiba/Desktop/Cours/Modélisation Progra/Projet exo/4ème rapport (avec lag)/Indicateurs avec lag, sans OM.csv")