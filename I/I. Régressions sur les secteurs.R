setwd("C:/Users/thiba/Desktop/Cours/Modélisation Progra/Terrain")

données <- read.csv("Exos simples - par NA38 + caté - 04-23.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

les_colonnes <- colnames(données)[c(3, 6, 9)]

exoSecteurs <- données[, les_colonnes, drop = FALSE]

exoSecteurs <- as.data.frame(exoSecteurs)

données <- read.csv("Bonnes données temp - E+MS - Grands secteurs - Régions - 98-24.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

les_colonnes <- colnames(données)[c(2, 4, 5, 11)]

effectifsSecteurs <- données[, les_colonnes, drop = FALSE]

effectifsSecteurs <- as.data.frame(effectifsSecteurs)


write.csv2(effectifsSecteurs[1:100, ], "EFFECTIFS_courts.csv", row.names = FALSE)
write.csv2(exoSecteurs, "EXONERATIONS.csv", row.names = FALSE)


library(tidyr)
library(dplyr)

effectifsSecteurs_mod <- effectifsSecteurs %>%
  group_by(annee, grand_secteur_d_activite) %>%
  summarise(effectifs = sum(effectifs_salaries_brut)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = grand_secteur_d_activite,
    values_from = effectifs
  )

effectifsSecteurs_mod <- effectifsSecteurs_mod %>%
  select(annee, everything())

effectifsSecteurs_mod <- effectifsSecteurs_mod[7:26, ]

##########

exoSecteurs_mod <- exoSecteurs %>%
  group_by(X.Année, Grand.secteur.d.activité) %>%
  summarise(exonérations = sum(Montant.des.exonérations)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Grand.secteur.d.activité,
    values_from = exonérations
  )

exoSecteurs_mod <- exoSecteurs_mod %>%
  select(X.Année, everything())

########## Début de l'analyse statistique

# Initialisation des listes
exonerations <- list()
effectifs <- list()
leMod <- list()
leTableau <- list()
nomsSecteurs <- list()
par(mfrow = c(2, 2))


# Boucle sur les indices spécifiés
for (i in 6:8) {
  # Remplissage des listes avec les données
  exonerations[[i]] <- exoSecteurs_mod[[i]]
  effectifs[[i]] <- effectifsSecteurs_mod[[i]]
  nomsSecteurs[[i]] <- colnames(exoSecteurs_mod)[i]
  
  # Création du modèle linéaire
  leMod[[i]] <- lm(effectifs[[i]] ~ exonerations[[i]])
  
  # Traçage du graphique pour chaque secteur
  plot(
    exonerations[[i]], effectifs[[i]], 
    main = paste(nomsSecteurs[[i]]), 
    xlab = "Exonérations", 
    ylab = "Effectifs salariés", 
    pch = 19
  )
  abline(leMod[[i]], col = "red")
}
par(mfrow = c(1, 1))
