##############################################
# TRAITEMENTS GRAPHIQUES SOUS R
# Évaluation de la qualité écologique
# Indices I2M2 et IBMR – Nive des Aldudes
##############################################

###########################
# Chargement des packages #
###########################

# Packages nécessaires à la manipulation des données et à la production des graphiques
library(fmsb)       
library(tidyverse)  
library(ggpubr)     
library(readxl)     
library(ggplot2)   
library(dplyr)      

###########################################
# PARTIE 1 – GRAPHIQUES RELATIFS À L’I2M2 #
###########################################

####################################
# 1.1 Outil d’évaluation de l’I2M2 #
####################################

# Données EQR des métriques constitutives de l’I2M2 pour les stations amont et aval
I2M2_eval <- data.frame(
  row.names = c("Amont", "Aval"),
  Shannon = c(0.7642, 0.6074),
  ASPT = c(0.779, 0.6963),
  Polyvoltinisme = c(0.5905, 0.7201),
  Ovoviviparite = c(0.6625, 0.7079),
  Richesse_taxonomique = c(0.5245, 0.6157)
)
I2M2_eval

# Définition des bornes minimales et maximales (0–1) pour la construction du radar
max_min_eval <- data.frame(
  Shannon = c(1, 0), ASPT = c(1, 0), Polyvoltinisme = c(1, 0),
  Ovoviviparite = c(1, 0), Richesse_taxonomique = c(1, 0)
)
rownames(max_min_eval) <- c("Max", "Min")

# Assemblage des bornes et des données stations
df_I2M2_eval <- rbind(max_min_eval, I2M2_eval)
df_I2M2_eval

# Fonction personnalisée pour produire un diagramme radar harmonisé
radar_Amont_upgrade <- function(data, color = "#00AFBB", 
                                vlabels = colnames(data), vlcex = 0.7,
                                caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Polygone représentant les valeurs des métriques
    pcol = color, pfcol = scales::alpha(color, 0.3), plwd = 1.5, plty = 1,
    # Grille de fond
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Axe
    axislabcol = "grey", 
    # Étiquettes des métriques
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Ajustement des marges graphiques
op <- par(mar = c(1, 2, 2, 2))

# Diagramme radar comparatif des métriques I2M2 (amont vs aval)
radar_Amont_upgrade(
  data = df_I2M2_eval,
  caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#00AFBB", "#E7B800")
)

# Ajout de la légende des stations
legend(
  x = "bottom",
  legend = rownames(df_I2M2_eval[-c(1,2),]),
  horiz = TRUE,
  bty = "n",
  pch = 20,
  col = c("#00AFBB", "#E7B800"),
  text.col = "black",
  cex = 1,
  pt.cex = 1.5
)
par(op)

######################################
# 1.2 Outil de diagnostic de l’I2M2
######################################

# Probabilités d’impact des pressions anthropiques estimées par l’outil de diagnostic
I2M2_diag <- data.frame(
  row.names = c("Amont", "Aval"),
  Matières_organiques = c(0.0735, 0.0707),
  Matières_phosphorées = c(0.0247, 0.0428),
  Matières_azotées = c(0.0283, 0.0332),
  Nitrates = c(0.2189, 0.2088),
  HAP = c(0.4571, 0.3896),
  Pesticides = c(0.8102, 0.6709),
  Ripisylve = c(0.3939, 0.4308),
  Voies_de_communication = c(0.2507,0.2943),
  Urbanisation = c(0.2465, 0.3517),
  Risque_de_colmatage = c(0.1793, 0.2038),
  Instabilité_hydrologique = c(0.2217, 0.2775),
  Anthropisation_BV = c(0.5493, 0.6045)
)
I2M2_diag

# Bornes 0–1 pour le radar de diagnostic
max_min_diag <- data.frame(
  Matières_organiques = c(1, 0),
  Matières_phosphorées = c(1, 0),
  Matières_azotées = c(1, 0),
  Nitrates = c(1, 0),
  HAP = c(1, 0),
  Pesticides = c(1, 0),
  Ripisylve = c(1, 0),
  Voies_de_communication = c(1, 0),
  Urbanisation = c(1, 0),
  Risque_de_colmatage = c(1, 0),
  Instabilité_hydrologique = c(1, 0),
  Anthropisation_BV = c(1, 0)
)
rownames(max_min_diag) <- c("Max", "Min")

# Mise en forme des données pour ggplot
df2_I2M2_diag <- t(I2M2_diag) %>%
  as.data.frame() %>%
  rownames_to_column("Pressions")

# Diagramme en points comparatif des pressions (amont / aval)
df3_I2M2_diag <- df2_I2M2_diag %>%
  select(Pressions, Amont, Aval) %>%
  pivot_longer(
    cols = c(Amont, Aval),
    names_to = "Stations",
    values_to = "Probabilités d'impact"
  )

ggdotchart(
  df3_I2M2_diag,
  x = "Pressions",
  y = "Probabilités d'impact",
  group = "Stations",
  color = "Stations",
  palette = c("#00AFBB", "#E7B800"),
  add = "segment",
  position = position_dodge(0.3),
  sorting = "descending",
  rotate = TRUE
)

######################################
# 1.3 Évolution temporelle de l’I2M2
######################################

# Importation des données temporelles d’EQR
tempo <- read_excel("~/Master/M2/Bioindication/Milieux continentaux/I2M2 ~ IBMR/Datasets/tempo.xlsx")

tempo <- tempo %>%
  rename(Station = `Station`, Annee = `Annee`, EQR = `EQR`)

# Définition des classes d'état et des couleurs associées
classes <- data.frame(
  min = c(0, 0.153, 0.306, 0.46, 0.665),
  max = c(0.153, 0.306, 0.46, 0.665, 1),
  Etat = factor(c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon"),
                levels = c("Mauvais", "Médiocre", "Moyen", "Bon", "Très bon")),
  Couleur = c("#FF0000", "#FFA500", "#FFFF00", "#90EE90", "#008000")
)

# Graphique d’évolution temporelle avec classes d’état en arrière-plan
t <- ggplot(tempo, aes(x = Annee, y = EQR, color = Station, group = Station)) +
  
  # Ajout des couleurs de classes en arrière-plan
  annotate("rect", xmin = min(tempo$Annee), xmax = max(tempo$Annee),
           ymin = 0, ymax = 0.153, fill = "#FF0000", alpha = 0.2) +
  annotate("rect", xmin = min(tempo$Annee), xmax = max(tempo$Annee),
           ymin = 0.153, ymax = 0.306, fill = "#FFA500", alpha = 0.2) +
  annotate("rect", xmin = min(tempo$Annee), xmax = max(tempo$Annee),
           ymin = 0.306, ymax = 0.46, fill = "#FFFF00", alpha = 0.2) +
  annotate("rect", xmin = min(tempo$Annee), xmax = max(tempo$Annee),
           ymin = 0.46, ymax = 0.665, fill = "#90EE90", alpha = 0.2) +
  annotate("rect", xmin = min(tempo$Annee), xmax = max(tempo$Annee),
           ymin = 0.665, ymax = 1, fill = "#008000", alpha = 0.2) +
  
  # Ajout des lignes pour les valeurs EQR par station
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  # Ajout des limites des classes en pointillés
  geom_hline(yintercept = c(0.153, 0.306, 0.46, 0.665), linetype = "dashed", color = "gray", alpha = 0.5) +
  # Ajout des lignes verticales de quadrillage fines
  geom_vline(xintercept = unique(tempo$Annee), linetype = "dotted", color = "gray60", alpha = 0.7, size = 0.2) +
  # Échelle et labels pour l'axe des abscisses (toutes les années)
  scale_x_continuous(breaks = unique(tempo$Annee)) +
  # Échelle et labels pour l'axe des ordonnées
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.153, 0.306, 0.46, 0.665, 1),
                     labels = c("0", "0.153", "0.306", "0.46", "0.665", "1")) +
  labs(
    title = "Évolution de l'EQR (I2M2) par station depuis 2012",
    x = "Année",
    y = "Valeur EQR",
    color = "Station"
  ) +
  # Légende pour les couleurs des stations
  scale_color_manual(values = c("Amont" = "#00AFBB", "Aval" = "#E7B800")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotation des étiquettes des années
  )

print(t)

################################################
# PARTIE 2 – GRAPHIQUES RELATIFS À L’IBMR
################################################

######################################
# 2.1 Recouvrement des macrophytes
######################################

# Charger les données
recouvrement <- read_excel("IBMR/recouvrement.xlsx")

# Regrouper les taxons avec Recouvrement < 0.2 en "Autres"
recouvrement_modifie <- recouvrement %>%
  mutate(Taxon = ifelse(Recouvrement < 0.2, "Autres", Taxon)) %>%
  group_by(Station, Taxon) %>%
  summarise(Recouvrement = sum(Recouvrement), .groups = "drop")

# Trier les taxons par ordre croissant de recouvrement (par station)
recouvrement_modifie <- recouvrement_modifie %>%
  group_by(Station) %>%
  mutate(Taxon = reorder(Taxon, Recouvrement)) %>%
  arrange(Station, Recouvrement)

# Créer une palette de couleurs personnalisée
couleurs_taxons <- c(
  "HYAFLU" = "#1F77B4", "HILSPX" = "#FF7F0E", "RHYRIP" = "#2CA02C",
  "VAUSPX" = "#D62728", "Autres" = "black",  # "Autres" en noir
  "CLASpx" = "#9467BD", "CHIPOL" = "#8C564B", "BRARIV" = "#E377C2",
  "FISGRN" = "#7F7F7F", "AGRSTO" = "#BCBD22", "PELEND" = "#17BECF",
  "FISCRA" = "#AEC7E8", "LEORIP" = "#FFBB78", "CINAQU" = "#98DF8A",
  "CRAFIL" = "#FF9896", "FONANT" = "#C5B0D5", "CINFON" = "#C49C94"
)

# Générer le graphique
p <- ggplot(recouvrement_modifie, aes(x = Station, y = Recouvrement, fill = Taxon)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = couleurs_taxons) +
  labs(
    x = "Stations",
    y = "Recouvrement (%)",
    fill = "Taxons"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "right"
  )

# Afficher le graphique
print(p)
