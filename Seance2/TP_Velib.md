---
title: "TP1 - Dataviz Vélib"
author: "Fabien Thibout"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE
)
```

Ce TP a pour objectif d'étudier la disponibilité des Vélib mécaniques et électriques au cours d'une journée moyenne, afin d'observer si leur répartition évolue selon l'heure.

```{r librairies}
# Chargement des librairies
setwd("C:/Users/fabien/Documents/IUT/BUT 1 SD/R")
.libPaths("Packages")

library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)
```

```{r importation}
# Définition du répertoire de travail
setwd("C:/Users/fabien/Documents/IUT/BUT 3 SD/Dataviz")


# Ici on précise que seuls les guillemets doubles sont utilisés pour encadrer du texte
# car certains noms de stations contiennent des apostrophes, comme "Jouffroy d'Abbans - Wagram", ce
# qui nous a valu pas mal d'incohérences au début de notre analyse ;)
velib <- read.table(
  "historique_stations.csv",
  sep = ",",
  header = TRUE,
  quote = "\"",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
```

On commence par préparer les données temporelles. Comme l'analyse porte sur l'évolution de la disponibilité au cours d'une journée, seule l'heure de chaque relevé est nécessaire.

```{r preparation}
# Conversion de la date au bon format afin de pouvoir récupérer les heures
velib$date <- as.POSIXct(
  velib$date,
  format = "%Y-%m-%dT%H:%MZ",
  tz = "UTC"
)


# Ici on récupère uniquement l'heure car notre analyse porte sur
# l'évolution de la disponibilité au cours d'une journée
velib <- velib %>%
  mutate(
    heure = as.integer(format(date, "%H"))
  )
```

On regroupe ensuite les relevés par heure afin d'obtenir une journée moyenne. La disponibilité moyenne des vélos mécaniques et électriques est calculée séparément.

```{r agregation}
# Ici on regroupe les relevés par heure afin d'obtenir une journée moyenne
# On calcule séparément la disponibilité moyenne des vélos mécaniques et électriques
heure_stats <- velib %>%
  group_by(heure) %>%
  summarise(
    meca_moyen = mean(available_mechanical, na.rm = TRUE),
    elec_moyen = mean(available_electrical, na.rm = TRUE),
    .groups = "drop"
  )
```

Pour comparer les deux types de vélos indépendamment du nombre total de vélos disponibles, on transforme ensuite ces valeurs en proportions.

```{r proportions}
# Ici on transforme les nombres moyens de vélos en proportions
# afin de comparer la répartition mécanique / électrique indépendamment du nombre total disponible
dispo_type_heure <- heure_stats %>%
  mutate(
    total_moyen = meca_moyen + elec_moyen,
    part_meca = meca_moyen / total_moyen * 100,
    part_elec = elec_moyen / total_moyen * 100
  )
```

Le graphique suivant représente simultanément les parts de vélos mécaniques et électriques disponibles. Les deux courbes utilisent la même échelle afin de pouvoir comparer directement leur évolution.

```{r graphique, fig.width=16, fig.height=5}
# Création du graphique principal
g1 <- ggplot(
  dispo_type_heure,
  aes(x = heure)
) +

  # La zone grisée permet de visualiser l'écart entre les deux types de vélos
  geom_ribbon(
    aes(
      ymin = part_elec,
      ymax = part_meca
    ),
    fill = "grey85",
    alpha = 0.45
  ) +

  # Courbe des vélos mécaniques
  geom_line(
    aes(
      y = part_meca,
      colour = "Vélos mécaniques"
    ),
    linewidth = 1.5
  ) +

  geom_point(
    aes(
      y = part_meca,
      colour = "Vélos mécaniques"
    ),
    size = 2.2
  ) +

  # Courbe des vélos électriques
  geom_line(
    aes(
      y = part_elec,
      colour = "Vélos électriques"
    ),
    linewidth = 1.5
  ) +

  geom_point(
    aes(
      y = part_elec,
      colour = "Vélos électriques"
    ),
    size = 2.2
  ) +

  # Une graduation toutes les deux heures permet de garder un axe lisible, surchargé sinon
  scale_x_continuous(
    breaks = seq(0, 23, 2),
    labels = function(x) paste0(x, "h")
  ) +

  # On conserve l'échelle complète de 0 à 100 % pour ne pas exagérer
  # visuellement les faibles variations observées
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    limits = c(0, 100),
    labels = function(x) paste0(x, " %")
  ) +

  # Couleurs opposées pour distinguer rapidement les deux types de vélos
  scale_colour_manual(
    values = c(
      "Vélos mécaniques" = "#2878B5",
      "Vélos électriques" = "#F28E2B"
    )
  ) +

  labs(
    x = "Heure de la journée",
    y = "Part des vélos disponibles",
    colour = NULL
  ) +

  # Utilisation d'un thème simple pour faire plutôt épuré
  theme_minimal(base_size = 13) +

  theme(
    axis.title = element_text(
      face = "bold",
      size = 11
    ),

    axis.text = element_text(
      size = 10,
      colour = "grey35"
    ),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    legend.position = "bottom",

    legend.text = element_text(
      size = 11
    ),

    plot.margin = margin(
      10, 20, 5, 20
    )
  )
```

Afin que la visualisation puisse être comprise directement, on ajoute une courte interprétation sous le graphique.

```{r interpretation}
# Texte d'interprétation directement intégré au dashboard, fait après les premières versions mais nécessaire
# pour faire plus 'propre'
texte_interpretation <- paste0(
  "La répartition reste globalement stable au cours de la journée : ",
  "les vélos mécaniques représentent environ 67 à 72 % des vélos disponibles, ",
  "contre 28 à 33 % pour les électriques. ",
  "L'écart se creuse légèrement entre 11 h et 19 h, période durant laquelle ",
  "la part des vélos électriques disponibles diminue. ",
  "À partir de 20 h, leur proportion remonte progressivement pour retrouver ",
  "en soirée un niveau proche de celui observé durant la nuit."
)


# Ici on crée un espace réservé au texte afin de pouvoir placer
# directement l'interprétation sous le graphique
interpretation <- ggplot() +

  annotate(
    "text",
    x = 0,
    y = 0.88,
    label = "À RETENIR",
    hjust = 0,
    vjust = 1,
    size = 4.8,
    fontface = "bold"
  ) +

  # str_wrap permet de faire automatiquement les retours à la ligne
  # et évite que le texte dépasse du dashboard, ce qui rendait illisible une partie de l'interprétation
  annotate(
    "text",
    x = 0,
    y = 0.62,
    label = str_wrap(
      texte_interpretation,
      width = 145
    ),
    hjust = 0,
    vjust = 1,
    size = 4,
    lineheight = 1.25
  ) +

  xlim(0, 1) +
  ylim(0, 1) +

  theme_void() +

  theme(
    plot.margin = margin(
      0, 30, 5, 30
    )
  )
```

Enfin, le graphique et son interprétation sont assemblés dans une même visualisation.

```{r dashboard, fig.width=16, fig.height=7.5}
# Assemblage du graphique et de son interprétation
# Le graphique prend volontairement plus de place car il reste l'élément principal du dashboard
dashboard_velib <- (
  g1 /
    interpretation +
    plot_layout(
      heights = c(3.2, 1.25)
    )
) +

  # Ajout du titre général, du sous-titre et de l'aide à la lecture
  # On les place ici afin de ne pas répéter le titre directement dans le graphique
  plot_annotation(
    title =
      "Électrique ou mécanique : la disponibilité change-t-elle selon l'heure ?",

    subtitle =
      "Répartition des vélos disponibles selon leur type au cours d'une journée moyenne",

    caption =
      "Lecture : à chaque heure, les parts des vélos mécaniques et électriques représentent ensemble 100 % des vélos disponibles.",

    theme = theme(
      plot.title = element_text(
        size = 23,
        face = "bold",
        margin = margin(b = 5)
      ),

      plot.subtitle = element_text(
        size = 13,
        colour = "grey35",
        margin = margin(b = 12)
      ),

      plot.caption = element_text(
        size = 9,
        colour = "grey45",
        hjust = 0,
        margin = margin(t = 8)
      ),

      plot.margin = margin(
        20, 30, 15, 30
      )
    )
  )


# Affichage explicite du dashboard pour contrôle
print(dashboard_velib)
```

La répartition reste globalement stable au cours de la journée. Les vélos mécaniques représentent environ 67 à 72 % des vélos disponibles, contre 28 à 33 % pour les vélos électriques. L'écart augmente légèrement entre 11 h et 19 h avant que la proportion de vélos électriques ne remonte à partir de 20 h.

```{r export, include=FALSE}
# Export pour rendu final
ggsave(
  filename = "dashboard_velib_electrique_mecanique.png",
  plot = dashboard_velib,
  width = 16,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
```
