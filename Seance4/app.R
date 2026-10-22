library(shiny)
library(ggplot2)

fichier <- "ventes-jeux-consoles-2006-2015.csv"

jeux <- read.csv(fichier, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
jeux$Year <- as.integer(jeux$Year)

consoles <- c(PS2 = "PlayStation 2", PS3 = "PlayStation 3", PS4 = "PlayStation 4",
              Wii = "Wii", WiiU = "Wii U", X360 = "Xbox 360", XOne = "Xbox One")
genres <- c(Action = "Action", Adventure = "Aventure", Fighting = "Combat",
            Misc = "Divers", Platform = "Plateforme", Puzzle = "Puzzle",
            Racing = "Course", `Role-Playing` = "Jeu de rôle", Shooter = "Tir",
            Simulation = "Simulation", Sports = "Sport", Strategy = "Stratégie")
jeux$Console <- unname(consoles[jeux$Platform])
jeux$Genre_fr <- unname(genres[jeux$Genre])

theme_set(theme_minimal(base_size = 13) + theme(
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  plot.title = element_text(face = "bold"),
  legend.position = "bottom"
))
bleu <- "#2563A6"
palette_consoles <- c("PlayStation 2" = "#0072B2", "PlayStation 3" = "#D55E00",
  "PlayStation 4" = "#009E73", "Wii" = "#CC79A7", "Wii U" = "#8C6300",
  "Xbox 360" = "#56B4E9", "Xbox One" = "#585858")

carte <- function(titre, explication, graphique, texte) {
  div(class = "carte", h3(titre), p(class = "explication", explication),
      plotOutput(graphique, height = if (graphique == "sorties") "460px" else "320px"), textOutput(texte))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f3f5f7; color:#233044; font-family:Arial,sans-serif; }
    .container-fluid { max-width:1500px; padding:25px; }
    h1 { font-size:30px; font-weight:700; margin-bottom:10px; }
    h3 { font-size:19px; font-weight:700; line-height:1.4; }
    .well,.carte,.chiffre { background:white; border:1px solid #dce2e8;
      border-radius:10px; box-shadow:none; }
    .carte { padding:18px; margin-bottom:20px; }
    .explication { color:#526176; min-height:40px; }
    .chiffre { padding:15px; margin-bottom:18px; }
    .chiffre strong { display:block; font-size:26px; color:#2563A6; }
    .note { padding:15px; background:#eaf0f6; border-radius:8px; margin:15px 0; }
    .shiny-text-output { line-height:1.5; }
    a { color:#165c96; }
  "))),
  h1("Les jeux vidéo : quelles différences entre les consoles ?"),
  p("Ventes des jeux sortis entre 2006 et 2015, sur sept consoles."),
  sidebarLayout(
    sidebarPanel(width = 3,
      h3("Choisir les jeux à comparer"),
      checkboxGroupInput("consoles", "Consoles", choices = sort(unname(consoles)),
                         selected = c("PlayStation 3", "Wii", "Xbox 360")),
      sliderInput("annees", "Années de sortie", min = 2006, max = 2015,
                  value = c(2006, 2015), sep = ""),
      selectInput("genre", "Genre de jeu", choices = c("Tous", sort(unname(genres))))),
    mainPanel(width = 9,
      div(class = "note", strong("Comment lire ces chiffres ?"),
          p("Les ventes sont exprimées en millions d’exemplaires. Elles concernent les jeux, pas les consoles. L’année est celle de la sortie du jeu : ce fichier ne donne pas les ventes réalisées année par année."),
          p("Une ligne correspond à un jeu sur une console. Un même titre peut donc apparaître plusieurs fois. Les consoles ont des dates de lancement différentes : leur comparaison dépend aussi de la période choisie.")),
      fluidRow(
        column(4, div(class = "chiffre", "Jeux sur une console", strong(textOutput("nombre")))),
        column(4, div(class = "chiffre", "Ventes recensées (millions)", strong(textOutput("ventes")))),
        column(4, div(class = "chiffre", "Consoles sélectionnées", strong(textOutput("nb_consoles"))))),
      fluidRow(
        column(6, carte("Sur quelles consoles les jeux se vendent-ils le plus ?",
          "On compare les ventes totales des jeux présents dans la sélection.", "comparaison", "lecture_comparaison")),
        column(6, carte("Quels éditeurs vendent le plus de jeux ?",
          "Les dix premiers éditeurs de la sélection, selon les ventes mondiales.", "editeurs", "lecture_editeurs"))),
      fluidRow(
        column(6, carte("Où les jeux se vendent-ils ?",
          "Pour chaque console, on compare la part des ventes réalisée dans chaque région.", "regions", "lecture_regions")),
        column(6, carte("Quels jeux marchent en Europe et en Amérique ?",
          "Chaque point représente un jeu sur une console. Les deux axes montrent ses ventes régionales.", "relation", "lecture_relation"))),
      fluidRow(
        column(6, carte("La plupart des jeux se vendent-ils beaucoup ?",
          "L’histogramme montre les ventes par jeu. L’échelle logarithmique permet de garder les petits jeux et les gros succès visibles.", "distribution", "lecture_distribution")),
        column(6, carte("Comment le nombre de sorties évolue-t-il ?",
          "On compte les jeux recensés par année de sortie et par console.", "sorties", "lecture_sorties"))),
      div(class = "carte", h3("D’où viennent les données ?"),
        p("Dataset Video Game Sales de Gregory Smith (Kaggle), issu de VGChartz. Sous-ensemble : PS2, PS3, PS4, Wii, Wii U, Xbox 360 et Xbox One ; années de sortie connues entre 2006 et 2015."),
        p("Il s’agit d’un ancien relevé de ventes estimées, pas d’un catalogue exhaustif. Les jeux peu vendus peuvent être absents. Les graphiques décrivent uniquement les données du fichier."),
        tags$a("Source du dataset", href = "https://www.kaggle.com/datasets/gregorut/videogamesales", target = "_blank"),
        tags$span(" · "),
        tags$a("Copie du CSV utilisée", href = "https://github.com/victor-de-paula/dataviz-videogame-sales/blob/main/vgsales.csv", target = "_blank"),
        br(), br(), downloadButton("export", "Télécharger la sélection"))
    )
  )
)

server <- function(input, output, session) {
  selection <- reactive({
    d <- jeux[jeux$Console %in% input$consoles & jeux$Year >= input$annees[1] &
                jeux$Year <= input$annees[2], ]
    if (input$genre != "Tous") d <- d[d$Genre_fr == input$genre, ]
    d
  })
  donnees <- reactive({
    d <- selection()
    validate(need(nrow(d) > 0, "Aucun jeu pour cette sélection. Choisissez une console ou élargissez les filtres."))
    d
  })
  fmt <- function(x, digits = 1) format(round(x, digits), big.mark = " ", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  totaux <- reactive(aggregate(Global_Sales ~ Console, donnees(), sum))
  output$nombre <- renderText(fmt(nrow(selection()), 0))
  output$ventes <- renderText(fmt(sum(selection()$Global_Sales)))
  output$nb_consoles <- renderText(length(unique(selection()$Console)))

  output$comparaison <- renderPlot({
    d <- totaux()
    ggplot(d, aes(Global_Sales, reorder(Console, Global_Sales))) +
      geom_col(fill = bleu, width = .65) +
      labs(x = "Ventes mondiales (millions)", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, .08)))
  })
  output$lecture_comparaison <- renderText({
    d <- totaux(); premier <- d[which.max(d$Global_Sales), ]
    paste0("Dans cette sélection, ", premier$Console, " arrive en tête avec ", fmt(premier$Global_Sales), " millions d’exemplaires.")
  })
  output$editeurs <- renderPlot({
    d <- donnees(); d <- d[!is.na(d$Publisher) & !d$Publisher %in% c("", "N/A"), ]
    validate(need(nrow(d) > 0, "Aucun éditeur renseigné."))
    a <- aggregate(Global_Sales ~ Publisher, d, sum)
    a <- head(a[order(-a$Global_Sales), ], 10)
    ggplot(a, aes(Global_Sales, reorder(Publisher, Global_Sales))) +
      geom_col(fill = bleu, width = .65) + labs(x = "Ventes mondiales (millions)", y = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0, .08)))
  })
  output$lecture_editeurs <- renderText("Les barres partent de zéro et sont classées pour faciliter la comparaison. Un éditeur peut vendre des jeux sur plusieurs consoles.")

  output$regions <- renderPlot({
    d <- donnees()
    colonnes <- c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")
    noms <- c("Amérique du Nord", "Europe", "Japon", "Autres régions")
    a <- do.call(rbind, lapply(seq_along(colonnes), function(i) {
      z <- aggregate(d[[colonnes[i]]], list(Console = d$Console), sum)
      data.frame(Console = z$Console, Region = noms[i], Ventes = z$x)
    }))
    a$Region <- factor(a$Region, levels = noms)
    ggplot(a, aes(Ventes, Console, fill = Region)) +
      geom_col(position = "fill", width = .65) +
      scale_x_continuous(labels = function(x) paste0(round(x * 100), " %"),
                         expand = c(0, 0)) +
      scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#767676")) +
      labs(x = "Part des ventes régionales", y = NULL, fill = NULL) +
      guides(fill = guide_legend(nrow = 2))
  })
  output$lecture_regions <- renderText("Chaque barre représente 100 % des ventes régionales de la console. Ce graphique compare des proportions, pas les volumes totaux.")

  output$relation <- renderPlot({
    ggplot(donnees(), aes(NA_Sales, EU_Sales, colour = Console, shape = Console)) +
      geom_point(alpha = .55, size = 2) +
      scale_colour_manual(values = palette_consoles) +
      scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8)) +
      scale_x_continuous(expand = expansion(mult = c(0, .05))) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      labs(x = "Ventes en Amérique du Nord (millions)", y = "Ventes en Europe (millions)", colour = NULL, shape = NULL) +
      guides(colour = guide_legend(nrow = 2))
  })
  output$lecture_relation <- renderText("Les points en haut à droite correspondent aux jeux très vendus dans les deux régions. Une relation entre les ventes ne prouve pas qu’une région influence l’autre.")

  output$distribution <- renderPlot({
    d <- donnees()
    ggplot(d, aes(Global_Sales)) +
      geom_histogram(binwidth = .25, boundary = -2, fill = bleu, colour = "white") +
      scale_x_log10(breaks = c(.01, .03, .1, .3, 1, 3, 10, 30, 100),
                    labels = c("0,01", "0,03", "0,1", "0,3", "1", "3", "10", "30", "100")) +
      labs(x = "Ventes mondiales (millions, échelle logarithmique)",
           y = "Nombre de jeux sur une console")
  })
  output$lecture_distribution <- renderText({
    d <- donnees()
    paste0("La moitié des jeux de la sélection se vendent à ", fmt(median(d$Global_Sales), 2),
           " million(s) d’exemplaires ou moins. Sur cet axe, les intervalles sont réguliers en logarithme : on compare des rapports de ventes, pas des écarts fixes.")
  })
  output$sorties <- renderPlot({
    d <- donnees()
    a <- aggregate(list(Nombre = rep(1, nrow(d))), list(Annee = d$Year, Console = d$Console), sum)
    grille <- expand.grid(Annee = seq(input$annees[1], input$annees[2]), Console = unique(d$Console))
    a <- merge(grille, a, all.x = TRUE)
    a$Nombre[is.na(a$Nombre)] <- 0
    g <- ggplot(a, aes(Annee, Nombre, group = Console))
    if (input$annees[1] != input$annees[2]) g <- g + geom_line(linewidth = .8, colour = bleu)
    g + geom_point(size = 1.5, colour = bleu) +
      facet_wrap(~Console, ncol = 2) +
      scale_x_continuous(breaks = seq(input$annees[1], input$annees[2], by = 2)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Année de sortie", y = "Nombre de jeux recensés")
  })
  output$lecture_sorties <- renderText("Ces courbes montrent les sorties recensées, et non les ventes annuelles. Une valeur nulle signifie qu’aucun jeu n’est présent dans la sélection pour cette année.")
  output$export <- downloadHandler(
    filename = function() "selection-jeux.csv",
    content = function(file) write.csv(selection(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

shinyApp(ui, server)
