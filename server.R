# ---- server.R ----
# projet ulule - bryan desjardins et julianne festoc
# master 1 dsms

# on charge les packages
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)

# ---- chargement des données ----

# on charge le fichier csv
data_ulule <- read_csv("data/data_ulule_2025.csv", show_col_types = FALSE)

# on prépare les données avec dplyr
data_ulule <- data_ulule %>%
  mutate(
    # on convertit les dates
    date_start = as.Date(date_start),
    date_end = as.Date(date_end),
    
    # on extrait l'année et le trimestre pour les graphiques
    annee = year(date_start),
    trimestre = quarter(date_start),
    
    # on crée une colonne "2020 T1", "2020 T2", etc pour l'axe x
    annee_trimestre = paste0(annee, " T", trimestre)
  )

# on filtre pour garder que ce qui nous intéresse
data_ulule <- data_ulule %>%
  filter(
    is_cancelled == FALSE,        # on enlève les campagnes annulées
    annee >= 2020,                # on garde que depuis 2020
    !is.na(date_start)            # on enlève les dates manquantes
  )

# ---- conversion des devises en euros ----

# on utilise priceR pour récupérer les taux de change
# ça s'exécute une seule fois au démarrage de l'appli

library(priceR)

# les devises qu'on doit convertir (y'a pas que des euros dans le fichier)
devises_a_convertir <- c("USD", "CAD", "GBP", "CHF", "BRL", "AUD", "DKK", "SEK", "NOK")

# on récupère les taux actuels
# on met un trycatch au cas où l'api répond pas
cat("Récupération des taux de change...\n")

taux_de_change <- tryCatch({
  
  # tableau vide pour stocker les taux
  taux <- data.frame(
    currency = character(),
    taux_vers_eur = numeric()
  )
  
  # on boucle sur chaque devise pour récupérer son taux
  for (devise in devises_a_convertir) {
    
    # exchange_rate_latest() donne le taux de conversion
    # ex: 1 USD = 0.92 EUR
    taux_actuel <- exchange_rate_latest(devise)
    taux_eur <- taux_actuel[["EUR"]]
    
    # on ajoute au tableau
    taux <- rbind(taux, data.frame(
      currency = devise,
      taux_vers_eur = taux_eur
    ))
    
    cat(paste0("  ", devise, " -> EUR : ", round(taux_eur, 4), "\n"))
  }
  
  # on ajoute EUR avec taux = 1 (pas besoin de convertir)
  taux <- rbind(taux, data.frame(currency = "EUR", taux_vers_eur = 1))
  
  taux
  
}, error = function(e) {
  # si l'api marche pas, on utilise des taux fixes de secours
  cat("Erreur API - utilisation des taux de secours\n")
  
  data.frame(
    currency = c("EUR", "USD", "CAD", "GBP", "CHF", "BRL", "AUD", "DKK", "SEK", "NOK"),
    taux_vers_eur = c(1, 0.92, 0.68, 1.17, 1.05, 0.18, 0.61, 0.13, 0.09, 0.09)
  )
})

cat("Taux de change récupérés avec succès !\n\n")

# on affiche pour vérifier que ça a marché
print(taux_de_change)

# ---- on applique les taux aux données ----

# on fait un left_join pour récupérer le taux de chaque campagne
data_ulule <- data_ulule %>%
  left_join(taux_de_change, by = "currency") %>%
  mutate(
    # on convertit le montant levé en euros
    montant_eur = amount_raised * taux_vers_eur,
    
    # pareil pour l'objectif
    objectif_eur = goal * taux_vers_eur
  )

# petit check pour voir si ça marche
cat("\nExemples de conversions :\n")
data_ulule %>%
  filter(currency != "EUR") %>%
  select(currency, amount_raised, taux_vers_eur, montant_eur) %>%
  head(5) %>%
  print()

# on récupère la liste des catégories pour les menus déroulants
liste_categories <- data_ulule %>%
  filter(!is.na(category)) %>%
  distinct(category) %>%
  arrange(category) %>%
  pull(category)

# on affiche dans la console pour vérifier
cat("Nombre de campagnes chargées :", nrow(data_ulule), "\n")
cat("Nombre de catégories :", length(liste_categories), "\n")
# ---- fonction server ----

function(input, output, session) {
  
  # ---- variables réactives pour les boutons d'inversion ----
  # reactiveVal ça permet de stocker une valeur qui change
  
  # false = top 10, true = flop 10
  afficher_flop_categories <- reactiveVal(FALSE)
  
  # false = taux de réussite, true = taux d'échec
  afficher_taux_echec <- reactiveVal(FALSE)
  
  # ---- initialisation du menu des catégories ----
  
  observe({
    # on met à jour le selectinput avec les catégories
    # par défaut on sélectionne les 5 premières
    updateSelectizeInput(
      session,
      "categories",
      choices = liste_categories,
      selected = head(liste_categories, 5)
    )
  })
  
  # ---- couleurs selon le thème ----
  # cette fonction retourne les bonnes couleurs selon le thème choisi
  
  couleurs_theme <- reactive({
    # on récupère le thème actuel (envoyé par le javascript)
    # par défaut "sombre" si pas encore défini
    theme <- if (is.null(input$theme_actuel)) "sombre" else input$theme_actuel
    
    if (theme == "clair") {
      # couleurs pour le thème clair
      list(
        texte = "#1e293b",
        texte_secondaire = "#475569",
        fond = "white",
        grille = "#e2e8f0"
      )
    } else {
      # couleurs pour le thème sombre
      list(
        texte = "#cbd5e1",
        texte_secondaire = "#94a3b8",
        fond = "transparent",
        grille = "rgba(148, 163, 184, 0.1)"
      )
    }
  })
  
  # ---- données filtrées ----
  # cette fonction se met à jour automatiquement quand les filtres changent
  
  donnees_filtrees <- reactive({
    # on attend que les inputs soient dispo
    req(input$categories, input$date_range)
    
    # si aucune catégorie sélectionnée on retourne un tableau vide
    if (length(input$categories) == 0) {
      return(data_ulule[0, ])
    }
    
    # on filtre les données
    resultat <- data_ulule %>%
      filter(
        category %in% input$categories,
        date_start >= input$date_range[1],
        date_start <= input$date_range[2]
      )
    
    return(resultat)
  })
  
  # ---- agrégation par trimestre (pour le graphique d'évolution) ----
  
  donnees_par_trimestre <- reactive({
    donnees_filtrees() %>%
      group_by(annee_trimestre) %>%
      summarise(
        nb_campagnes = n(),
        nb_reussies = sum(goal_raised, na.rm = TRUE),
        montant_total = sum(montant_eur, na.rm = TRUE),
        taux_reussite = round((nb_reussies / nb_campagnes) * 100, 1)
      ) %>%
      arrange(annee_trimestre)
  })
  
  # ---- agrégation par catégorie ----
  
  donnees_par_categorie <- reactive({
    donnees_filtrees() %>%
      group_by(category) %>%
      summarise(
        nb_campagnes = n(),
        nb_reussies = sum(goal_raised, na.rm = TRUE),
        montant_total = sum(montant_eur, na.rm = TRUE),
        taux_reussite = round((nb_reussies / nb_campagnes) * 100, 1)
      ) %>%
      arrange(desc(nb_campagnes))
  })
  
  # ---- indicateurs kpi (les 4 cartes en haut) ----
  
  # kpi 1 : nombre total de campagnes
  output$kpi_total <- renderText({
    format(nrow(donnees_filtrees()), big.mark = " ")
  })
  
  # kpi 2 : nombre de campagnes réussies
  output$kpi_success <- renderText({
    nb <- sum(donnees_filtrees()$goal_raised, na.rm = TRUE)
    format(nb, big.mark = " ")
  })
  
  # kpi 3 : montant total levé
  output$kpi_amount <- renderText({
    total <- sum(donnees_filtrees()$montant_eur, na.rm = TRUE)
    # on divise par 1 million et on ajoute "M€"
    paste0(format(round(total / 1000000, 1), big.mark = " "), "M€")
  })
  
  # kpi 4 : taux de réussite
  output$kpi_rate <- renderText({
    total <- nrow(donnees_filtrees())
    reussies <- sum(donnees_filtrees()$goal_raised, na.rm = TRUE)
    
    if (total > 0) {
      taux <- round((reussies / total) * 100, 1)
      paste0(taux, "%")
    } else {
      "0%"
    }
  })
  
  
  # ---- graphique 1 : évolution trimestrielle ----
  
  
  output$plot_evolution <- renderPlotly({
    
    data <- donnees_par_trimestre()
    couleurs <- couleurs_theme()  # on récupère les couleurs du thème
    
    # on choisit quelle colonne afficher selon l'indicateur
    if (input$indicateur == "count") {
      colonne_y <- "nb_campagnes"
      titre_y <- "Nombre de campagnes"
    } else if (input$indicateur == "success") {
      colonne_y <- "nb_reussies"
      titre_y <- "Campagnes réussies"
    } else if (input$indicateur == "amount") {
      colonne_y <- "montant_total"
      titre_y <- "Montant (EUR)"
    } else {
      colonne_y <- "taux_reussite"
      titre_y <- "Taux (%)"
    }
    
    # on crée le graphique avec plotly
    plot_ly(
      data,
      x = ~annee_trimestre,
      y = as.formula(paste0("~", colonne_y)),
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#60a5fa", width = 3),
      marker = list(size = 8, color = "#60a5fa")
    ) %>%
      layout(
        xaxis = list(
          title = "", 
          tickangle = -45, 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        yaxis = list(
          title = titre_y, 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        plot_bgcolor = couleurs$fond,
        paper_bgcolor = couleurs$fond,
        font = list(color = couleurs$texte)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ---- graphique 2 : top/flop 10 catégories ----
  
  
  output$plot_categories <- renderPlotly({
    
    couleurs <- couleurs_theme()
    
    # selon le mode (top ou flop), on trie différemment
    if (afficher_flop_categories()) {
      # flop 10 : les catégories avec le moins de campagnes
      data <- donnees_par_categorie() %>%
        arrange(nb_campagnes) %>%  # tri croissant
        head(10) %>%
        # pour le flop, on veut les plus petits en haut
        mutate(category_ord = reorder(category, -nb_campagnes))
      couleur_barres <- "#ef4444"  # rouge pour le flop
    } else {
      # top 10 : les catégories avec le plus de campagnes
      data <- donnees_par_categorie() %>%
        arrange(desc(nb_campagnes)) %>%  # tri décroissant
        head(10) %>%
        # pour le top, on veut les plus grands en haut
        mutate(category_ord = reorder(category, nb_campagnes))
      couleur_barres <- "#3b82f6"  # bleu pour le top
    }
    
    plot_ly(
      data,
      y = ~category_ord,
      x = ~nb_campagnes,
      type = "bar",
      orientation = "h",
      marker = list(color = couleur_barres)
    ) %>%
      layout(
        xaxis = list(
          title = "Nombre", 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        yaxis = list(
          title = "", 
          color = couleurs$texte
        ),
        plot_bgcolor = couleurs$fond,
        paper_bgcolor = couleurs$fond,
        font = list(color = couleurs$texte)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ---- graphique 3 : distribution des montants ----
  
  
  output$plot_distribution <- renderPlotly({
    
    # on filtre les valeurs extrêmes pour que ce soit lisible
    # on garde que les montants en dessous du 95e percentile
    seuil <- quantile(donnees_filtrees()$montant_eur, 0.95, na.rm = TRUE)
    
    data <- donnees_filtrees() %>%
      filter(montant_eur > 0, montant_eur < seuil)
    
    couleurs <- couleurs_theme()  # on récupère les couleurs du thème
    
    # couleur de bordure selon le thème
    theme <- if (is.null(input$theme_actuel)) "sombre" else input$theme_actuel
    bordure <- if (theme == "clair") "#e2e8f0" else "#1e293b"
    
    plot_ly(
      data,
      x = ~montant_eur,
      type = "histogram",
      marker = list(
        color = "#60a5fa",
        line = list(color = bordure, width = 1)
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Montant (EUR)", 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        yaxis = list(
          title = "Fréquence", 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        bargap = 0.15,
        plot_bgcolor = couleurs$fond,
        paper_bgcolor = couleurs$fond,
        font = list(color = couleurs$texte)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ---- graphique 4 : taux de réussite/échec par catégorie ----
  
  
  output$plot_success_rate <- renderPlotly({
    
    couleurs <- couleurs_theme()
    
    if (afficher_taux_echec()) {
      # taux d'échec = 100 - taux de réussite
      data <- donnees_par_categorie() %>%
        mutate(taux_affiche = 100 - taux_reussite) %>%
        arrange(desc(taux_affiche)) %>%
        head(10)
      
      # couleurs inversées : rouge si échec élevé, vert si faible
      couleurs_barres <- ifelse(
        data$taux_affiche >= 60, "#ef4444",  # rouge si >= 60% d'échec
        ifelse(data$taux_affiche >= 40, "#f59e0b", "#10b981")  # orange ou vert
      )
      titre_axe <- "Taux d'échec (%)"
      
    } else {
      # taux de réussite normal
      data <- donnees_par_categorie() %>%
        mutate(taux_affiche = taux_reussite) %>%
        arrange(desc(taux_affiche)) %>%
        head(10)
      
      # couleurs : vert si réussite élevée, rouge si faible
      couleurs_barres <- ifelse(
        data$taux_affiche >= 60, "#10b981",
        ifelse(data$taux_affiche >= 40, "#f59e0b", "#ef4444")
      )
      titre_axe <- "Taux de réussite (%)"
    }
    
    plot_ly(
      data,
      x = ~taux_affiche,
      y = ~reorder(category, taux_affiche),
      type = "bar",
      orientation = "h",
      marker = list(color = couleurs_barres)
    ) %>%
      layout(
        xaxis = list(
          title = titre_axe, 
          color = couleurs$texte,
          gridcolor = couleurs$grille
        ),
        yaxis = list(
          title = "", 
          color = couleurs$texte
        ),
        plot_bgcolor = couleurs$fond,
        paper_bgcolor = couleurs$fond,
        font = list(color = couleurs$texte)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # ---- tableau des données ----
  
  
  # on initialise le selectinput des catégories
  observe({
    updateSelectizeInput(
      session,
      "categories_tableau",
      choices = liste_categories,
      selected = liste_categories  # par défaut tout est sélectionné
    )
  })
  
  # bouton "tout sélectionner"
  observeEvent(input$btn_select_all_cat, {
    updateSelectizeInput(
      session,
      "categories_tableau",
      selected = liste_categories
    )
  })
  
  # bouton "tout désélectionner"
  observeEvent(input$btn_deselect_all_cat, {
    updateSelectizeInput(
      session,
      "categories_tableau",
      selected = character(0)  # vide
    )
  })
  
  # données filtrées pour le tableau
  donnees_tableau <- reactive({
    
    # si aucune catégorie sélectionnée on affiche rien
    if (is.null(input$categories_tableau) || length(input$categories_tableau) == 0) {
      return(data_ulule[0, ])  # retourne un tableau vide
    }
    
    # sinon on filtre par les catégories sélectionnées
    data_ulule %>%
      filter(category %in% input$categories_tableau)
  })
  
  output$table_campaigns <- renderDT({
    
    # on sélectionne les colonnes à afficher
    tableau <- donnees_tableau() %>%
      select(
        Titre = absolute_url,
        Catégorie = category,
        Début = date_start,
        Fin = date_end,
        `Objectif (EUR)` = objectif_eur,
        `Levé (EUR)` = montant_eur,
        `Réussi` = goal_raised,
        Pays = country
      ) %>%
      mutate(
        `Objectif (EUR)` = round(`Objectif (EUR)`, 0),
        `Levé (EUR)` = round(`Levé (EUR)`, 0),
        
        # important : on convertit en facteurs pour avoir des menus déroulants
        Catégorie = as.factor(Catégorie),
        `Réussi` = factor(
          ifelse(`Réussi`, "Oui", "Non"),
          levels = c("Oui", "Non")
        ),
        Pays = as.factor(Pays)
      )
    
    # on crée le tableau interactif
    datatable(
      tableau,
      filter = "top",
      options = list(
        pageLength = 15,
        language = list(
          search = "Rechercher:",
          lengthMenu = "Afficher _MENU_ lignes",
          info = "_START_ à _END_ sur _TOTAL_"
        ),
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(c("Objectif (EUR)", "Levé (EUR)"), currency = "€", digits = 0)
    
  }, server = FALSE)
  
  
  # ---- téléchargement csv ----
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ulule_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # on exporte les données filtrées
      export <- donnees_tableau() %>%
        select(id, absolute_url, category, date_start, date_end,
               country, goal_raised, montant_eur, objectif_eur, percent)
      write_csv(export, file)
    }
  )
  
  
  # ---- statistiques globales (page à propos) ----
  
  
  output$stats_globales <- renderUI({
    
    # calculs sur toutes les données (pas filtrées)
    total <- nrow(data_ulule)
    reussies <- sum(data_ulule$goal_raised, na.rm = TRUE)
    montant <- sum(data_ulule$montant_eur, na.rm = TRUE)
    moyenne <- mean(data_ulule$montant_eur, na.rm = TRUE)
    taux <- round((reussies / total) * 100, 1)
    
    # on crée le html
    tagList(
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Total campagnes:"), tags$br(),
        tags$span(
          class = "stat-value",  # classe css qui suit le thème
          format(total, big.mark = " ")
        )
      ),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Montant levé:"), tags$br(),
        tags$span(
          class = "stat-value",
          paste0(format(round(montant / 1000000, 1), big.mark = " "), "M€")
        )
      ),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Montant moyen:"), tags$br(),
        tags$span(
          class = "stat-value",
          paste0(format(round(moyenne, 0), big.mark = " "), "€")
        )
      ),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Taux réussite:"), tags$br(),
        tags$span(
          class = "stat-value",
          paste0(taux, "%")
        )
      )
    )
  })
  
  
  # ---- top 5 catégories ----
  
  
  output$top_categories <- renderUI({
    
    # on compte par catégorie et on prend le top 5
    top5 <- data_ulule %>%
      count(category, sort = TRUE) %>%
      head(5)
    
    # on crée la liste html
    tagList(
      lapply(1:nrow(top5), function(i) {
        tags$div(
          style = "margin-bottom: 10px; padding: 10px; 
                   background: rgba(59, 130, 246, 0.1); 
                   border-radius: 8px;",
          tags$div(
            class = "category-name",
            style = "font-weight: 600;",
            paste0(i, ". ", top5$category[i])
          ),
          tags$div(
            class = "category-count",
            style = "font-size: 0.9em;",
            paste0(format(top5$n[i], big.mark = " "), " campagnes")
          )
        )
      })
    )
  })
  
  
  # ---- bouton réinitialiser ----
  
  
  observeEvent(input$reset_filters, {
    # on remet les valeurs par défaut
    updateSelectizeInput(session, "categories", selected = head(liste_categories, 5))
    updateSelectInput(session, "indicateur", selected = "count")
    updateDateRangeInput(session, "date_range", start = "2020-01-01", end = Sys.Date())
  })
  
  
  # ---- boutons tout sélectionner / désélectionner ----
  
  
  # quand on clique sur "tout sélectionner"
  observeEvent(input$btn_tout_selectionner, {
    updateSelectizeInput(session, "categories", selected = liste_categories)
  })
  
  # quand on clique sur "tout désélectionner"
  observeEvent(input$btn_tout_deselectionner, {
    updateSelectizeInput(session, "categories", selected = character(0))
  })
  
  
  # ---- boutons d'inversion des graphiques ----
  
  
  # quand on clique sur le bouton d'inversion des catégories
  observeEvent(input$btn_inverser_categories, {
    # on inverse la valeur actuelle
    afficher_flop_categories(!afficher_flop_categories())
  })
  
  # quand on clique sur le bouton d'inversion du taux
  observeEvent(input$btn_inverser_taux, {
    afficher_taux_echec(!afficher_taux_echec())
  })
  
  # titre dynamique pour le graphique des catégories
  output$titre_categories <- renderText({
    if (afficher_flop_categories()) {
      "Flop 10 Catégories"
    } else {
      "Top 10 Catégories"
    }
  })
  
  # titre dynamique pour le graphique des taux
  output$titre_taux <- renderText({
    if (afficher_taux_echec()) {
      "Taux d'Échec par Catégorie"
    } else {
      "Taux de Réussite par Catégorie"
    }
  })
  
  
  
  # ---- page découverte libre ----
  
  
  
  # données filtrées par la période choisie
  
  
  donnees_periode_libre <- reactive({
    data_ulule %>%
      filter(
        date_start >= input$periode_libre[1],
        date_start <= input$periode_libre[2]
      )
  })
  
  
  # listes des types de variables
  
  
  # variable temporelle (pour les évolutions)
  variable_temporelle <- c("annee_trimestre")
  
  # variables catégorielles classiques
  variables_categorielles <- c("category", "country", "lang", "goal_raised")
  
  # variables numériques (données brutes par campagne)
  variables_numeriques <- c("montant_eur", "objectif_eur", "percent", "nb_days", "comment_counts", "news_count")
  
  # indicateurs agrégés (pour les évolutions temporelles)
  indicateurs_agreges <- c("count", "nb_reussies", "montant_eur", "objectif_eur", "taux_reussite")
  
  
  # fonction pour déterminer les types de graphiques possibles
  
  
  trouver_types_graphiques <- function(var_x, var_y) {
    
    # si aucune variable x on retourne rien
    if (is.null(var_x) || var_x == "") {
      return(c("-- Choisir une variable --" = ""))
    }
    
    # --- CAS SPECIAL : TRIMESTRE (évolution temporelle) ---
    if (var_x == "annee_trimestre") {
      
      # si pas de y ou y est un indicateur agrégé -> évolution temporelle
      if (is.null(var_y) || var_y == "" || var_y %in% indicateurs_agreges) {
        return(c("Lignes", "Barres"))
      }
      
      # sinon combinaison non supportée
      return(c("-- Combinaison non supportée --" = ""))
    }
    
    # --- VARIABLE X CATEGORIELLE ---
    if (var_x %in% variables_categorielles) {
      
      # pas de y -> répartition simple
      if (is.null(var_y) || var_y == "" || var_y == "count") {
        return(c("Barres", "Camembert"))
      }
      
      # y numérique -> barres avec moyenne
      if (var_y %in% variables_numeriques) {
        return(c("Barres (moyenne)"))
      }
      
      # y catégorielle -> non supporté
      if (var_y %in% variables_categorielles) {
        return(c("-- Combinaison non supportée --" = ""))
      }
    }
    
    # --- VARIABLE X NUMERIQUE ---
    if (var_x %in% variables_numeriques) {
      
      # pas de y -> distribution
      if (is.null(var_y) || var_y == "" || var_y == "count") {
        return(c("Histogramme", "Boxplot"))
      }
      
      # y numérique -> scatter
      if (var_y %in% variables_numeriques) {
        return(c("Nuage de points"))
      }
      
      # y catégorielle -> boxplot comparatif
      if (var_y %in% variables_categorielles) {
        return(c("Boxplot comparatif"))
      }
    }
    
    # par défaut
    return(c("-- Choisir une variable --" = ""))
  }
  
  
  # fonction générique pour créer un graphique
  
  
  creer_graphique <- function(data, var_x, var_y, type_graphique, couleurs) {
    
    # --- MESSAGE SI AUCUNE VARIABLE X ---
    if (is.null(var_x) || var_x == "") {
      return(
        plot_ly() %>%
          layout(
            title = list(
              text = "Sélectionnez une variable X",
              font = list(color = couleurs$texte, size = 14)
            ),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    # --- MESSAGE SI AUCUN TYPE ---
    if (is.null(type_graphique) || type_graphique == "" || 
        grepl("--", type_graphique)) {
      return(
        plot_ly() %>%
          layout(
            title = list(
              text = "Sélectionnez un type de graphique",
              font = list(color = couleurs$texte, size = 14)
            ),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    
    # graphiques temporels (trimestre en x)
    
    
    if (var_x == "annee_trimestre") {
      
      # on détermine quel indicateur afficher
      indicateur <- ifelse(is.null(var_y) || var_y == "", "count", var_y)
      
      # on agrège par trimestre
      data_trimestre <- data %>%
        group_by(annee_trimestre) %>%
        summarise(
          nb_campagnes = n(),
          nb_reussies = sum(goal_raised, na.rm = TRUE),
          montant_total = sum(montant_eur, na.rm = TRUE),
          objectif_total = sum(objectif_eur, na.rm = TRUE),
          taux_reussite = round(sum(goal_raised, na.rm = TRUE) / n() * 100, 1)
        ) %>%
        arrange(annee_trimestre)
      
      # on choisit la colonne selon l'indicateur
      if (indicateur == "count") {
        data_trimestre$valeur <- data_trimestre$nb_campagnes
        titre_y <- "Nombre de campagnes"
      } else if (indicateur == "nb_reussies") {
        data_trimestre$valeur <- data_trimestre$nb_reussies
        titre_y <- "Nombre de campagnes réussies"
      } else if (indicateur == "montant_eur") {
        data_trimestre$valeur <- data_trimestre$montant_total
        titre_y <- "Montant total (EUR)"
      } else if (indicateur == "objectif_eur") {
        data_trimestre$valeur <- data_trimestre$objectif_total
        titre_y <- "Objectif total (EUR)"
      } else if (indicateur == "taux_reussite") {
        data_trimestre$valeur <- data_trimestre$taux_reussite
        titre_y <- "Taux de réussite (%)"
      } else {
        data_trimestre$valeur <- data_trimestre$nb_campagnes
        titre_y <- "Nombre de campagnes"
      }
      
      # on crée le graphique
      if (type_graphique == "Lignes") {
        plot_ly(
          data_trimestre,
          x = ~annee_trimestre,
          y = ~valeur,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#3b82f6", width = 2),
          marker = list(color = "#3b82f6", size = 8)
        ) %>%
          layout(
            xaxis = list(title = "Trimestre", color = couleurs$texte, tickangle = 45),
            yaxis = list(title = titre_y, color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
        
      } else if (type_graphique == "Barres") {
        plot_ly(
          data_trimestre,
          x = ~annee_trimestre,
          y = ~valeur,
          type = "bar",
          marker = list(color = "#3b82f6")
        ) %>%
          layout(
            xaxis = list(title = "Trimestre", color = couleurs$texte, tickangle = 45),
            yaxis = list(title = titre_y, color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
    }
    
    
    # graphiques à 1 variable
    
    
    else if (is.null(var_y) || var_y == "" || var_y == "count") {
      
      # --- barres (variable catégorielle) ---
      if (type_graphique == "Barres") {
        
        if (var_x == "goal_raised") {
          data_agregee <- data %>%
            mutate(valeur = ifelse(goal_raised, "Réussie", "Échouée")) %>%
            count(valeur, sort = TRUE)
        } else {
          data_agregee <- data %>%
            count(!!sym(var_x), sort = TRUE) %>%
            rename(valeur = !!sym(var_x))
        }
        
        data_agregee <- head(data_agregee, 10)
        
        plot_ly(
          data_agregee,
          y = ~reorder(valeur, n),
          x = ~n,
          type = "bar",
          orientation = "h",
          marker = list(color = "#3b82f6")
        ) %>%
          layout(
            xaxis = list(title = "Nombre", color = couleurs$texte, gridcolor = couleurs$grille),
            yaxis = list(title = "", color = couleurs$texte),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
      
      # --- camenbert (variable catégorielle) ---
      else if (type_graphique == "Camembert") {
        
        if (var_x == "goal_raised") {
          data_agregee <- data %>%
            mutate(valeur = ifelse(goal_raised, "Réussie", "Échouée")) %>%
            count(valeur, sort = TRUE)
        } else {
          data_agregee <- data %>%
            count(!!sym(var_x), sort = TRUE) %>%
            rename(valeur = !!sym(var_x))
        }
        
        data_agregee <- head(data_agregee, 10)
        
        plot_ly(
          data_agregee,
          labels = ~valeur,
          values = ~n,
          type = "pie",
          marker = list(colors = c("#3b82f6", "#10b981", "#f59e0b", "#ef4444", "#8b5cf6",
                                   "#06b6d4", "#ec4899", "#84cc16", "#f97316", "#6366f1"))
        ) %>%
          layout(
            paper_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte),
            legend = list(font = list(color = couleurs$texte))
          ) %>%
          config(displayModeBar = FALSE)
      }
      
      # --- histogramme (variable numérique) ---
      else if (type_graphique == "Histogramme") {
        
        valeurs <- data[[var_x]]
        seuil <- quantile(valeurs, 0.95, na.rm = TRUE)
        valeurs_filtrees <- valeurs[valeurs <= seuil & !is.na(valeurs)]
        
        plot_ly(
          x = valeurs_filtrees,
          type = "histogram",
          marker = list(color = "#3b82f6", line = list(color = "#1e40af", width = 1))
        ) %>%
          layout(
            xaxis = list(title = var_x, color = couleurs$texte, gridcolor = couleurs$grille),
            yaxis = list(title = "Fréquence", color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
      
      # --- boxplot (variable numérique) ---
      else if (type_graphique == "Boxplot") {
        
        valeurs <- data[[var_x]]
        seuil <- quantile(valeurs, 0.95, na.rm = TRUE)
        valeurs_filtrees <- valeurs[valeurs <= seuil & !is.na(valeurs)]
        
        plot_ly(
          y = valeurs_filtrees,
          type = "box",
          marker = list(color = "#3b82f6"),
          line = list(color = "#3b82f6"),
          fillcolor = "rgba(59, 130, 246, 0.5)"
        ) %>%
          layout(
            yaxis = list(title = var_x, color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
    }
    
    
    # graphiques à 2 variables
    
    
    else {
      
      # --- nuage de points (2 variables numériques) ---
      if (type_graphique == "Nuage de points") {
        
        seuil_x <- quantile(data[[var_x]], 0.95, na.rm = TRUE)
        seuil_y <- quantile(data[[var_y]], 0.95, na.rm = TRUE)
        
        data_filtre <- data %>%
          filter(
            !!sym(var_x) <= seuil_x,
            !!sym(var_y) <= seuil_y,
            !is.na(!!sym(var_x)),
            !is.na(!!sym(var_y))
          )
        
        plot_ly(
          data_filtre,
          x = ~get(var_x),
          y = ~get(var_y),
          type = "scatter",
          mode = "markers",
          marker = list(color = "#3b82f6", size = 6, opacity = 0.6)
        ) %>%
          layout(
            xaxis = list(title = var_x, color = couleurs$texte, gridcolor = couleurs$grille),
            yaxis = list(title = var_y, color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
      
      # --- boxplot pour comparer (numérique X + catégorielle Y) ---
      else if (type_graphique == "Boxplot comparatif") {
        
        seuil_x <- quantile(data[[var_x]], 0.95, na.rm = TRUE)
        
        if (var_y == "goal_raised") {
          data_filtre <- data %>%
            filter(!!sym(var_x) <= seuil_x, !is.na(!!sym(var_x))) %>%
            mutate(groupe = ifelse(goal_raised, "Réussie", "Échouée"))
        } else {
          data_filtre <- data %>%
            filter(!!sym(var_x) <= seuil_x, !is.na(!!sym(var_x))) %>%
            mutate(groupe = as.character(!!sym(var_y)))
          
          top_groupes <- data_filtre %>%
            count(groupe, sort = TRUE) %>%
            head(6) %>%
            pull(groupe)
          
          data_filtre <- data_filtre %>%
            filter(groupe %in% top_groupes)
        }
        
        plot_ly(
          data_filtre,
          x = ~groupe,
          y = ~get(var_x),
          type = "box",
          color = ~groupe,
          colors = c("#3b82f6", "#10b981", "#f59e0b", "#ef4444", "#8b5cf6", "#06b6d4")
        ) %>%
          layout(
            xaxis = list(title = var_y, color = couleurs$texte),
            yaxis = list(title = var_x, color = couleurs$texte, gridcolor = couleurs$grille),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte),
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      }
      
      # --- barres de moyenne (catégorielle X + numérique Y) ---
      else if (type_graphique == "Barres (moyenne)") {
        
        if (var_x == "goal_raised") {
          data_agregee <- data %>%
            mutate(groupe = ifelse(goal_raised, "Réussie", "Échouée")) %>%
            group_by(groupe) %>%
            summarise(moyenne = mean(!!sym(var_y), na.rm = TRUE)) %>%
            arrange(desc(moyenne))
        } else {
          data_agregee <- data %>%
            group_by(!!sym(var_x)) %>%
            summarise(moyenne = mean(!!sym(var_y), na.rm = TRUE)) %>%
            arrange(desc(moyenne)) %>%
            rename(groupe = !!sym(var_x))
          
          data_agregee <- head(data_agregee, 10)
        }
        
        plot_ly(
          data_agregee,
          y = ~reorder(groupe, moyenne),
          x = ~moyenne,
          type = "bar",
          orientation = "h",
          marker = list(color = "#3b82f6")
        ) %>%
          layout(
            xaxis = list(title = paste("Moyenne de", var_y), color = couleurs$texte, gridcolor = couleurs$grille),
            yaxis = list(title = "", color = couleurs$texte),
            paper_bgcolor = couleurs$fond,
            plot_bgcolor = couleurs$fond,
            font = list(color = couleurs$texte)
          ) %>%
          config(displayModeBar = FALSE)
      }
    }
  }
  
  
  # mise à jour des types de graphiques quand on change les variables
  
  
  # graphique 1
  observeEvent(c(input$var_x_graph1, input$var_y_graph1), {
    types <- trouver_types_graphiques(input$var_x_graph1, input$var_y_graph1)
    updateSelectInput(session, "type_graph1", choices = types)
  })
  
  # graphique 2
  observeEvent(c(input$var_x_graph2, input$var_y_graph2), {
    types <- trouver_types_graphiques(input$var_x_graph2, input$var_y_graph2)
    updateSelectInput(session, "type_graph2", choices = types)
  })
  
  # graphique 3
  observeEvent(c(input$var_x_graph3, input$var_y_graph3), {
    types <- trouver_types_graphiques(input$var_x_graph3, input$var_y_graph3)
    updateSelectInput(session, "type_graph3", choices = types)
  })
  
  # graphique 4
  observeEvent(c(input$var_x_graph4, input$var_y_graph4), {
    types <- trouver_types_graphiques(input$var_x_graph4, input$var_y_graph4)
    updateSelectInput(session, "type_graph4", choices = types)
  })
  
  
  # rendu des 4 graphiques
  
  
  output$plot_libre1 <- renderPlotly({
    creer_graphique(donnees_periode_libre(), input$var_x_graph1, input$var_y_graph1, input$type_graph1, couleurs_theme())
  })
  
  output$plot_libre2 <- renderPlotly({
    creer_graphique(donnees_periode_libre(), input$var_x_graph2, input$var_y_graph2, input$type_graph2, couleurs_theme())
  })
  
  output$plot_libre3 <- renderPlotly({
    creer_graphique(donnees_periode_libre(), input$var_x_graph3, input$var_y_graph3, input$type_graph3, couleurs_theme())
  })
  
  output$plot_libre4 <- renderPlotly({
    creer_graphique(donnees_periode_libre(), input$var_x_graph4, input$var_y_graph4, input$type_graph4, couleurs_theme())
  })
  
}