# ---- ui.R ----
# projet ulule - bryan desjardins et julianne festoc
# master 1 dsms

# on charge les packages
library(shiny)
library(shinythemes)
library(plotly)
library(DT)

# ---- css pour le style de l'appli ----

mon_css <- "
/* police de caractères */
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap');

body {
  font-family: 'Inter', sans-serif;
  background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);
}

/* Barre de navigation */
.navbar-default {
  background: rgba(15, 23, 42, 0.95);
  border: none;
  box-shadow: 0 4px 24px rgba(0,0,0,0.3);
}

.navbar-default .navbar-brand {
  color: #f1f5f9 !important;
  font-weight: 600;
  font-size: 1.2em;
}

.navbar-default .navbar-nav > li > a {
  color: #cbd5e1 !important;
  font-weight: 500;
  padding: 12px 20px !important;
  border-radius: 10px;
}

.navbar-default .navbar-nav > li > a:hover {
  background: rgba(96, 165, 250, 0.15);
  color: #f1f5f9 !important;
}

.navbar-default .navbar-nav > .active > a {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%) !important;
  color: white !important;
}

/* Container principal */
.container-fluid {
  background: transparent !important;
  padding: 30px 20px !important;
}

/* Cartes KPI (les 4 indicateurs en haut) */
.kpi-card {
  background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
  border-radius: 16px;
  padding: 28px;
  box-shadow: 0 4px 24px rgba(0,0,0,0.4);
  border: 1px solid rgba(148, 163, 184, 0.1);
  margin-bottom: 20px;
  transition: transform 0.3s;
}

.kpi-card:hover {
  transform: translateY(-4px);
  box-shadow: 0 12px 40px rgba(59, 130, 246, 0.3);
}

.kpi-value {
  font-size: 2.5em;
  font-weight: 700;
  color: white;
  margin: 10px 0;
}

.kpi-label {
  font-size: 0.85em;
  color: #94a3b8;
  font-weight: 500;
  text-transform: uppercase;
}

.kpi-icon {
  font-size: 2em;
  color: #60a5fa;
}

/* Container pour les graphiques */
.chart-container {
  background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
  border-radius: 16px;
  padding: 28px;
  box-shadow: 0 4px 24px rgba(0,0,0,0.4);
  margin-bottom: 20px;
}

.chart-title {
  font-size: 1.6em;
  font-weight: 600;
  color: #f1f5f9;
  margin-bottom: 20px;
  border-bottom: 2px solid rgba(148, 163, 184, 0.2);
  padding-bottom: 12px;
}

/* Panel des filtres */
.filter-panel {
  background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
  border-radius: 16px;
  padding: 28px;
  box-shadow: 0 4px 24px rgba(0,0,0,0.4);
  margin-bottom: 20px;
}

.filter-panel h4 {
  color: #f1f5f9;
}

/* Boutons */
.btn-primary {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
  border: none;
  border-radius: 10px;
  padding: 12px 24px;
  font-weight: 600;
  color: white;
}

.btn-primary:hover {
  background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%);
  transform: translateY(-2px);
}

/* Petits boutons (tout sélectionner / désélectionner) */
.btn-sm {
  padding: 6px 12px;
  font-size: 0.85em;
  border-radius: 6px;
  transition: all 0.2s;
}

.btn-sm:hover {
  transform: translateY(-1px);
  opacity: 0.9;
}

/* Boutons outline pour thème sombre */
.btn-outline-primary {
  background: transparent;
  border: 1px solid #3b82f6;
  color: #3b82f6;
}

.btn-outline-primary:hover {
  background: #3b82f6;
  color: white;
}

.btn-outline-secondary {
  background: transparent;
  border: 1px solid #64748b;
  color: #94a3b8;
}

.btn-outline-secondary:hover {
  background: #64748b;
  color: white;
}

/* Boutons outline pour thème clair */
.theme-clair .btn-outline-primary {
  border-color: #2563eb;
  color: #2563eb;
}

.theme-clair .btn-outline-secondary {
  border-color: #64748b;
  color: #475569;
}

/* Menus déroulants (selectize) */
.selectize-input {
  background: rgba(15, 23, 42, 0.8) !important;
  border-radius: 10px;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  color: #f1f5f9;
  min-height: 45px;
  max-height: 80px;
  overflow-y: auto;
}

/* Les tags sélectionnés - style bleu intégré au thème */
.selectize-input .item {
  color: white !important;
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%) !important;
  border: none !important;
  padding: 6px 12px;
  border-radius: 6px;
  font-weight: 500;
  box-shadow: 0 2px 4px rgba(59, 130, 246, 0.3);
}

/* Bouton X pour supprimer un tag */
.selectize-input .item .remove {
  color: rgba(255, 255, 255, 0.7) !important;
  border-left: 1px solid rgba(255, 255, 255, 0.3) !important;
  margin-left: 8px;
  padding-left: 8px;
}

.selectize-input .item .remove:hover {
  color: white !important;
}

.selectize-input input {
  color: #f1f5f9 !important;
}

/* Menu déroulant */
.selectize-dropdown {
  background: #1e293b !important;
  border-radius: 10px;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.4);
}

.selectize-dropdown-content .option {
  color: #cbd5e1 !important;
  background: transparent !important;
  padding: 12px 16px !important;
  border-bottom: 1px solid rgba(148, 163, 184, 0.1);
}

.selectize-dropdown-content .option:hover {
  background: rgba(59, 130, 246, 0.2) !important;
  color: white !important;
}

.selectize-dropdown-content .option.active {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%) !important;
  color: white !important;
}

/* En-tête des pages */
.page-header {
  background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
  border-radius: 16px;
  padding: 36px;
  margin-bottom: 30px;
  box-shadow: 0 4px 24px rgba(0,0,0,0.4);
}

.page-header h1 {
  color: #f1f5f9;
  font-weight: 700;
  margin: 0;
}

.page-header p {
  color: #94a3b8;
  font-size: 1.1em;
  margin: 10px 0 0 0;
}

/* Formulaires */
.control-label {
  color: #cbd5e1;
  font-weight: 500;
}

.form-control {
  background: rgba(15, 23, 42, 0.8) !important;
  border-radius: 10px;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  color: #f1f5f9 !important;
}

.form-control option {
  background: #1e293b;
  color: #f1f5f9;
}

.form-control:focus {
  border-color: #3b82f6 !important;
  box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.3) !important;
  background: rgba(15, 23, 42, 0.9) !important;
}

/* Input de date */
.input-daterange {
  background: transparent !important;
}

.input-daterange .form-control {
  background: rgba(15, 23, 42, 0.8) !important;
  color: #f1f5f9 !important;
  text-align: center;
}

.input-daterange .input-group-addon {
  background: rgba(59, 130, 246, 0.2) !important;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  color: #94a3b8 !important;
}

/* Calendrier datepicker */
.datepicker {
  background: #1e293b !important;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  border-radius: 10px;
}

.datepicker table tr td,
.datepicker table tr th {
  color: #cbd5e1 !important;
}

.datepicker table tr td.active,
.datepicker table tr td.active:hover {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%) !important;
  color: white !important;
}

.datepicker table tr td:hover {
  background: rgba(59, 130, 246, 0.2) !important;
}

/* Tableaux DT */
.dataTables_wrapper {
  color: #cbd5e1;
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate {
  color: #cbd5e1;
}

table.dataTable thead th {
  background: rgba(59, 130, 246, 0.2);
  color: #f1f5f9;
  border-bottom: 2px solid #3b82f6;
}

table.dataTable tbody tr {
  background: rgba(30, 41, 59, 0.4);
}

table.dataTable tbody tr:hover {
  background: rgba(59, 130, 246, 0.15) !important;
}

table.dataTable tbody td {
  color: #cbd5e1;
}

.dataTables_wrapper .dataTables_paginate .paginate_button {
  color: #cbd5e1 !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: rgba(59, 130, 246, 0.2) !important;
  color: white !important;
}

.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%) !important;
  color: white !important;
}

/* Textes */
p, ul li {
  color: #cbd5e1 !important;
}

h3 {
  color: #f1f5f9 !important;
}

strong {
  color: #f1f5f9 !important;
}

.category-name {
  color: #f1f5f9 !important;
}

.category-count {
  color: #94a3b8 !important;
}

/* Valeurs des statistiques globales */
.stat-value {
  font-size: 1.5em;
  font-weight: bold;
  color: #f1f5f9;
}

/* Thème clair pour les stats */
.theme-clair .stat-value {
  color: #1e293b;
}

/* Justifier le texte dans la page À propos */
.chart-container p {
  text-align: justify;
}

.dataTables_wrapper label {
  color: #cbd5e1 !important;
}

/* Filtres du tableau DT */
.dataTables_filter input,
table.dataTable thead input,
table.dataTable thead select {
  background: rgba(15, 23, 42, 0.8) !important;
  border: 1px solid rgba(96, 165, 250, 0.3) !important;
  border-radius: 6px !important;
  color: #f1f5f9 !important;
  padding: 5px 10px !important;
}

table.dataTable thead select option {
  background: #1e293b !important;
  color: #f1f5f9 !important;
}

/* Filtres en thème clair */
.theme-clair .dataTables_filter input,
.theme-clair table.dataTable thead input,
.theme-clair table.dataTable thead select {
  background: white !important;
  border: 1px solid #cbd5e1 !important;
  color: #1e293b !important;
}

.theme-clair table.dataTable thead select option {
  background: white !important;
  color: #1e293b !important;
}

/* ==========================================================================
   BOUTON SWITCH THEME (en haut à droite)
   ========================================================================== */

.bouton-theme {
  position: fixed;
  top: 20px;
  right: 20px;
  z-index: 9999;  /* Pour être au-dessus de tout */
}

.bouton-theme button {
  background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%);
  border: none;
  border-radius: 50%;
  width: 50px;
  height: 50px;
  color: white;
  font-size: 20px;
  cursor: pointer;
  box-shadow: 0 4px 15px rgba(59, 130, 246, 0.4);
  transition: all 0.3s ease;
}

.bouton-theme button:hover {
  transform: scale(1.1);
  box-shadow: 0 6px 20px rgba(59, 130, 246, 0.6);
}

/* ==========================================================================
   THEME CLAIR
   On utilise la classe .theme-clair sur le body pour activer ce thème
   ========================================================================== */

/* Fond de page */
body.theme-clair {
  background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%) !important;
}

/* Barre de navigation */
.theme-clair .navbar-default {
  background: rgba(255, 255, 255, 0.95) !important;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
}

.theme-clair .navbar-default .navbar-brand {
  color: #1e293b !important;
}

.theme-clair .navbar-default .navbar-nav > li > a {
  color: #475569 !important;
}

.theme-clair .navbar-default .navbar-nav > li > a:hover {
  background: rgba(59, 130, 246, 0.1);
  color: #1e293b !important;
}

/* Cartes et containers */
.theme-clair .kpi-card,
.theme-clair .chart-container,
.theme-clair .filter-panel,
.theme-clair .page-header {
  background: white !important;
  border: 1px solid #e2e8f0;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.08);
}

/* Textes */
.theme-clair .kpi-value {
  color: #1e293b !important;
}

.theme-clair .kpi-label {
  color: #64748b !important;
}

.theme-clair .chart-title,
.theme-clair .page-header h1,
.theme-clair h3,
.theme-clair h4 {
  color: #1e293b !important;
}

.theme-clair .page-header p,
.theme-clair p,
.theme-clair ul li {
  color: #475569 !important;
}

.theme-clair .control-label {
  color: #334155 !important;
}

.theme-clair strong {
  color: #1e293b !important;
}

.theme-clair .category-name {
  color: #1e293b !important;
}

.theme-clair .category-count {
  color: #64748b !important;
}

/* Formulaires en thème clair */
.theme-clair .form-control,
.theme-clair .selectize-input {
  background: white !important;
  border: 1px solid #cbd5e1 !important;
  color: #1e293b !important;
}

.theme-clair .selectize-input input {
  color: #1e293b !important;
}

.theme-clair .selectize-dropdown {
  background: white !important;
  border: 1px solid #cbd5e1 !important;
}

.theme-clair .selectize-dropdown-content .option {
  color: #1e293b !important;
  background: white !important;
}

.theme-clair .selectize-dropdown-content .option:hover {
  background: #f1f5f9 !important;
}

/* Tableaux en thème clair */
.theme-clair table.dataTable thead th {
  background: #f1f5f9 !important;
  color: #1e293b !important;
  border-bottom: 2px solid #3b82f6 !important;
}

.theme-clair table.dataTable tbody tr {
  background: white !important;
}

.theme-clair table.dataTable tbody tr:hover {
  background: #f8fafc !important;
}

.theme-clair table.dataTable tbody td {
  color: #1e293b !important;
}

.theme-clair .dataTables_wrapper .dataTables_length,
.theme-clair .dataTables_wrapper .dataTables_filter,
.theme-clair .dataTables_wrapper .dataTables_info,
.theme-clair .dataTables_wrapper .dataTables_paginate,
.theme-clair .dataTables_wrapper label {
  color: #334155 !important;
}

/* Input de date en thème clair */
.theme-clair .input-daterange .form-control {
  background: white !important;
  color: #1e293b !important;
}

.theme-clair .input-daterange .input-group-addon {
  background: #f1f5f9 !important;
  color: #64748b !important;
}
"


# ---- interface utilisateur ----


# on utilise navbarPage pour faire plusieurs onglets
navbarPage(
  
  # titre de l'appli
  title = "📊 Dashboard Ulule",
  
  # thème de base
  theme = shinytheme("flatly"),
  
  # on ajoute notre css et les icônes
  tags$head(
    tags$style(HTML(mon_css)),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    
    # ==========================================================================
    # javascript pour le switch de thème
    # ce code s'exécute dans le navigateur
    # ==========================================================================
    tags$script(HTML("
      // On attend que Shiny soit complètement connecté avant d'exécuter le code
      $(document).on('shiny:connected', function() {
        
        // 1) On récupère le thème sauvegardé dans le navigateur (si existe)
        //    localStorage permet de sauvegarder des données même après fermeture
        var themeSauvegarde = localStorage.getItem('theme');
        
        // 2) Si l'utilisateur avait choisi le thème clair, on l'applique
        if (themeSauvegarde === 'clair') {
          $('body').addClass('theme-clair');
          $('#icone-theme').removeClass('fa-sun').addClass('fa-moon');
          
          // On informe Shiny du thème actuel (pour les graphiques)
          Shiny.setInputValue('theme_actuel', 'clair');
        } else {
          Shiny.setInputValue('theme_actuel', 'sombre');
        }
        
      });
      
      // Le gestionnaire de clic peut être dans document.ready
      // car il ne s'exécute que quand l'utilisateur clique
      $(document).ready(function() {
        
        // Quand on clique sur le bouton, on bascule le thème
        $('#btn-theme').click(function() {
          
          // toggleClass ajoute la classe si absente, la retire si présente
          $('body').toggleClass('theme-clair');
          
          // On change l'icône selon le thème actuel
          var icone = $('#icone-theme');
          
          if ($('body').hasClass('theme-clair')) {
            // On est en thème clair -> afficher icône lune (pour revenir au sombre)
            icone.removeClass('fa-sun').addClass('fa-moon');
            localStorage.setItem('theme', 'clair');
            
            // On informe Shiny pour mettre à jour les graphiques
            Shiny.setInputValue('theme_actuel', 'clair');
          } else {
            // On est en thème sombre -> afficher icône soleil (pour passer au clair)
            icone.removeClass('fa-moon').addClass('fa-sun');
            localStorage.setItem('theme', 'sombre');
            
            // On informe Shiny pour mettre à jour les graphiques
            Shiny.setInputValue('theme_actuel', 'sombre');
          }
        });
        
      });
    "))
  ),
  
  # ==========================================================================
  # ---- bouton switch thème ----
  # ==========================================================================
  tags$div(
    class = "bouton-theme",
    tags$button(
      id = "btn-theme",
      title = "Changer de thème",
      # icône soleil par défaut (on démarre en thème sombre)
      tags$i(id = "icone-theme", class = "fa fa-sun")
    )
  ),
  
  
  # ---- onglet 1 : dashboard ----
  
  
  tabPanel(
    title = "Dashboard",
    icon = icon("chart-line"),
    
    # --- En-tête de la page ---
    fluidRow(
      column(
        width = 12,
        div(
          class = "page-header",
          h1("Tableau de bord Ulule"),
          p("Analyse des campagnes de financement participatif depuis 2020")
        )
      )
    ),
    
    # --- Zone des filtres ---
    fluidRow(
      column(
        width = 12,
        div(
          class = "filter-panel",
          h4("Filtres et Options"),
          
          fluidRow(
            # filtre 1 : indicateur
            column(
              width = 4,
              selectInput(
                inputId = "indicateur",
                label = "Indicateur",
                choices = c(
                  "Nombre de campagnes" = "count",
                  "Campagnes réussies" = "success",
                  "Montant financé (EUR)" = "amount",
                  "Ratio financé (%)" = "ratio"
                ),
                selected = "count"
              )
            ),
            
            # filtre 2 : catégories
            column(
              width = 4,
              selectizeInput(
                inputId = "categories",
                label = "Catégories",
                choices = NULL,  # sera rempli par le server
                selected = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Choisir des catégories...",
                  plugins = list('remove_button')
                )
              ),
              # boutons pour tout sélectionner/désélectionner
              div(
                style = "margin-top: -10px;",
                actionButton(
                  inputId = "btn_tout_selectionner",
                  label = "Tout sélectionner",
                  icon = icon("check-double"),
                  class = "btn-sm",
                  style = "margin-right: 5px; background: #10b981; border: none; color: white;"
                ),
                actionButton(
                  inputId = "btn_tout_deselectionner",
                  label = "Tout désélectionner",
                  icon = icon("times"),
                  class = "btn-sm",
                  style = "background: #ef4444; border: none; color: white;"
                )
              )
            ),
            
            # filtre 3 : période
            column(
              width = 4,
              dateRangeInput(
                inputId = "date_range",
                label = "Période",
                start = "2020-01-01",
                end = Sys.Date(),
                language = "fr",
                separator = "à"
              )
            )
          ),
          
          # bouton reset
          fluidRow(
            column(
              width = 12,
              actionButton(
                inputId = "reset_filters",
                label = "Réinitialiser",
                icon = icon("refresh"),
                class = "btn-primary"
              )
            )
          )
        )
      )
    ),
    
    # --- Cartes KPI (4 indicateurs) ---
    fluidRow(
      # kpi 1 : total campagnes
      column(
        width = 3,
        div(
          class = "kpi-card",
          div(class = "kpi-icon", icon("rocket")),
          div(class = "kpi-label", "Total Campagnes"),
          div(class = "kpi-value", textOutput("kpi_total", inline = TRUE))
        )
      ),
      
      # kpi 2 : campagnes réussies
      column(
        width = 3,
        div(
          class = "kpi-card",
          div(class = "kpi-icon", icon("trophy")),
          div(class = "kpi-label", "Campagnes Réussies"),
          div(class = "kpi-value", textOutput("kpi_success", inline = TRUE))
        )
      ),
      
      # kpi 3 : montant total
      column(
        width = 3,
        div(
          class = "kpi-card",
          div(class = "kpi-icon", icon("euro-sign")),
          div(class = "kpi-label", "Montant Total"),
          div(class = "kpi-value", textOutput("kpi_amount", inline = TRUE))
        )
      ),
      
      # kpi 4 : taux de réussite
      column(
        width = 3,
        div(
          class = "kpi-card",
          div(class = "kpi-icon", icon("percent")),
          div(class = "kpi-label", "Taux de Réussite"),
          div(class = "kpi-value", textOutput("kpi_rate", inline = TRUE))
        )
      )
    ),
    
    # --- Graphiques principaux ---
    fluidRow(
      # graphique d'évolution
      column(
        width = 8,
        div(
          class = "chart-container",
          div(class = "chart-title", "Évolution Trimestrielle"),
          plotlyOutput("plot_evolution", height = "450px")
        )
      ),
      
      # top catégories
      column(
        width = 4,
        div(
          class = "chart-container",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(class = "chart-title", style = "margin-bottom: 0;", textOutput("titre_categories", inline = TRUE)),
            actionButton(
              inputId = "btn_inverser_categories",
              label = NULL,
              icon = icon("exchange-alt"),
              class = "btn-sm",
              style = "background: #6366f1; border: none; color: white;",
              title = "Inverser Top/Flop"
            )
          ),
          plotlyOutput("plot_categories", height = "450px")
        )
      )
    ),
    
    # --- Graphiques secondaires ---
    fluidRow(
      # distribution des montants
      column(
        width = 6,
        div(
          class = "chart-container",
          div(class = "chart-title", "Distribution des Montants"),
          plotlyOutput("plot_distribution", height = "400px")
        )
      ),
      
      # taux de réussite par catégorie
      column(
        width = 6,
        div(
          class = "chart-container",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(class = "chart-title", style = "margin-bottom: 0;", textOutput("titre_taux", inline = TRUE)),
            actionButton(
              inputId = "btn_inverser_taux",
              label = NULL,
              icon = icon("exchange-alt"),
              class = "btn-sm",
              style = "background: #6366f1; border: none; color: white;",
              title = "Inverser Réussite/Échec"
            )
          ),
          plotlyOutput("plot_success_rate", height = "400px")
        )
      )
    ),
    
    # --- Graphique supplémentaire ---
    fluidRow(
      # objectif vs montant levé
      column(
        width = 12,
        div(
          class = "chart-container",
          div(class = "chart-title", "Objectif vs Montant Levé"),
          plotlyOutput("plot_objectif_montant", height = "400px")
        )
      )
    )
  ),
  
  
  # ---- onglet 2 : explorer les données ----
  
  
  tabPanel(
    title = "Explorer",
    icon = icon("table"),
    
    # en-tête
    fluidRow(
      column(
        width = 12,
        div(
          class = "page-header",
          h1("Explorer les Campagnes"),
          p("Rechercher et télécharger les données")
        )
      )
    ),
    
    # tableau des données
    fluidRow(
      column(
        width = 12,
        div(
          class = "chart-container",
          
          fluidRow(
            column(
              width = 6,
              div(class = "chart-title", "Tableau des Campagnes")
            ),
            column(
              width = 6,
              align = "right",
              downloadButton(
                outputId = "download_data",
                label = "Télécharger CSV",
                icon = icon("download"),
                class = "btn-primary"
              )
            )
          ),
          
          hr(),
          
          # filtre par catégories avec boutons
          fluidRow(
            column(
              width = 12,
              div(
                style = "display: flex; align-items: flex-end; gap: 15px;",
                # le selectinput prend toute la place disponible
                div(
                  style = "flex: 1;",
                  selectizeInput(
                    inputId = "categories_tableau",
                    label = "Filtrer par catégories",
                    choices = NULL,  # sera rempli côté serveur
                    selected = NULL,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      placeholder = "Toutes les catégories",
                      plugins = list("remove_button")
                    )
                  )
                ),
                # les boutons à droite
                div(
                  style = "margin-bottom: 15px; white-space: nowrap;",
                  actionButton(
                    inputId = "btn_select_all_cat",
                    label = "Tout",
                    icon = icon("check-square"),
                    class = "btn-sm btn-outline-primary"
                  ),
                  actionButton(
                    inputId = "btn_deselect_all_cat",
                    label = "Aucun",
                    icon = icon("square"),
                    class = "btn-sm btn-outline-secondary"
                  )
                )
              )
            )
          ),
          
          # le tableau dt
          DTOutput("table_campaigns")
        )
      )
    )
  ),
  
  
  # ---- onglet 3 : à propos ----
  
  
  tabPanel(
    title = "À propos",
    icon = icon("info-circle"),
    
    # en-tête
    fluidRow(
      column(
        width = 12,
        div(
          class = "page-header",
          h1("À propos"),
          p("Informations sur le projet")
        )
      )
    ),
    
    fluidRow(
      # colonne principale avec les explications
      column(
        width = 8,
        div(
          class = "chart-container",
          
          h3("Objectif du projet"),
          p("Cette application Shiny permet d'analyser les campagnes de financement participatif Ulule depuis 2020."),
          
          h3("Les indicateurs"),
          tags$ul(
            tags$li("Nombre de campagnes par trimestre"),
            tags$li("Campagnes réussies (objectif atteint)"),
            tags$li("Montant total financé en euros"),
            tags$li("Taux de réussite en pourcentage")
          ),
          
          h3("Fonctionnalités"),
          tags$ul(
            tags$li("Filtres par catégories et dates"),
            tags$li("Graphiques interactifs Plotly"),
            tags$li("Tableau explorable"),
            tags$li("Export des données CSV"),
            tags$li("Conversion automatique en EUR")
          ),
          
          h3("Données"),
          p("Source: Plateforme Ulule (campagnes 2020-2025)"),
          p("Les campagnes annulées sont exclues de l'analyse."),
          
          h3("Développement"),
          p("Nous sommes 2 étudiants en master DSMS à l'UBS et nous avons développé cette application 
         dans le cadre de notre projet de visualisation. On s'est aidé de Claude 
         pour nous débloquer sur certains points techniques."),
          p("Côté technique, l'appli tourne avec R Shiny qui permet de faire des applis 
         web en R. Pour manipuler les données on a utilisé tidyverse (dplyr, ggplot2...), 
         les graphiques interactifs sont faits avec plotly et le tableau avec DT. 
         Pour les devises, on utilise priceR qui va chercher les taux de change 
         automatiquement quand l'application démarre."),
          
          h3("Auteurs"),
          p("Projet réalisé par DESJARDINS Bryan et FESTOC Julianne"),
          p("Master 1 DSMS  Université Bretagne Sud  Vannes")
        )
      ),
      
      # colonne latérale avec les stats
      column(
        width = 4,
        
        # stats globales
        div(
          class = "chart-container",
          div(class = "chart-title", "Statistiques Globales"),
          hr(),
          uiOutput("stats_globales")
        ),
        
        # top 5 catégories
        div(
          class = "chart-container",
          style = "margin-top: 20px;",
          div(class = "chart-title", "Top 5 Catégories"),
          hr(),
          uiOutput("top_categories")
        )
      )
    )
  ),
  
  
  # ---- onglet 4 : découverte libre ----
  
  
  tabPanel(
    title = "Découverte libre",
    icon = icon("flask"),
    
    # en-tête
    fluidRow(
      column(
        width = 12,
        div(
          class = "page-header",
          h1("Découverte libre"),
          p("Créez vos propres visualisations en choisissant les variables et types de graphiques")
        )
      )
    ),
    
    # filtre de période
    fluidRow(
      column(
        width = 12,
        div(
          class = "chart-container",
          fluidRow(
            column(
              width = 4,
              dateRangeInput(
                inputId = "periode_libre",
                label = "Période à analyser",
                start = "2020-01-01",
                end = Sys.Date(),
                min = "2010-01-01",
                max = Sys.Date(),
                language = "fr",
                separator = " → "
              )
            ),
            column(
              width = 8,
              div(
                style = "padding-top: 25px; color: #94a3b8;",
                icon("info-circle"),
                " Les 4 graphiques ci-dessous utilisent les données filtrées par cette période."
              )
            )
          )
        )
      )
    ),
    
    # ligne 1 : graphiques 1 et 2
    fluidRow(
      # graphique 1
      column(
        width = 6,
        div(
          class = "chart-container",
          div(class = "chart-title", "Graphique 1"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "var_x_graph1",
                label = "Variable X",
                choices = c(
                  "-- Choisir --" = "",
                  "Trimestre" = "annee_trimestre",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised",
                  "Montant levé (EUR)" = "montant_eur",
                  "Objectif (EUR)" = "objectif_eur",
                  "% de l'objectif" = "percent",
                  "Durée (jours)" = "nb_days",
                  "Commentaires" = "comment_counts",
                  "Actualités" = "news_count"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "var_y_graph1",
                label = "Variable Y (optionnel)",
                choices = c(
                  "-- Aucune --" = "",
                  "Nb campagnes" = "count",
                  "Nb réussies" = "nb_reussies",
                  "Montant total (EUR)" = "montant_eur",
                  "Objectif total (EUR)" = "objectif_eur",
                  "Taux de réussite (%)" = "taux_reussite",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "type_graph1",
                label = "Type de graphique",
                choices = c("-- Choisir une variable --" = ""),
                selected = ""
              )
            )
          ),
          plotlyOutput("plot_libre1", height = "350px")
        )
      ),
      
      # graphique 2
      column(
        width = 6,
        div(
          class = "chart-container",
          div(class = "chart-title", "Graphique 2"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "var_x_graph2",
                label = "Variable X",
                choices = c(
                  "-- Choisir --" = "",
                  "Trimestre" = "annee_trimestre",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised",
                  "Montant levé (EUR)" = "montant_eur",
                  "Objectif (EUR)" = "objectif_eur",
                  "% de l'objectif" = "percent",
                  "Durée (jours)" = "nb_days",
                  "Commentaires" = "comment_counts",
                  "Actualités" = "news_count"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "var_y_graph2",
                label = "Variable Y (optionnel)",
                choices = c(
                  "-- Aucune --" = "",
                  "Nb campagnes" = "count",
                  "Nb réussies" = "nb_reussies",
                  "Montant total (EUR)" = "montant_eur",
                  "Objectif total (EUR)" = "objectif_eur",
                  "Taux de réussite (%)" = "taux_reussite",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "type_graph2",
                label = "Type de graphique",
                choices = c("-- Choisir une variable --" = ""),
                selected = ""
              )
            )
          ),
          plotlyOutput("plot_libre2", height = "350px")
        )
      )
    ),
    
    # ligne 2 : graphiques 3 et 4
    fluidRow(
      # graphique 3
      column(
        width = 6,
        div(
          class = "chart-container",
          div(class = "chart-title", "Graphique 3"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "var_x_graph3",
                label = "Variable X",
                choices = c(
                  "-- Choisir --" = "",
                  "Trimestre" = "annee_trimestre",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised",
                  "Montant levé (EUR)" = "montant_eur",
                  "Objectif (EUR)" = "objectif_eur",
                  "% de l'objectif" = "percent",
                  "Durée (jours)" = "nb_days",
                  "Commentaires" = "comment_counts",
                  "Actualités" = "news_count"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "var_y_graph3",
                label = "Variable Y (optionnel)",
                choices = c(
                  "-- Aucune --" = "",
                  "Nb campagnes" = "count",
                  "Nb réussies" = "nb_reussies",
                  "Montant total (EUR)" = "montant_eur",
                  "Objectif total (EUR)" = "objectif_eur",
                  "Taux de réussite (%)" = "taux_reussite",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "type_graph3",
                label = "Type de graphique",
                choices = c("-- Choisir une variable --" = ""),
                selected = ""
              )
            )
          ),
          plotlyOutput("plot_libre3", height = "350px")
        )
      ),
      
      # graphique 4
      column(
        width = 6,
        div(
          class = "chart-container",
          div(class = "chart-title", "Graphique 4"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "var_x_graph4",
                label = "Variable X",
                choices = c(
                  "-- Choisir --" = "",
                  "Trimestre" = "annee_trimestre",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised",
                  "Montant levé (EUR)" = "montant_eur",
                  "Objectif (EUR)" = "objectif_eur",
                  "% de l'objectif" = "percent",
                  "Durée (jours)" = "nb_days",
                  "Commentaires" = "comment_counts",
                  "Actualités" = "news_count"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "var_y_graph4",
                label = "Variable Y (optionnel)",
                choices = c(
                  "-- Aucune --" = "",
                  "Nb campagnes" = "count",
                  "Nb réussies" = "nb_reussies",
                  "Montant total (EUR)" = "montant_eur",
                  "Objectif total (EUR)" = "objectif_eur",
                  "Taux de réussite (%)" = "taux_reussite",
                  "Catégorie" = "category",
                  "Pays" = "country",
                  "Langue" = "lang",
                  "Réussite" = "goal_raised"
                ),
                selected = ""
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = "type_graph4",
                label = "Type de graphique",
                choices = c("-- Choisir une variable --" = ""),
                selected = ""
              )
            )
          ),
          plotlyOutput("plot_libre4", height = "350px")
        )
      )
    )
  )
)