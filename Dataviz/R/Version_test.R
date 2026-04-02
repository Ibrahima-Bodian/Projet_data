# TETRIS ANALYTICS — Application R Shiny (Version Améliorée)
# BUT Science des Données · Promo 2026
# Équipe 5 — Interface graphique

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)

# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
base_url <- "http://127.0.0.1:8000"

get_parties <- function() {
  res <- GET(paste0(base_url, "/parties/"))
  fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

get_tours <- function(partie_id) {
  res <- GET(paste0(base_url, "/parties/", partie_id, "/tours/"))
  fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

count_action_type <- function(actions, action_type) {
  if (length(actions) == 0) return(0)
  sum(vapply(actions, function(action) {
    identical(tolower(action$type %||% ""), tolower(action_type))
  }, logical(1)))
}

`%||%` <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

normalize_tours <- function(partie_id, tours) {
  if (length(tours) == 0) return(NULL)
  
  rows <- lapply(tours, function(tour) {
    stats <- tour$Statistiques %||% list()
    actions <- tour$Actions %||% list()
    timestamps <- vapply(actions, function(action) action$timestamp %||% NA_real_, numeric(1))
    timestamps <- timestamps[!is.na(timestamps)]
    shapes <- vapply(actions, function(action) action$shape %||% NA_real_, numeric(1))
    shapes <- shapes[!is.na(shapes)]
    
    data.frame(
      game_id = partie_id,
      turn_id = tour$Tour %||% NA_integer_,
      time_start = if (length(timestamps) > 0) min(timestamps) else NA_real_,
      shape_id = if (length(shapes) > 0) shapes[[1]] else NA_real_,
      level = tour$Level %||% NA_real_,
      score = tour$Score %||% NA_real_,
      full_lines = tour$Full_lines %||% NA_real_,
      statistics_sum = stats$Sum %||% NA_real_,
      score_ratio = stats$Score_ration %||% NA_real_,
      efficiency = stats$Efficiency %||% NA_real_,
      num_events = length(actions),
      num_move_left = count_action_type(actions, "left"),
      num_move_right = count_action_type(actions, "right"),
      num_down = count_action_type(actions, "down"),
      num_rotate = count_action_type(actions, "rotate"),
      num_drop = count_action_type(actions, "drop"),
      num_none = count_action_type(actions, "none"),
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(rows)
}

parties <- get_parties()
partie_ids <- vapply(parties, function(partie) partie$Partie %||% NA_integer_, numeric(1))
partie_ids <- partie_ids[!is.na(partie_ids)]

tours_list <- lapply(partie_ids, function(pid) {
  tours <- get_tours(pid)
  normalize_tours(pid, tours)
})

df <- bind_rows(tours_list)

by_game <- df %>%
  group_by(game_id) %>%
  summarise(
    score_final  = max(score, na.rm = TRUE),
    nb_tours     = n(),
    lignes_total = max(full_lines, na.rm = TRUE),
    niveau_max   = max(level, na.rm = TRUE),
    duree        = max(time_start, na.rm = TRUE) - min(time_start, na.rm = TRUE),
    drop_rate    = mean(num_drop, na.rm = TRUE),
    inaction_moy = mean(num_none, na.rm = TRUE),
    .groups = "drop"
  )

long_games <- by_game %>% filter(nb_tours > 20)

# Couleurs améliorées
SHAPES <- c("I", "J", "L", "O", "S", "T", "Z")
COL_I <- "#00E5FF"
COL_J <- "#1E88E5"
COL_L <- "#FB8C00"
COL_O <- "#FDD835"
COL_S <- "#43A047"
COL_T <- "#8E24AA"
COL_Z <- "#E53935"

PIECE_COLORS <- c(COL_I, COL_J, COL_L, COL_O, COL_S, COL_T, COL_Z)
names(PIECE_COLORS) <- SHAPES

# Palettes de couleurs améliorées
GRADIENT_SCORE <- c("#E53935", "#FB8C00", "#FDD835", "#43A047")
GRADIENT_DROP <- c("#E53935", "#FB8C00", "#43A047")

# Thème ggplot personnalisé amélioré
theme_tetris <- function(legend_pos = "right") {
  theme_minimal(base_family = "Nunito") +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "#F8FAFD", color = NA),
      panel.grid.major = element_line(color = "#E8EDF5", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "#546E7A", size = 11, face = "plain"),
      axis.title = element_text(color = "#2C3E50", size = 12, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text = element_text(color = "#546E7A", size = 10),
      legend.title = element_text(color = "#2C3E50", size = 11, face = "bold"),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.position = legend_pos,
      plot.title = element_text(color = "#2C3E50", size = 14, face = "bold", margin = margin(b = 12)),
      plot.subtitle = element_text(color = "#78909C", size = 11, margin = margin(b = 16)),
      plot.caption = element_text(color = "#90A4AE", size = 9, margin = margin(t = 12)),
      plot.margin = margin(16, 20, 16, 20)
    )
}

# Configuration des onglets
TABS <- list(
  list(id = "vue", label = "📊 Vue générale",
       plots = list(
         list(id = "p_scores",  label = "Scores par partie",
              sub = "Distribution des scores finaux - Vert = excellent, Rouge = faible"),
         list(id = "p_scatter", label = "Durée vs Score",
              sub = "Relation entre le temps de jeu et la performance"),
         list(id = "p_lignes",  label = "Efficacité",
              sub = "Lignes complétées par partie - Couleur selon le niveau atteint")
       )
  ),
  list(id = "prog", label = "📈 Progression",
       plots = list(
         list(id = "p_evol",  label = "Évolution du score",
              sub = "Trajectoire du score - Points jaunes = lignes complétées"),
         list(id = "p_ratio", label = "Score ratio",
              sub = "Qualité du jeu par tour - Tendance à la hausse = progression")
       )
  ),
  list(id = "comp", label = "🎮 Comportement",
       plots = list(
         list(id = "p_actions",  label = "Actions par tour",
              sub = "Distribution des actions - Découvrez votre style de jeu"),
         list(id = "p_drop",     label = "Taux de drop",
              sub = "Rouge = réflexion lente, Vert = prise de décision rapide"),
         list(id = "p_inaction", label = "Inaction vs Score",
              sub = "Relation entre l'hésitation et la performance")
       )
  ),
  list(id = "piece", label = "🧩 Pièces",
       plots = list(
         list(id = "p_freq", label = "Fréquence",
              sub = "Distribution des 7 types de pièces Tetris"),
         list(id = "p_perf", label = "Performance",
              sub = "Score moyen selon le type de pièce reçu")
       )
  )
)

all_subs <- list()
for (tab in TABS)
  for (pl in tab$plots)
    all_subs[[pl$id]] <- pl$sub

# UI
ui <- fluidPage(
  title = "Tetris Analytics Dashboard",
  
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;500;600;700;800;900&family=Space+Mono:wght@400;700&display=swap"),
    tags$style(HTML('
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: "Nunito", sans-serif; background: #F0F4F8; overflow: hidden; }
      
      /* Landing Page */
      #landing {
        position: fixed; top: 0; left: 0; right: 0; bottom: 0;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        z-index: 10000;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .landing-card {
        background: white;
        border-radius: 32px;
        padding: 48px;
        max-width: 600px;
        text-align: center;
        box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        animation: fadeInUp 0.8s ease;
      }
      
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(30px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .landing-title {
        font-size: 48px;
        font-weight: 900;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 16px;
      }
      
      .landing-subtitle {
        font-size: 18px;
        color: #666;
        margin-bottom: 32px;
      }
      
      .landing-stats {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 20px;
        margin: 32px 0;
      }
      
      .landing-stat {
        background: #f5f5f5;
        padding: 20px;
        border-radius: 16px;
      }
      
      .landing-stat-value {
        font-size: 32px;
        font-weight: 800;
        color: #667eea;
      }
      
      .landing-stat-label {
        font-size: 14px;
        color: #666;
        margin-top: 8px;
      }
      
      .landing-btn {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        padding: 14px 32px;
        border-radius: 40px;
        font-size: 16px;
        font-weight: 700;
        cursor: pointer;
        transition: transform 0.2s, box-shadow 0.2s;
      }
      
      .landing-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 10px 30px rgba(102,126,234,0.4);
      }
      
      /* Dashboard */
      .dashboard {
        height: 100vh;
        display: flex;
        flex-direction: column;
      }
      
      .header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 0 32px;
        height: 70px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-shrink: 0;
      }
      
      .header-title {
        font-size: 20px;
        font-weight: 800;
        color: white;
        letter-spacing: 2px;
      }
      
      .header-stats {
        display: flex;
        gap: 32px;
      }
      
      .header-stat {
        text-align: right;
      }
      
      .header-stat-value {
        font-size: 20px;
        font-weight: 800;
        color: white;
      }
      
      .header-stat-label {
        font-size: 11px;
        color: rgba(255,255,255,0.7);
        letter-spacing: 1px;
      }
      
      .tab-bar {
        background: white;
        border-bottom: 2px solid #e0e0e0;
        padding: 0 32px;
        display: flex;
        gap: 8px;
        flex-shrink: 0;
      }
      
      .tab-btn {
        padding: 0 24px;
        height: 50px;
        background: transparent;
        border: none;
        font-size: 14px;
        font-weight: 700;
        color: #666;
        cursor: pointer;
        transition: all 0.2s;
        position: relative;
      }
      
      .tab-btn:hover {
        color: #667eea;
      }
      
      .tab-btn.active {
        color: #667eea;
      }
      
      .tab-btn.active::after {
        content: "";
        position: absolute;
        bottom: -2px;
        left: 0;
        right: 0;
        height: 3px;
        background: #667eea;
      }
      
      .plot-bar {
        background: white;
        padding: 12px 32px;
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        border-bottom: 1px solid #e0e0e0;
        flex-shrink: 0;
      }
      
      .plot-btn {
        padding: 6px 20px;
        background: #f5f5f5;
        border: 1px solid #e0e0e0;
        border-radius: 20px;
        font-size: 12px;
        font-weight: 600;
        color: #666;
        cursor: pointer;
        transition: all 0.2s;
      }
      
      .plot-btn:hover {
        background: #667eea;
        color: white;
        border-color: #667eea;
      }
      
      .plot-btn.active {
        background: #667eea;
        color: white;
        border-color: #667eea;
      }
      
      .main-content {
        flex: 1;
        padding: 24px 32px;
        display: flex;
        flex-direction: column;
        gap: 16px;
        overflow: auto;
        min-height: 0;
      }
      
      .chart-header {
        display: flex;
        justify-content: space-between;
        align-items: baseline;
        flex-shrink: 0;
      }
      
      .chart-title {
        font-size: 18px;
        font-weight: 800;
        color: #2C3E50;
      }
      
      .chart-subtitle {
        font-size: 13px;
        color: #78909C;
        margin-top: 4px;
      }
      
      .chart-card {
        background: white;
        border-radius: 20px;
        padding: 20px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        flex: 1;
        min-height: 0;
        display: flex;
        flex-direction: column;
      }
      
      .plot-container {
        flex: 1;
        min-height: 0;
      }
      
      select, .form-control {
        background: #f5f5f5 !important;
        border: 1px solid #e0e0e0 !important;
        border-radius: 8px !important;
        padding: 8px 16px !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        color: #333 !important;
      }
    '))
  ),
  
  div(id = "landing",
      div(class = "landing-card",
          div(class = "landing-title", "🎮 Tetris Analytics"),
          div(class = "landing-subtitle", "Analysez votre gameplay avec des visualisations avancées"),
          div(class = "landing-stats",
              div(class = "landing-stat",
                  div(class = "landing-stat-value", format(max(by_game$score_final, na.rm = TRUE), big.mark = " ")),
                  div(class = "landing-stat-label", "Score Max")
              ),
              div(class = "landing-stat",
                  div(class = "landing-stat-value", max(by_game$lignes_total, na.rm = TRUE)),
                  div(class = "landing-stat-label", "Lignes Max")
              ),
              div(class = "landing-stat",
                  div(class = "landing-stat-value", nrow(by_game)),
                  div(class = "landing-stat-label", "Parties")
              ),
              div(class = "landing-stat",
                  div(class = "landing-stat-value", format(nrow(df), big.mark = " ")),
                  div(class = "landing-stat-label", "Tours")
              )
          ),
          tags$button(class = "landing-btn", onclick = "$('#landing').fadeOut()", "🚀 Explorer les données")
    )
    ),
    
    div(class = "dashboard", style = "display: none;",
        div(class = "header",
            div(class = "header-title", "🎮 TETRIS ANALYTICS"),
            div(class = "header-stats",
                div(class = "header-stat",
                    div(class = "header-stat-value", format(max(by_game$score_final, na.rm = TRUE), big.mark = " ")),
                    div(class = "header-stat-label", "SCORE MAX")
                ),
                div(class = "header-stat",
                    div(class = "header-stat-value", max(by_game$lignes_total, na.rm = TRUE)),
                    div(class = "header-stat-label", "LIGNES MAX")
                ),
                div(class = "header-stat",
                    div(class = "header-stat-value", nrow(by_game)),
                    div(class = "header-stat-label", "PARTIES")
                ),
                div(class = "header-stat",
                    div(class = "header-stat-value", format(nrow(df), big.mark = " ")),
                    div(class = "header-stat-label", "TOURS")
                )
            )
        ),
        
        div(class = "tab-bar",
            lapply(TABS, function(tab)
              tags$button(
                class = paste0("tab-btn", if (tab$id == "vue") " active"),
                id = paste0("tab_", tab$id),
                onclick = paste0("switchTab('", tab$id, "')"),
                tab$label
              )
            )
        ),
        
        div(class = "plot-bar",
            uiOutput("plot_buttons")
        ),
        
        div(class = "main-content",
            div(class = "chart-header",
                div(
                  div(class = "chart-title", textOutput("chart_title", inline = TRUE)),
                  div(class = "chart-subtitle", textOutput("chart_sub", inline = TRUE))
                ),
                conditionalPanel(
                  condition = "input.active_plot == 'p_evol' || input.active_plot == 'p_ratio'",
                  selectInput("sel_game", label = NULL,
                              choices = setNames(
                                long_games$game_id,
                                paste0("Partie ", long_games$game_id,
                                       " | Score: ", format(long_games$score_final, big.mark = " "),
                                       " | ", long_games$nb_tours, " tours")
                              ),
                              selected = long_games$game_id[which.max(long_games$score_final)],
                              width = "320px")
                )
            ),
            
            div(class = "chart-card",
                div(class = "plot-container",
                    plotlyOutput("main_plot", height = "100%", width = "100%")
                )
            )
        )
    ),
    
    tags$script(HTML('
    function switchTab(tabId) {
      document.querySelectorAll(".tab-btn").forEach(btn => btn.classList.remove("active"));
      document.getElementById("tab_" + tabId).classList.add("active");
      Shiny.setInputValue("active_tab", tabId, { priority: "event" });
    }
    
    function selectPlot(plotId) {
      document.querySelectorAll(".plot-btn").forEach(btn => btn.classList.remove("active"));
      document.getElementById("plot_" + plotId).classList.add("active");
      Shiny.setInputValue("active_plot", plotId, { priority: "event" });
    }
    
    Shiny.addCustomMessageHandler("setActivePlot", function(plotId) {
      selectPlot(plotId);
    });
    
    $(document).ready(function() {
      $("#landing .landing-btn").click(function() {
        $("#landing").fadeOut(400, function() {
          $(".dashboard").fadeIn(400);
        });
      });
    });
  '))
)

# Server
server <- function(input, output, session) {
  
  cur_tab <- reactive({
    if (is.null(input$active_tab) || input$active_tab == "") "vue"
    else input$active_tab
  })
  
  tab_plots <- reactive({
    tab <- Filter(function(t) t$id == cur_tab(), TABS)
    if (length(tab) == 0) return(list())
    tab[[1]]$plots
  })
  
  output$plot_buttons <- renderUI({
    plots <- tab_plots()
    if (length(plots) == 0) return(NULL)
    lapply(seq_along(plots), function(i) {
      pl <- plots[[i]]
      tags$button(
        class = paste0("plot-btn", if (i == 1) " active"),
        id = paste0("plot_", pl$id),
        onclick = paste0("selectPlot('", pl$id, "')"),
        pl$label
      )
    })
  })
  
  observeEvent(input$active_tab, ignoreNULL = FALSE, {
    plots <- tab_plots()
    if (length(plots) > 0)
      session$sendCustomMessage("setActivePlot", plots[[1]]$id)
  })
  
  active_plot <- reactive({
    if (is.null(input$active_plot) || input$active_plot == "") "p_scores"
    else input$active_plot
  })
  
  output$chart_title <- renderText({
    for (tab in TABS)
      for (pl in tab$plots)
        if (pl$id == active_plot()) return(pl$label)
    ""
  })
  
  output$chart_sub <- renderText({
    sub <- all_subs[[active_plot()]]
    if (is.null(sub)) "" else sub
  })
  
  sel_data <- reactive({
    req(input$sel_game)
    df %>% filter(game_id == as.integer(input$sel_game))
  })
  
  output$main_plot <- renderPlotly({
    id <- active_plot()
    
    # Graphique 1: Scores par partie (amélioré)
    if (id == "p_scores") {
      p <- by_game %>%
        arrange(desc(score_final)) %>%
        mutate(game_id = factor(game_id, levels = game_id),
               color_group = cut(score_final, breaks = c(-Inf, 200, 800, 2000, Inf), 
                                labels = c("Faible", "Moyen", "Bon", "Excellent"))) %>%
        ggplot(aes(x = game_id, y = score_final, fill = color_group)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = ifelse(score_final > 50, format(score_final, big.mark = " "), "")),
                  vjust = -0.5, size = 3.5, fontface = "bold") +
        scale_fill_manual(values = c("Faible" = "#E53935", "Moyen" = "#FB8C00", 
                                    "Bon" = "#FDD835", "Excellent" = "#43A047")) +
        labs(x = "Partie", y = "Score final", fill = "Performance") +
        theme_tetris() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(color = "#333"))) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 2: Durée vs Score (amélioré avec régression)
    } else if (id == "p_scatter") {
      p <- by_game %>%
        filter(duree > 0, !is.na(score_final)) %>%
        ggplot(aes(x = duree, y = score_final)) +
        geom_point(aes(size = nb_tours, color = niveau_max), alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "#8E24AA", 
                   fill = "#8E24AA", alpha = 0.2, size = 1.2) +
        scale_color_gradientn(colors = c("#1E88E5", "#43A047", "#FDD835", "#FB8C00", "#E53935"),
                             name = "Niveau max") +
        scale_size_continuous(name = "Nb tours", range = c(3, 12)) +
        labs(x = "Durée (secondes)", y = "Score final") +
        theme_tetris()
      
      ggplotly(p) %>%
        layout(hoverlabel = list(bgcolor = "white")) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 3: Lignes complétées
    } else if (id == "p_lignes") {
      p <- by_game %>%
        mutate(game_id = factor(game_id, levels = game_id[order(-lignes_total)])) %>%
        ggplot(aes(x = game_id, y = lignes_total, fill = factor(niveau_max))) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_manual(values = c("1" = "#1E88E5", "2" = "#00E5FF", "3" = "#43A047",
                                    "4" = "#FDD835", "5" = "#FB8C00", "6" = "#E53935"),
                         name = "Niveau max") +
        labs(x = "Partie", y = "Lignes complétées") +
        theme_tetris() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 4: Évolution du score
    } else if (id == "p_evol") {
      d <- sel_data()
      d <- d %>% mutate(line_break = cumsum(full_lines > lag(full_lines, default = 0)))
      
      p <- ggplot(d, aes(x = turn_id, y = score)) +
        geom_area(fill = "#667eea", alpha = 0.2) +
        geom_line(color = "#667eea", size = 1.5) +
        geom_point(data = d %>% filter(full_lines > lag(full_lines, default = 0)),
                  aes(x = turn_id, y = score), color = "#FDD835", size = 3, stroke = 1.5) +
        labs(x = "Tour", y = "Score cumulé", 
             caption = "🟡 Lignes complétées") +
        theme_tetris() +
        theme(plot.caption = element_text(hjust = 0))
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hoverlabel = list(bgcolor = "white")) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 5: Score ratio
    } else if (id == "p_ratio") {
      d <- sel_data() %>% filter(!is.na(score_ratio))
      
      p <- ggplot(d, aes(x = turn_id, y = score_ratio)) +
        geom_point(color = "#8E24AA", size = 3, alpha = 0.6) +
        geom_smooth(method = "loess", se = TRUE, color = "#E53935", 
                   fill = "#E53935", alpha = 0.15, size = 1.2) +
        labs(x = "Tour", y = "Score ratio") +
        theme_tetris()
      
      ggplotly(p) %>%
        layout(hoverlabel = list(bgcolor = "white")) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 6: Actions par tour
    } else if (id == "p_actions") {
      action_data <- data.frame(
        action = c("⬅️ Gauche", "➡️ Droite", "⬇️ Bas", "🔄 Rotation", "💥 Drop", "⏸️ Inaction"),
        val = c(mean(df$num_move_left), mean(df$num_move_right), mean(df$num_down),
               mean(df$num_rotate), mean(df$num_drop), mean(df$num_none)),
        color = c("#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#E53935", "#90A4AE")
      ) %>%
        arrange(desc(val))
      
      p <- ggplot(action_data, aes(x = reorder(action, val), y = val, fill = color)) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = round(val, 2)), hjust = -0.2, size = 4, fontface = "bold") +
        scale_fill_identity() +
        coord_flip() +
        labs(x = NULL, y = "Moyenne par tour") +
        theme_tetris()
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 7: Taux de drop
    } else if (id == "p_drop") {
      p <- by_game %>%
        filter(nb_tours > 5) %>%
        arrange(drop_rate) %>%
        mutate(game_id = factor(game_id, levels = game_id)) %>%
        ggplot(aes(x = game_id, y = drop_rate, fill = drop_rate)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_fill_gradient2(low = "#E53935", mid = "#FB8C00", high = "#43A047",
                            midpoint = 0.5, labels = scales::percent) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(x = "Partie", y = "Taux de drop", fill = "Taux") +
        theme_tetris() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 8: Inaction vs Score
    } else if (id == "p_inaction") {
      dat <- by_game %>% filter(nb_tours > 5, !is.na(inaction_moy))
      
      p <- ggplot(dat, aes(x = inaction_moy, y = score_final)) +
        geom_point(aes(size = nb_tours, color = factor(niveau_max)), alpha = 0.8) +
        geom_smooth(method = "lm", se = TRUE, color = "#E53935", 
                   fill = "#E53935", alpha = 0.15, size = 1.2) +
        scale_color_manual(values = c("1" = "#1E88E5", "2" = "#00E5FF", "3" = "#43A047",
                                     "4" = "#FDD835", "5" = "#FB8C00", "6" = "#E53935"),
                          name = "Niveau max") +
        scale_size_continuous(name = "Nb tours", range = c(3, 12)) +
        labs(x = "Inaction moyenne par tour", y = "Score final") +
        theme_tetris()
      
      ggplotly(p) %>%
        layout(hoverlabel = list(bgcolor = "white")) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 9: Fréquence des pièces
    } else if (id == "p_freq") {
      freq_data <- df %>%
        count(shape_id) %>%
        mutate(piece = SHAPES[shape_id + 1],
               col = PIECE_COLORS[piece])
      
      p <- ggplot(freq_data, aes(x = reorder(piece, -n), y = n, fill = col)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = n), vjust = -0.5, size = 4.5, fontface = "bold") +
        scale_fill_identity() +
        labs(x = "Type de pièce", y = "Nombre d'apparitions") +
        theme_tetris()
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
      
    # Graphique 10: Performance par pièce
    } else if (id == "p_perf") {
      perf_data <- df %>%
        filter(!is.na(score_ratio)) %>%
        group_by(shape_id) %>%
        summarise(sr = mean(score_ratio, na.rm = TRUE), .groups = "drop") %>%
        mutate(piece = SHAPES[shape_id + 1],
               col = PIECE_COLORS[piece])
      
      p <- ggplot(perf_data, aes(x = reorder(piece, sr), y = sr, fill = col)) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = round(sr, 1)), vjust = -0.5, size = 4.5, fontface = "bold") +
        scale_fill_identity() +
        labs(x = "Type de pièce", y = "Score ratio moyen") +
        theme_tetris()
      
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    }
  })
}

shinyApp(ui, server)
