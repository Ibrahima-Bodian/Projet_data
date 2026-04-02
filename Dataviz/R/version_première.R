library(shiny)
library(ggplot2)
library(dplyr)

# ── Données ──────────────────────────────────────────────────────────────────
df <- read.csv("../data/tetris_parsed_turns.csv", stringsAsFactors = FALSE)

by_game <- df %>%
  group_by(game_id) %>%
  summarise(
    score_final     = max(score,       na.rm = TRUE),
    nb_tours        = n(),
    lignes_total    = max(full_lines,  na.rm = TRUE),
    niveau_max      = max(level,       na.rm = TRUE),
    duree           = max(time_start,  na.rm = TRUE) - min(time_start, na.rm = TRUE),
    score_ratio_moy = mean(score_ratio, na.rm = TRUE),
    drop_rate       = mean(num_drop,   na.rm = TRUE),
    inaction_moy    = mean(num_none,   na.rm = TRUE),
    rotate_moy      = mean(num_rotate, na.rm = TRUE),
    .groups = "drop"
  )

SHAPES <- c("I","J","L","O","S","T","Z")

# Couleurs pièces Tetris authentiques
COL_I <- "#00BCD4"   # cyan
COL_J <- "#1565C0"   # bleu
COL_L <- "#EF6C00"   # orange
COL_O <- "#F9A825"   # jaune
COL_S <- "#2E7D32"   # vert
COL_T <- "#6A1B9A"   # violet
COL_Z <- "#C62828"   # rouge

PIECE_COLORS <- c(COL_I, COL_J, COL_L, COL_O, COL_S, COL_T, COL_Z)
names(PIECE_COLORS) <- SHAPES

long_games <- by_game %>% filter(nb_tours > 20)

# ── Thème ggplot ─────────────────────────────────────────────────────────────
th <- function(legend_pos = "none") {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.background  = element_rect(fill = "#FAFBFD",     color = NA),
      panel.grid.major  = element_line(color = "#E8ECF2", linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = "#5A6478", size = 10),
      axis.title        = element_text(color = "#5A6478", size = 11),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text       = element_text(color = "#5A6478",  size = 10),
      legend.title      = element_text(color = "#5A6478",  size = 10),
      legend.key        = element_rect(fill  = "transparent", color = NA),
      legend.position   = legend_pos,
      plot.margin       = margin(8, 12, 8, 8)
    )
}

score_color <- function(s) {
  dplyr::case_when(
    s >= 2000 ~ COL_I,
    s >= 800  ~ COL_S,
    s >= 200  ~ COL_O,
    TRUE      ~ COL_Z
  )
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "TETRIS Analytics",
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700;800&family=Space+Mono:wght@700&display=swap"),
    tags$style(HTML('
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: "Nunito", sans-serif;
        background: #F0F4FA;
        color: #1E2A3A;
        min-height: 100vh;
      }

      /* Hero header avec image de fond */
      .hero {
        background:
          background: linear-gradient(135deg, rgba(21,101,192,0.92) 0%, rgba(106,27,154,0.88) 100%);
        padding: 36px 40px 32px;
        color: white;
        position: relative;
        overflow: hidden;
      }
      .hero::after {
        content: "";
        position: absolute; top: 0; left: 0; right: 0; bottom: 0;
        background-image:
          linear-gradient(rgba(255,255,255,.04) 1px, transparent 1px),
          linear-gradient(90deg, rgba(255,255,255,.04) 1px, transparent 1px);
        background-size: 32px 32px;
        pointer-events: none;
      }
      .hero-inner {
        position: relative; z-index: 1;
        display: flex; align-items: center; justify-content: space-between;
      }
      .hero-left { display: flex; align-items: center; gap: 18px; }

      /* Grille de blocs Tetris animée */
      .tetris-grid {
        display: grid;
        grid-template-columns: repeat(4, 16px);
        grid-template-rows: repeat(4, 16px);
        gap: 3px;
      }
      .tetris-grid span {
        width: 16px; height: 16px; border-radius: 3px;
        opacity: 0.9;
      }
      @keyframes drop { 0%{transform:translateY(-20px);opacity:0} 100%{transform:none;opacity:.9} }

      .hero-title h1 {
        font-family: "Space Mono", monospace;
        font-size: 26px; font-weight: 700;
        letter-spacing: 4px; color: #fff;
        text-shadow: 0 2px 12px rgba(0,0,0,.3);
      }
      .hero-title p {
        font-size: 12px; color: rgba(255,255,255,.75);
        letter-spacing: 2.5px; margin-top: 4px; font-weight: 600;
      }
      .hero-stats { display: flex; gap: 28px; }
      .hstat { text-align: center; }
      .hstat-val {
        font-family: "Space Mono", monospace;
        font-size: 22px; font-weight: 700; color: #fff;
      }
      .hstat-lbl { font-size: 10px; color: rgba(255,255,255,.65); letter-spacing: 1.5px; margin-top: 2px; }

      /* Corps principal */
      .main { max-width: 1280px; margin: 0 auto; padding: 28px 28px 48px; }

      /* KPI cards */
      .kpi-row { display: grid; grid-template-columns: repeat(4,1fr); gap: 14px; margin-bottom: 26px; }
      .kpi {
        background: #fff;
        border-radius: 14px;
        padding: 18px 20px 16px;
        border-top: 4px solid transparent;
        box-shadow: 0 2px 12px rgba(30,42,58,.07);
        transition: transform .18s, box-shadow .18s;
      }
      .kpi:hover { transform: translateY(-2px); box-shadow: 0 6px 20px rgba(30,42,58,.12); }
      .kpi.t1 { border-top-color: #00BCD4; }
      .kpi.t2 { border-top-color: #F9A825; }
      .kpi.t3 { border-top-color: #C62828; }
      .kpi.t4 { border-top-color: #6A1B9A; }
      .kpi-lbl { font-size: 11px; font-weight: 700; color: #8A96A8; letter-spacing: 1.5px; text-transform: uppercase; margin-bottom: 8px; }
      .kpi-val { font-family: "Space Mono", monospace; font-size: 30px; font-weight: 700; color: #1E2A3A; line-height: 1; }
      .kpi-sub { font-size: 11px; color: #8A96A8; margin-top: 6px; }

      /* Navigation */
      .nav-row { display: flex; gap: 6px; margin-bottom: 24px; flex-wrap: wrap; }
      .nbtn {
        background: #fff;
        border: 1.5px solid #DDE3EE;
        color: #5A6478;
        font-family: "Nunito", sans-serif;
        font-size: 13px; font-weight: 700;
        padding: 9px 22px; border-radius: 10px;
        cursor: pointer; transition: all .18s;
        letter-spacing: .3px;
      }
      .nbtn:hover { border-color: #1565C0; color: #1565C0; background: #EBF2FF; }
      .nbtn.active { background: #1565C0; border-color: #1565C0; color: #fff; }

      /* Grilles de panels */
      .g2   { display: grid; grid-template-columns: 1fr 1fr;         gap: 18px; margin-bottom: 18px; }
      .g3   { display: grid; grid-template-columns: 1fr 1fr 1fr;     gap: 18px; margin-bottom: 18px; }
      .gfull { margin-bottom: 18px; }

      /* Card panel */
      .panel {
        background: #fff;
        border-radius: 14px;
        padding: 20px 22px 18px;
        box-shadow: 0 2px 12px rgba(30,42,58,.06);
      }
      .panel-t  { font-size: 14px; font-weight: 700; color: #1E2A3A; margin-bottom: 2px; }
      .panel-s  { font-size: 11px; color: #8A96A8; margin-bottom: 14px; }
      .panel-hl { display: flex; align-items: baseline; gap: 8px; margin-bottom: 14px; }
      .dot {
        width: 10px; height: 10px; border-radius: 2px;
        display: inline-block; margin-right: 4px;
      }

      /* select */
      select, .form-control {
        background: #F4F7FC !important;
        border: 1.5px solid #DDE3EE !important;
        color: #1E2A3A !important;
        border-radius: 9px !important;
        font-family: "Nunito", sans-serif !important;
        font-size: 13px !important;
        font-weight: 600 !important;
        padding: 8px 14px !important;
        outline: none !important;
      }

      /* Footer */
      .footer {
        text-align: center; font-size: 11px;
        color: #8A96A8; letter-spacing: 1px;
        border-top: 1px solid #DDE3EE;
        padding-top: 16px; margin-top: 8px;
      }

      /* Animation entrée tab */
      .tab-c { animation: fi .22s ease; }
      @keyframes fi { from{opacity:0;transform:translateY(5px)} to{opacity:1;transform:none} }
    '))
  ),
  
  # ── Hero ──
  div(class = "hero",
      div(class = "hero-inner",
          div(class = "hero-left",
              # Grille Tetris colorée
              div(class = "tetris-grid",
                  lapply(list(
                    COL_I, COL_J, COL_L, COL_O,
                    COL_O, COL_I, COL_T, COL_J,
                    COL_T, COL_S, COL_I, COL_Z,
                    COL_Z, COL_T, COL_O, COL_S
                  ), function(col)
                    tags$span(style = paste0("background:", col, "; animation: drop .6s ease both;"))
                  )
              ),
              div(class = "hero-title",
                  tags$h1("TETRIS ANALYTICS"),
                  tags$p("TABLEAU DE BORD \u2014 \u00c9QUIPE 5")
              )
          ),
          div(class = "hero-stats",
              div(class = "hstat",
                  div(class = "hstat-val", nrow(by_game)),
                  div(class = "hstat-lbl", "PARTIES")
              ),
              div(class = "hstat",
                  div(class = "hstat-val", format(nrow(df), big.mark=" ")),
                  div(class = "hstat-lbl", "TOURS")
              ),
              div(class = "hstat",
                  div(class = "hstat-val", format(max(by_game$score_final), big.mark=" ")),
                  div(class = "hstat-lbl", "MEILLEUR SCORE")
              )
          )
      )
  ),
  
  div(class = "main",
      
      # ── KPI ──
      div(class = "kpi-row",
          div(class = "kpi t1",
              div(class = "kpi-lbl", "Meilleur score"),
              div(class = "kpi-val", format(max(by_game$score_final), big.mark = " ")),
              div(class = "kpi-sub", paste0("Partie #", by_game$game_id[which.max(by_game$score_final)]))
          ),
          div(class = "kpi t2",
              div(class = "kpi-lbl", "Lignes max"),
              div(class = "kpi-val", max(by_game$lignes_total)),
              div(class = "kpi-sub", paste0("Niveau max atteint : ", max(by_game$niveau_max)))
          ),
          div(class = "kpi t3",
              div(class = "kpi-lbl", "Score moyen"),
              div(class = "kpi-val", format(round(mean(by_game$score_final)), big.mark = " ")),
              div(class = "kpi-sub", paste0(sum(by_game$score_final > 0), " parties jouées"))
          ),
          div(class = "kpi t4",
              div(class = "kpi-lbl", "Tours / partie"),
              div(class = "kpi-val", round(mean(by_game$nb_tours))),
              div(class = "kpi-sub", paste0("Total : ", format(nrow(df), big.mark=" "), " tours"))
          )
      ),
      
      # ── Navigation ──
      div(class = "nav-row",
          tags$button(class = "nbtn active", id = "b1",
                      onclick = "switchTab('t1')", "\u2665 Vue générale"),
          tags$button(class = "nbtn", id = "b2",
                      onclick = "switchTab('t2')", "\u25b2 Progression"),
          tags$button(class = "nbtn", id = "b3",
                      onclick = "switchTab('t3')", "\u25c6 Comportement"),
          tags$button(class = "nbtn", id = "b4",
                      onclick = "switchTab('t4')", "\u25a0 Pièces")
      ),
      
      tags$script(HTML("
      function switchTab(id) {
        document.querySelectorAll('.tab-c').forEach(function(e){ e.style.display='none'; });
        document.getElementById(id).style.display = 'block';
        document.querySelectorAll('.nbtn').forEach(function(e){ e.classList.remove('active'); });
        event.currentTarget.classList.add('active');
      }
    ")),
      
      # ══ TAB 1 : Vue générale ════════════════════════════════════════════════
      div(id = "t1", class = "tab-c",
          div(class = "g2",
              div(class = "panel",
                  div(class = "panel-t", "Score final par partie"),
                  div(class = "panel-s", "Couleur selon le niveau de performance"),
                  plotOutput("p1", height = "260px")
              ),
              div(class = "panel",
                  div(class = "panel-t", "Durée de partie vs Score final"),
                  div(class = "panel-s", "Nuage de points — droite de régression en pointillés"),
                  plotOutput("p2", height = "260px")
              )
          ),
          div(class = "gfull",
              div(class = "panel",
                  div(class = "panel-t", "Lignes complétées par partie"),
                  div(class = "panel-s", "Indicateur d'efficacité de placement — couleur par niveau max"),
                  plotOutput("p3", height = "200px")
              )
          )
      ),
      
      # ══ TAB 2 : Progression ═════════════════════════════════════════════════
      div(id = "t2", class = "tab-c", style = "display:none",
          div(class = "gfull",
              div(class = "panel",
                  div(class = "panel-t", "Évolution du score au fil des tours"),
                  div(class = "panel-s", "Sélectionner une partie ci-dessous"),
                  selectInput("sel_game", label = NULL,
                              choices = setNames(
                                long_games$game_id,
                                paste0("Partie ", long_games$game_id,
                                       "  —  score : ", format(long_games$score_final, big.mark = " "),
                                       "  —  ", long_games$nb_tours, " tours")
                              ),
                              selected = long_games$game_id[which.max(long_games$score_final)],
                              width = "380px"
                  ),
                  plotOutput("p4", height = "280px")
              )
          ),
          div(class = "gfull",
              div(class = "panel",
                  div(class = "panel-t", "Score ratio au fil des tours"),
                  div(class = "panel-s", "Qualité de jeu tour par tour — droite de tendance rouge"),
                  plotOutput("p5", height = "250px")
              )
          )
      ),
      
      # ══ TAB 3 : Comportement ════════════════════════════════════════════════
      div(id = "t3", class = "tab-c", style = "display:none",
          div(class = "g2",
              div(class = "panel",
                  div(class = "panel-t", "Répartition des actions par tour"),
                  div(class = "panel-s", "Moyenne sur l'ensemble des parties"),
                  plotOutput("p6", height = "270px")
              ),
              div(class = "panel",
                  div(class = "panel-t", "Taux de drop par partie"),
                  div(class = "panel-s", "Rouge = peu décisif, Vert = très décisif"),
                  plotOutput("p7", height = "270px")
              )
          ),
          div(class = "gfull",
              div(class = "panel",
                  div(class = "panel-t", "Inaction moyenne vs score final"),
                  div(class = "panel-s", "Chaque point = une partie — taille proportionnelle au nombre de tours"),
                  plotOutput("p8", height = "260px")
              )
          )
      ),
      
      # ══ TAB 4 : Pièces ═══════════════════════════════════════════════════════
      div(id = "t4", class = "tab-c", style = "display:none",
          div(class = "g2",
              div(class = "panel",
                  div(class = "panel-t", "Fréquence des pièces reçues"),
                  div(class = "panel-s", "Distribution sur toutes les parties"),
                  plotOutput("p9", height = "280px")
              ),
              div(class = "panel",
                  div(class = "panel-t", "Score ratio moyen par type de pièce"),
                  div(class = "panel-s", "Performance moyenne selon la pièce reçue"),
                  plotOutput("p10", height = "280px")
              )
          )
      ),
      
      div(class = "footer",
          "TETRIS ANALYTICS  \u2022  \u00c9QUIPE 5  \u2022  Interface R Shiny")
  )
)

# ── SERVER ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # 1. Score par partie
  output$p1 <- renderPlot(bg = "transparent", {
    by_game %>%
      mutate(col = score_color(score_final)) %>%
      ggplot(aes(x = factor(game_id), y = score_final, fill = col)) +
      geom_col(width = 0.72) +
      geom_text(aes(label = ifelse(score_final > 50,
                                   format(score_final, big.mark = " "), "")),
                vjust = -0.4, color = "#1E2A3A", size = 2.8, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, .12))) +
      labs(x = "Numéro de partie", y = "Score final") +
      th()
  })
  
  # 2. Scatter durée vs score
  output$p2 <- renderPlot(bg = "transparent", {
    by_game %>% filter(duree > 0) %>%
      ggplot(aes(x = duree, y = score_final)) +
      geom_point(color = "#1565C0", size = 4, alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE,
                  color = "#C62828", fill = "#C62828",
                  alpha = 0.08, linewidth = 1.2, linetype = "dashed") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Durée (secondes)", y = "Score final") +
      th()
  })
  
  # 3. Lignes complétées
  output$p3 <- renderPlot(bg = "transparent", {
    by_game %>%
      mutate(piece_col = PIECE_COLORS[pmin(niveau_max, 7)]) %>%
      ggplot(aes(x = factor(game_id), y = lignes_total,
                 fill = factor(niveau_max))) +
      geom_col(width = 0.72) +
      scale_fill_manual(
        values = c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,
                   "5"=COL_T,"6"=COL_Z,"7"=COL_L),
        name = "Niveau max") +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      labs(x = "Partie", y = "Lignes complétées") +
      th(legend_pos = "right")
  })
  
  # 4. Progression score (réactive)
  sel_data <- reactive({
    req(input$sel_game)
    df %>% filter(game_id == as.integer(input$sel_game))
  })
  
  output$p4 <- renderPlot(bg = "transparent", {
    d <- sel_data()
    ggplot(d, aes(x = turn_id, y = score)) +
      geom_area(fill = "#1565C0", alpha = 0.12) +
      geom_line(color = "#1565C0", linewidth = 1.4) +
      geom_point(data = d %>% filter(full_lines > lag(full_lines, default = 0)),
                 aes(x = turn_id, y = score),
                 color = COL_O, size = 3, shape = 21,
                 fill = COL_O, stroke = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Numéro du tour", y = "Score cumulé",
           caption = "Points jaunes = tours avec lignes complétées") +
      th()
  })
  
  # 5. Score ratio + tendance
  output$p5 <- renderPlot(bg = "transparent", {
    d <- sel_data() %>% filter(!is.na(score_ratio))
    ggplot(d, aes(x = turn_id, y = score_ratio)) +
      geom_point(color = "#6A1B9A", size = 2, alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE,
                  color = "#C62828", fill = "#C62828",
                  alpha = 0.08, linewidth = 1.4) +
      labs(x = "Numéro du tour", y = "Score ratio") +
      th()
  })
  
  # 6. Actions
  output$p6 <- renderPlot(bg = "transparent", {
    data.frame(
      action = c("Inaction", "Mvt gauche", "Mvt droite",
                 "Rotation", "Drop", "Descente"),
      val = c(
        mean(df$num_none,       na.rm = TRUE),
        mean(df$num_move_left,  na.rm = TRUE),
        mean(df$num_move_right, na.rm = TRUE),
        mean(df$num_rotate,     na.rm = TRUE),
        mean(df$num_drop,       na.rm = TRUE),
        mean(df$num_down,       na.rm = TRUE)
      ),
      col = c(COL_J, COL_I, COL_S, COL_T, COL_Z, COL_L)
    ) %>%
      ggplot(aes(x = reorder(action, val), y = val, fill = col)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = round(val, 1)),
                hjust = -0.2, color = "#1E2A3A", size = 3.4, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
      coord_flip() +
      labs(x = NULL, y = "Moyenne par tour") +
      th()
  })
  
  # 7. Drop rate
  output$p7 <- renderPlot(bg = "transparent", {
    by_game %>% filter(nb_tours > 5) %>%
      ggplot(aes(x = factor(game_id), y = drop_rate, fill = drop_rate)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      scale_fill_gradient(low = "#C62828", high = "#2E7D32") +
      scale_y_continuous(labels = scales::percent,
                         limits = c(0, 1.1),
                         expand = expansion(mult = c(0, .05))) +
      labs(x = "Partie", y = "Taux de drop") +
      th()
  })
  
  # 8. Inaction vs score
  output$p8 <- renderPlot(bg = "transparent", {
    by_game %>% filter(nb_tours > 5) %>%
      ggplot(aes(x = inaction_moy, y = score_final,
                 color = factor(niveau_max), size = nb_tours)) +
      geom_point(alpha = 0.8) +
      geom_smooth(aes(group = 1), method = "lm", se = FALSE,
                  color = "#C62828", linewidth = 1.2,
                  linetype = "dashed", inherit.aes = FALSE,
                  data = by_game %>% filter(nb_tours > 5),
                  mapping = aes(x = inaction_moy, y = score_final)) +
      scale_color_manual(
        values = c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,
                   "5"=COL_T,"6"=COL_Z),
        name = "Niveau") +
      scale_size_continuous(name = "Nb tours", range = c(3, 9)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Inaction moyenne par tour", y = "Score final") +
      th(legend_pos = "right")
  })
  
  # 9. Fréquence pièces
  output$p9 <- renderPlot(bg = "transparent", {
    df %>% count(shape_id) %>%
      mutate(piece = SHAPES[shape_id + 1],
             col   = PIECE_COLORS[piece]) %>%
      ggplot(aes(x = reorder(piece, -n), y = n, fill = col)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.4,
                color = "#1E2A3A", size = 3.4, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .12))) +
      labs(x = "Type de pièce", y = "Nombre d'apparitions") +
      th()
  })
  
  # 10. Score ratio par pièce
  output$p10 <- renderPlot(bg = "transparent", {
    df %>% filter(!is.na(score_ratio)) %>%
      group_by(shape_id) %>%
      summarise(sr = mean(score_ratio, na.rm = TRUE), .groups = "drop") %>%
      mutate(piece = SHAPES[shape_id + 1],
             col   = PIECE_COLORS[piece]) %>%
      ggplot(aes(x = reorder(piece, sr), y = sr, fill = col)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = round(sr, 1)), vjust = -0.4,
                color = "#1E2A3A", size = 3.4, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, .14)),
                         limits = c(0, 14)) +
      labs(x = "Type de pièce", y = "Score ratio moyen") +
      th()
  })
}

shinyApp(ui, server)