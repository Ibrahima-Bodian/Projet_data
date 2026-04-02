library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(httr)
library(jsonlite)

# ════════════════════════════════════════════════════════════════════════════
#  CONFIGURATION API
#  ➜ Quand l'équipe API RESTful est prête, changer uniquement cette section.
# ════════════════════════════════════════════════════════════════════════════

# -- Paramètres de connexion --------------------------------------------------
API_BASE_URL <- "https://jsonplaceholder.typicode.com"   # ← remplacer par l'URL de l'équipe 4
API_KEY      <- NULL                                      # ← mettre la clé API si nécessaire
API_TIMEOUT  <- 10                                        # secondes

# -- Endpoints ----------------------------------------------------------------
# Adapter les chemins quand l'API RESTful de l'équipe sera disponible
ENDPOINT_TURNS  <- "/posts"    # ← sera : "/turns"   ou "/games/{id}/turns"
ENDPOINT_GAMES  <- "/users"    # ← sera : "/games"
ENDPOINT_PIECES <- "/todos"    # ← sera : "/pieces"  ou "/stats/pieces"

# ════════════════════════════════════════════════════════════════════════════
#  COUCHE D'ACCÈS AUX DONNÉES
#  Toute la logique de fetch + parsing est ici.
#  Si l'API change de format, seule cette section est à modifier.
# ════════════════════════════════════════════════════════════════════════════

# Helper générique : appel GET avec gestion d'erreur
api_get <- function(endpoint, query_params = list()) {
  url <- paste0(API_BASE_URL, endpoint)

  headers <- add_headers(
    "Content-Type" = "application/json",
    "Accept"       = "application/json"
  )
  if (!is.null(API_KEY)) {
    headers <- add_headers(
      "Content-Type"  = "application/json",
      "Accept"        = "application/json",
      "Authorization" = paste("Bearer", API_KEY)
    )
  }

  tryCatch({
    resp <- GET(url, headers, query = query_params, timeout(API_TIMEOUT))

    if (http_error(resp)) {
      warning(paste("Erreur API:", status_code(resp), "-", url))
      return(NULL)
    }

    content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)

  }, error = function(e) {
    warning(paste("Connexion API impossible:", e$message))
    return(NULL)
  })
}

# ── fetch_turns() ─────────────────────────────────────────────────────────────
# Récupère les tours de jeu depuis l'API.
# JSONPlaceholder /posts retourne : userId, id, title, body
# → on simule les colonnes Tetris pour que le reste de l'app fonctionne.
#
# QUAND L'API EQUIPE 4 EST PRÊTE :
#   Supprimer le bloc "# Simulation" et décommenter "# Données réelles"
fetch_turns <- function() {
  raw <- api_get(ENDPOINT_TURNS)
  if (is.null(raw)) return(NULL)

  # ── Simulation (JSONPlaceholder) ──────────────────────────────────────────
  # On transforme les champs de /posts en colonnes Tetris fictives
  # pour que tous les graphiques de l'app fonctionnent sans modification.
  set.seed(42)
  n <- nrow(raw)
  df <- data.frame(
    game_id       = raw$userId,
    turn_id       = raw$id,
    time_start    = cumsum(runif(n, 0.5, 3)),
    shape_id      = sample(0:6, n, replace = TRUE),
    level         = pmin(ceiling(raw$id / 10), 7),
    score         = cumsum(sample(c(0, 19, 37, 53, 100), n, replace = TRUE,
                                  prob = c(.3,.3,.2,.15,.05))),
    full_lines    = cumsum(sample(0:1, n, replace = TRUE, prob = c(.8,.2))),
    score_ratio   = runif(n, 0, 20),
    num_move_left  = sample(0:3, n, replace = TRUE),
    num_move_right = sample(0:3, n, replace = TRUE),
    num_down       = sample(0:5, n, replace = TRUE),
    num_rotate     = sample(0:2, n, replace = TRUE),
    num_drop       = sample(0:1, n, replace = TRUE),
    num_none       = sample(0:8, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  # ── /Simulation ───────────────────────────────────────────────────────────

  # ── Données réelles (décommenter quand API équipe 4 disponible) ───────────
  # df <- raw %>%
  #   rename(
  #     game_id       = game_id,
  #     turn_id       = turn_id,
  #     time_start    = time_start,
  #     shape_id      = shape_id,
  #     level         = level,
  #     score         = score,
  #     full_lines    = full_lines,
  #     score_ratio   = score_ratio,
  #     num_move_left  = num_move_left,
  #     num_move_right = num_move_right,
  #     num_down       = num_down,
  #     num_rotate     = num_rotate,
  #     num_drop       = num_drop,
  #     num_none       = num_none
  #   ) %>%
  #   mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  # ── /Données réelles ──────────────────────────────────────────────────────

  df
}

# ── fetch_api_status() ────────────────────────────────────────────────────────
# Vérifie si l'API répond (utilisé pour le bandeau de statut dans l'UI).
fetch_api_status <- function() {
  tryCatch({
    resp <- GET(paste0(API_BASE_URL, "/posts/1"), timeout(5))
    list(ok = !http_error(resp), code = status_code(resp))
  }, error = function(e) {
    list(ok = FALSE, code = "timeout")
  })
}

# ════════════════════════════════════════════════════════════════════════════
#  TRAITEMENT DES DONNÉES
# ════════════════════════════════════════════════════════════════════════════

safe_max <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

make_by_game <- function(df) {
  df %>%
    group_by(game_id) %>%
    summarise(
      score_final     = safe_max(score),
      nb_tours        = n(),
      lignes_total    = safe_max(full_lines),
      niveau_max      = safe_max(level),
      duree           = safe_max(time_start) - min(time_start, na.rm = TRUE),
      score_ratio_moy = mean(score_ratio,   na.rm = TRUE),
      drop_rate       = mean(num_drop,      na.rm = TRUE),
      inaction_moy    = mean(num_none,      na.rm = TRUE),
      rotate_moy      = mean(num_rotate,    na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(score_final))
}

# ════════════════════════════════════════════════════════════════════════════
#  CONSTANTES VISUELLES
# ════════════════════════════════════════════════════════════════════════════

SHAPES <- c("I","J","L","O","S","T","Z")
COL_I  <- "#00BCD4"; COL_J <- "#1565C0"; COL_L <- "#EF6C00"
COL_O  <- "#F9A825"; COL_S <- "#2E7D32"; COL_T <- "#6A1B9A"; COL_Z <- "#C62828"
PIECE_COLORS <- setNames(c(COL_I,COL_J,COL_L,COL_O,COL_S,COL_T,COL_Z), SHAPES)

score_color <- function(s) {
  dplyr::case_when(
    s >= 2000 ~ COL_I, s >= 800 ~ COL_S, s >= 200 ~ COL_O, TRUE ~ COL_Z
  )
}

th <- function(legend_pos = "none") {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "#FAFBFD",     color = NA),
      panel.grid.major = element_line(color = "#E8ECF2", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "#5A6478", size = 10),
      axis.title       = element_text(color = "#5A6478", size = 11),
      legend.background= element_rect(fill = "transparent", color = NA),
      legend.text      = element_text(color = "#5A6478", size = 10),
      legend.title     = element_text(color = "#5A6478", size = 10),
      legend.key       = element_rect(fill = "transparent", color = NA),
      legend.position  = legend_pos,
      plot.margin      = margin(8, 12, 8, 8)
    )
}

# ════════════════════════════════════════════════════════════════════════════
#  UI
# ════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  title = "TETRIS Analytics",
  tags$head(
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700;800&family=Space+Mono:wght@700&display=swap"),
    tags$style(HTML('
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
      body { font-family: "Nunito", sans-serif; background: #F0F4FA; color: #1E2A3A; min-height: 100vh; }

      /* Bandeau statut API */
      .api-bar { padding: 8px 40px; font-size: 12px; font-weight: 700;
                 letter-spacing: 1px; display: flex; align-items: center; gap: 10px; }
      .api-bar.ok  { background: #E8F5E9; color: #2E7D32; border-bottom: 1px solid #C8E6C9; }
      .api-bar.err { background: #FFEBEE; color: #C62828; border-bottom: 1px solid #FFCDD2; }
      .api-dot { width: 8px; height: 8px; border-radius: 50%; display: inline-block; }
      .api-bar.ok  .api-dot { background: #2E7D32; animation: pulse 2s infinite; }
      .api-bar.err .api-dot { background: #C62828; }
      @keyframes pulse { 0%,100%{opacity:1} 50%{opacity:.4} }

      .hero {
        background: linear-gradient(135deg, rgba(21,101,192,0.92) 0%, rgba(106,27,154,0.88) 100%);
        padding: 36px 40px 32px; color: white; position: relative; overflow: hidden;
      }
      .hero::after {
        content: ""; position: absolute; top:0; left:0; right:0; bottom:0;
        background-image:
          linear-gradient(rgba(255,255,255,.04) 1px, transparent 1px),
          linear-gradient(90deg, rgba(255,255,255,.04) 1px, transparent 1px);
        background-size: 32px 32px; pointer-events: none;
      }
      .hero-inner { position:relative; z-index:1; display:flex; align-items:center; justify-content:space-between; }
      .hero-left  { display:flex; align-items:center; gap:18px; }
      .tetris-grid { display:grid; grid-template-columns:repeat(4,16px); grid-template-rows:repeat(4,16px); gap:3px; }
      .tetris-grid span { width:16px; height:16px; border-radius:3px; opacity:.9; }
      @keyframes drop { 0%{transform:translateY(-20px);opacity:0} 100%{transform:none;opacity:.9} }
      .hero-title h1 { font-family:"Space Mono",monospace; font-size:26px; font-weight:700; letter-spacing:4px; color:#fff; text-shadow:0 2px 12px rgba(0,0,0,.3); }
      .hero-title p  { font-size:12px; color:rgba(255,255,255,.75); letter-spacing:2.5px; margin-top:4px; font-weight:600; }
      .hero-stats    { display:flex; gap:28px; }
      .hstat         { text-align:center; }
      .hstat-val     { font-family:"Space Mono",monospace; font-size:22px; font-weight:700; color:#fff; }
      .hstat-lbl     { font-size:10px; color:rgba(255,255,255,.65); letter-spacing:1.5px; margin-top:2px; }

      .main { max-width:1280px; margin:0 auto; padding:28px 28px 48px; }

      .kpi-row { display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:26px; }
      .kpi { background:#fff; border-radius:14px; padding:18px 20px 16px; border-top:4px solid transparent;
             box-shadow:0 2px 12px rgba(30,42,58,.07); transition:transform .18s,box-shadow .18s; }
      .kpi:hover { transform:translateY(-2px); box-shadow:0 6px 20px rgba(30,42,58,.12); }
      .kpi.t1{border-top-color:#00BCD4} .kpi.t2{border-top-color:#F9A825}
      .kpi.t3{border-top-color:#C62828} .kpi.t4{border-top-color:#6A1B9A}
      .kpi-lbl { font-size:11px; font-weight:700; color:#8A96A8; letter-spacing:1.5px; text-transform:uppercase; margin-bottom:8px; }
      .kpi-val { font-family:"Space Mono",monospace; font-size:30px; font-weight:700; color:#1E2A3A; line-height:1; }
      .kpi-sub { font-size:11px; color:#8A96A8; margin-top:6px; }

      .nav-row { display:flex; gap:6px; margin-bottom:24px; flex-wrap:wrap; }
      .nbtn { background:#fff; border:1.5px solid #DDE3EE; color:#5A6478; font-family:"Nunito",sans-serif;
              font-size:13px; font-weight:700; padding:9px 22px; border-radius:10px; cursor:pointer; transition:all .18s; }
      .nbtn:hover  { border-color:#1565C0; color:#1565C0; background:#EBF2FF; }
      .nbtn.active { background:#1565C0; border-color:#1565C0; color:#fff; }

      .refresh-btn { margin-left:auto; background:#1565C0; border:none; color:#fff; font-family:"Nunito",sans-serif;
                     font-size:13px; font-weight:700; padding:9px 20px; border-radius:10px; cursor:pointer; transition:all .18s; }
      .refresh-btn:hover { background:#0D47A1; }

      .g2    { display:grid; grid-template-columns:1fr 1fr;     gap:18px; margin-bottom:18px; }
      .gfull { margin-bottom:18px; }
      .panel   { background:#fff; border-radius:14px; padding:20px 22px 18px; box-shadow:0 2px 12px rgba(30,42,58,.06); }
      .panel-t { font-size:14px; font-weight:700; color:#1E2A3A; margin-bottom:2px; }
      .panel-s { font-size:11px; color:#8A96A8; margin-bottom:14px; }

      select, .form-control {
        background:#F4F7FC !important; border:1.5px solid #DDE3EE !important;
        color:#1E2A3A !important; border-radius:9px !important;
        font-family:"Nunito",sans-serif !important; font-size:13px !important;
        font-weight:600 !important; padding:8px 14px !important; outline:none !important;
      }

      .loading { text-align:center; padding:60px; color:#8A96A8; font-size:14px; }
      .footer { text-align:center; font-size:11px; color:#8A96A8; letter-spacing:1px;
                border-top:1px solid #DDE3EE; padding-top:16px; margin-top:8px; }
      .tab-c { animation:fi .22s ease; }
      @keyframes fi { from{opacity:0;transform:translateY(5px)} to{opacity:1;transform:none} }
    '))
  ),

  # ── Bandeau statut API ──
  uiOutput("api_status_bar"),

  # ── Hero ──
  div(class = "hero",
    div(class = "hero-inner",
      div(class = "hero-left",
        div(class = "tetris-grid",
          lapply(list(COL_I,COL_J,COL_L,COL_O, COL_O,COL_I,COL_T,COL_J,
                      COL_T,COL_S,COL_I,COL_Z, COL_Z,COL_T,COL_O,COL_S),
            function(col) tags$span(style = paste0("background:", col, "; animation:drop .6s ease both;")))
        ),
        div(class = "hero-title",
          tags$h1("TETRIS ANALYTICS"),
          tags$p("TABLEAU DE BORD \u2014 \u00c9QUIPE 5")
        )
      ),
      div(class = "hero-stats",
        div(class = "hstat", uiOutput("hero_nb_games"), div(class = "hstat-lbl", "PARTIES")),
        div(class = "hstat", uiOutput("hero_nb_turns"),  div(class = "hstat-lbl", "TOURS")),
        div(class = "hstat", uiOutput("hero_best"),      div(class = "hstat-lbl", "MEILLEUR SCORE"))
      )
    )
  ),

  div(class = "main",

    # ── KPI ──
    div(class = "kpi-row",
      div(class = "kpi t1", div(class="kpi-lbl","Meilleur score"),  uiOutput("kpi1")),
      div(class = "kpi t2", div(class="kpi-lbl","Lignes max"),      uiOutput("kpi2")),
      div(class = "kpi t3", div(class="kpi-lbl","Score moyen"),     uiOutput("kpi3")),
      div(class = "kpi t4", div(class="kpi-lbl","Tours / partie"),  uiOutput("kpi4"))
    ),

    # ── Navigation + bouton refresh ──
    div(class = "nav-row",
      tags$button(class="nbtn active", id="b1", onclick="switchTab('t1')", "\u2665 Vue générale"),
      tags$button(class="nbtn",        id="b2", onclick="switchTab('t2')", "\u25b2 Progression"),
      tags$button(class="nbtn",        id="b3", onclick="switchTab('t3')", "\u25c6 Comportement"),
      tags$button(class="nbtn",        id="b4", onclick="switchTab('t4')", "\u25a0 Pièces"),
      tags$button(class="refresh-btn", onclick="Shiny.setInputValue('refresh', Math.random())",
                  "\u21bb  Rafraîchir les données")
    ),
    tags$script(HTML("
      function switchTab(id) {
        document.querySelectorAll('.tab-c').forEach(function(e){ e.style.display='none'; });
        document.getElementById(id).style.display = 'block';
        document.querySelectorAll('.nbtn').forEach(function(e){ e.classList.remove('active'); });
        event.currentTarget.classList.add('active');
      }
    ")),

    # ══ TAB 1 ══════════════════════════════════════════════════════════════════
    div(id="t1", class="tab-c",
      div(class="g2",
        div(class="panel", div(class="panel-t","Score final par partie"),
            div(class="panel-s","Couleur selon le niveau de performance"),
            plotOutput("p1", height="260px")),
        div(class="panel", div(class="panel-t","Durée de partie vs Score final"),
            div(class="panel-s","Nuage de points — droite de régression en pointillés"),
            plotOutput("p2", height="260px"))
      ),
      div(class="gfull",
        div(class="panel", div(class="panel-t","Lignes complétées par partie"),
            div(class="panel-s","Couleur par niveau max atteint"),
            plotOutput("p3", height="200px")))
    ),

    # ══ TAB 2 ══════════════════════════════════════════════════════════════════
    div(id="t2", class="tab-c", style="display:none",
      div(class="gfull",
        div(class="panel", div(class="panel-t","Évolution du score au fil des tours"),
            div(class="panel-s","Sélectionner une partie ci-dessous"),
            uiOutput("sel_game_ui"),
            plotOutput("p4", height="280px"))
      ),
      div(class="gfull",
        div(class="panel", div(class="panel-t","Score ratio au fil des tours"),
            div(class="panel-s","Qualité de jeu tour par tour — tendance rouge"),
            plotOutput("p5", height="250px")))
    ),

    # ══ TAB 3 ══════════════════════════════════════════════════════════════════
    div(id="t3", class="tab-c", style="display:none",
      div(class="g2",
        div(class="panel", div(class="panel-t","Répartition des actions par tour"),
            div(class="panel-s","Moyenne sur l'ensemble des parties"),
            plotOutput("p6", height="270px")),
        div(class="panel", div(class="panel-t","Taux de drop par partie"),
            div(class="panel-s","Rouge = peu décisif · Vert = très décisif"),
            plotOutput("p7", height="270px"))
      ),
      div(class="gfull",
        div(class="panel", div(class="panel-t","Inaction moyenne vs score final"),
            div(class="panel-s","Chaque point = une partie — taille ∝ nombre de tours"),
            plotOutput("p8", height="260px")))
    ),

    # ══ TAB 4 ══════════════════════════════════════════════════════════════════
    div(id="t4", class="tab-c", style="display:none",
      div(class="g2",
        div(class="panel", div(class="panel-t","Fréquence des pièces reçues"),
            div(class="panel-s","Distribution sur toutes les parties"),
            plotOutput("p9", height="280px")),
        div(class="panel", div(class="panel-t","Score ratio moyen par type de pièce"),
            div(class="panel-s","Performance moyenne selon la pièce reçue"),
            plotOutput("p10", height="280px"))
      )
    ),

    div(class="footer", "TETRIS ANALYTICS  \u2022  \u00c9QUIPE 5  \u2022  Interface R Shiny")
  )
)

# ════════════════════════════════════════════════════════════════════════════
#  SERVER
# ════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Fetch réactif : se relance au démarrage et au clic "Rafraîchir" ────────
  data_raw <- reactive({
    input$refresh   # dépendance pour le bouton refresh
    withProgress(message = "Connexion à l'API...", value = 0.3, {
      df <- fetch_turns()
      setProgress(value = 1, message = "Données chargées !")
      df
    })
  })

  # ── Données traitées ────────────────────────────────────────────────────────
  df <- reactive({
    req(data_raw())
    data_raw()
  })

  by_game <- reactive({
    req(df())
    make_by_game(df())
  })

  long_games <- reactive({
    req(by_game())
    by_game() %>% filter(nb_tours > 5)
  })

  # ── Statut API ──────────────────────────────────────────────────────────────
  api_status <- reactive({
    input$refresh
    fetch_api_status()
  })

  output$api_status_bar <- renderUI({
    s <- api_status()
    if (s$ok) {
      div(class = "api-bar ok",
        span(class = "api-dot"),
        paste0("API connectée · ", API_BASE_URL, " · HTTP ", s$code,
               " · Mode : SIMULATION (remplacer par l'API équipe 4)")
      )
    } else {
      div(class = "api-bar err",
        span(class = "api-dot"),
        paste0("API injoignable · ", API_BASE_URL, " · Code : ", s$code)
      )
    }
  })

  # ── Hero stats ──────────────────────────────────────────────────────────────
  output$hero_nb_games <- renderUI(div(class="hstat-val", nrow(by_game())))
  output$hero_nb_turns  <- renderUI(div(class="hstat-val", format(nrow(df()), big.mark=" ")))
  output$hero_best      <- renderUI(div(class="hstat-val", format(max(by_game()$score_final, na.rm=TRUE), big.mark=" ")))

  # ── KPI ─────────────────────────────────────────────────────────────────────
  output$kpi1 <- renderUI({
    bg <- by_game()
    tagList(
      div(class="kpi-val", format(max(bg$score_final, na.rm=TRUE), big.mark=" ")),
      div(class="kpi-sub", paste0("Partie #", bg$game_id[which.max(bg$score_final)]))
    )
  })
  output$kpi2 <- renderUI({
    bg <- by_game()
    tagList(
      div(class="kpi-val", max(bg$lignes_total, na.rm=TRUE)),
      div(class="kpi-sub", paste0("Niveau max : ", max(bg$niveau_max, na.rm=TRUE)))
    )
  })
  output$kpi3 <- renderUI({
    bg <- by_game()
    tagList(
      div(class="kpi-val", format(round(mean(bg$score_final, na.rm=TRUE)), big.mark=" ")),
      div(class="kpi-sub", paste0(sum(!is.na(bg$score_final)), " parties jouées"))
    )
  })
  output$kpi4 <- renderUI({
    bg <- by_game()
    tagList(
      div(class="kpi-val", round(mean(bg$nb_tours, na.rm=TRUE))),
      div(class="kpi-sub", paste0("Total : ", format(nrow(df()), big.mark=" "), " tours"))
    )
  })

  # ── Selector partie (dynamique selon les données reçues) ────────────────────
  output$sel_game_ui <- renderUI({
    lg <- long_games()
    req(nrow(lg) > 0)
    selectInput("sel_game", label = NULL,
      choices  = setNames(
        lg$game_id,
        paste0("Partie ", lg$game_id,
               "  —  score : ", format(lg$score_final, big.mark=" "),
               "  —  ", lg$nb_tours, " tours")
      ),
      selected = lg$game_id[which.max(lg$score_final)],
      width    = "420px"
    )
  })

  sel_data <- reactive({
    req(input$sel_game)
    df() %>% filter(game_id == as.integer(input$sel_game))
  })

  # ── Graphiques ───────────────────────────────────────────────────────────────

  output$p1 <- renderPlot(bg = "transparent", {
    by_game() %>%
      mutate(col = score_color(score_final)) %>%
      ggplot(aes(x = factor(game_id), y = score_final, fill = col)) +
      geom_col(width = 0.72) +
      geom_text(aes(label = ifelse(score_final > 50, format(score_final, big.mark=" "), "")),
                vjust = -0.4, color = "#1E2A3A", size = 2.8, fontface = "bold") +
      scale_fill_identity() +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0,.12))) +
      labs(x = "Numéro de partie", y = "Score final") + th()
  })

  output$p2 <- renderPlot(bg = "transparent", {
    by_game() %>% filter(!is.na(duree), duree > 0) %>%
      ggplot(aes(x = duree, y = score_final)) +
      geom_point(color = "#1565C0", size = 4, alpha = 0.75) +
      geom_smooth(method="lm", se=TRUE, color="#C62828", fill="#C62828",
                  alpha=0.08, linewidth=1.2, linetype="dashed") +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Durée (secondes)", y = "Score final") + th()
  })

  output$p3 <- renderPlot(bg = "transparent", {
    by_game() %>%
      mutate(niv = factor(pmin(niveau_max, 7))) %>%
      ggplot(aes(x = factor(game_id), y = lignes_total, fill = niv)) +
      geom_col(width = 0.72) +
      scale_fill_manual(
        values = c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,"5"=COL_T,"6"=COL_Z,"7"=COL_L),
        name = "Niveau max", drop = FALSE
      ) +
      scale_y_continuous(expand = expansion(mult = c(0,.1))) +
      labs(x = "Partie", y = "Lignes complétées") + th(legend_pos = "right")
  })

  output$p4 <- renderPlot(bg = "transparent", {
    d <- sel_data()
    ggplot(d, aes(x = turn_id, y = score)) +
      geom_area(fill = "#1565C0", alpha = 0.12) +
      geom_line(color = "#1565C0", linewidth = 1.4) +
      geom_point(data = d %>% filter(full_lines > lag(full_lines, default=0)),
                 aes(x=turn_id, y=score), color=COL_O, fill=COL_O, size=3, shape=21, stroke=1.2) +
      scale_y_continuous(labels = scales::comma) +
      labs(x="Numéro du tour", y="Score cumulé", caption="Points jaunes = lignes complétées") +
      th()
  })

  output$p5 <- renderPlot(bg = "transparent", {
    d <- sel_data() %>% filter(!is.na(score_ratio))
    ggplot(d, aes(x=turn_id, y=score_ratio)) +
      geom_point(color="#6A1B9A", size=2, alpha=0.5) +
      geom_smooth(method="lm", se=TRUE, color="#C62828", fill="#C62828", alpha=0.08, linewidth=1.4) +
      labs(x="Numéro du tour", y="Score ratio") + th()
  })

  output$p6 <- renderPlot(bg = "transparent", {
    d <- df()
    data.frame(
      action = c("Inaction","Mvt gauche","Mvt droite","Rotation","Drop","Descente"),
      val    = c(mean(d$num_none,na.rm=T), mean(d$num_move_left,na.rm=T),
                 mean(d$num_move_right,na.rm=T), mean(d$num_rotate,na.rm=T),
                 mean(d$num_drop,na.rm=T), mean(d$num_down,na.rm=T)),
      col = c(COL_J,COL_I,COL_S,COL_T,COL_Z,COL_L)
    ) %>%
      ggplot(aes(x=reorder(action,val), y=val, fill=col)) +
      geom_col(width=0.65, show.legend=FALSE) +
      geom_text(aes(label=round(val,1)), hjust=-0.2, color="#1E2A3A", size=3.4, fontface="bold") +
      scale_fill_identity() +
      scale_y_continuous(expand=expansion(mult=c(0,0.22))) +
      coord_flip() + labs(x=NULL, y="Moyenne par tour") + th()
  })

  output$p7 <- renderPlot(bg = "transparent", {
    d <- by_game() %>% filter(nb_tours > 5, !is.na(drop_rate))
    ggplot(d, aes(x=factor(game_id), y=drop_rate, fill=drop_rate)) +
      geom_col(width=0.72, show.legend=FALSE) +
      scale_fill_gradient(low="#C62828", high="#2E7D32") +
      scale_y_continuous(labels=scales::percent,
                         limits=c(0, max(d$drop_rate, na.rm=TRUE)*1.15),
                         expand=expansion(mult=c(0,.05))) +
      labs(x="Partie", y="Taux de drop") + th()
  })

  output$p8 <- renderPlot(bg = "transparent", {
    d <- by_game() %>% filter(nb_tours > 5, !is.na(inaction_moy), !is.na(score_final))
    ggplot(d, aes(x=inaction_moy, y=score_final,
                  color=factor(pmin(niveau_max,6)), size=nb_tours)) +
      geom_point(alpha=0.8) +
      geom_smooth(data=d, aes(x=inaction_moy, y=score_final), inherit.aes=FALSE,
                  method="lm", se=FALSE, color="#C62828", linewidth=1.2, linetype="dashed") +
      scale_color_manual(values=c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,"5"=COL_T,"6"=COL_Z),
                         name="Niveau") +
      scale_size_continuous(name="Nb tours", range=c(3,9)) +
      scale_y_continuous(labels=scales::comma) +
      labs(x="Inaction moyenne par tour", y="Score final") + th(legend_pos="right")
  })

  output$p9 <- renderPlot(bg = "transparent", {
    df() %>%
      filter(!is.na(shape_id), shape_id >= 0, shape_id <= 6) %>%
      count(shape_id) %>%
      mutate(piece=SHAPES[shape_id+1], col=PIECE_COLORS[piece]) %>%
      ggplot(aes(x=reorder(piece,-n), y=n, fill=col)) +
      geom_col(width=0.7, show.legend=FALSE) +
      geom_text(aes(label=n), vjust=-0.4, color="#1E2A3A", size=3.4, fontface="bold") +
      scale_fill_identity() +
      scale_y_continuous(expand=expansion(mult=c(0,.12))) +
      labs(x="Type de pièce", y="Nombre d'apparitions") + th()
  })

  output$p10 <- renderPlot(bg = "transparent", {
    df() %>%
      filter(!is.na(score_ratio), !is.na(shape_id), shape_id >= 0, shape_id <= 6) %>%
      group_by(shape_id) %>%
      summarise(sr=mean(score_ratio, na.rm=TRUE), .groups="drop") %>%
      mutate(piece=SHAPES[shape_id+1], col=PIECE_COLORS[piece]) %>%
      ggplot(aes(x=reorder(piece,sr), y=sr, fill=col)) +
      geom_col(width=0.7, show.legend=FALSE) +
      geom_text(aes(label=round(sr,1)), vjust=-0.4, color="#1E2A3A", size=3.4, fontface="bold") +
      scale_fill_identity() +
      scale_y_continuous(expand=expansion(mult=c(0,.14))) +
      labs(x="Type de pièce", y="Score ratio moyen") + th()
  })
}

shinyApp(ui, server)
