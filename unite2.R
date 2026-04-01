# TETRIS ANALYTICS — Application R Shiny
# BUT Science des Données · Promo 2026
# Équipe 5 — Interface graphique
library(shiny)
library(ggplot2)
library(dplyr)

# 1. CHARGEMENT ET PRÉPARATION DES DONNÉES
# Lecture du fichier CSV des tours de jeu
df <- read.csv("C:/tetris-test/shiny/data/tetris_parsed_turns.csv", stringsAsFactors = FALSE)

# Résumé par partie : une ligne = une partie avec ses statistiques clés
by_game <- df %>%
  group_by(game_id) %>%
  summarise(
    score_final  = max(score,      na.rm = TRUE),  # score final atteint
    nb_tours     = n(),                             # nombre de tours joués
    lignes_total = max(full_lines, na.rm = TRUE),  # lignes complétées
    niveau_max   = max(level,      na.rm = TRUE),  # niveau max atteint
    duree        = max(time_start, na.rm = TRUE) - min(time_start, na.rm = TRUE),
    drop_rate    = mean(num_drop,  na.rm = TRUE),  # taux de drop moyen
    inaction_moy = mean(num_none,  na.rm = TRUE),  # inaction moyenne
    .groups = "drop"
  )

# Parties longues (> 20 tours) utilisées dans l'onglet Progression
long_games <- by_game %>% filter(nb_tours > 20)

# 2. CONSTANTES : COULEURS ET CONFIG
# Noms des 7 pièces Tetris
SHAPES <- c("I", "J", "L", "O", "S", "T", "Z")

# Couleurs authentiques des pièces Tetris
COL_I <- "#00BCD4"  # cyan
COL_J <- "#1565C0"  # bleu
COL_L <- "#EF6C00"  # orange
COL_O <- "#F9A825"  # jaune
COL_S <- "#2E7D32"  # vert
COL_T <- "#6A1B9A"  # violet
COL_Z <- "#C62828"  # rouge

# Association nom de pièce → couleur
PIECE_COLORS <- c(COL_I, COL_J, COL_L, COL_O, COL_S, COL_T, COL_Z)
names(PIECE_COLORS) <- SHAPES

# 3. FONCTIONS UTILITAIRES
# Couleur d'une barre selon le score (vert=excellent, rouge=faible)
score_color <- function(s) {
  dplyr::case_when(
    s >= 2000 ~ COL_I,
    s >= 800  ~ COL_S,
    s >= 200  ~ COL_O,
    TRUE      ~ COL_Z
  )
}

# Thème ggplot personnalisé pour tous les graphiques
theme_tetris <- function(legend_pos = "none") {
  theme_minimal() +
    theme(
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.background  = element_rect(fill = "#F8FAFD",     color = NA),
      panel.grid.major  = element_line(color = "#E4E9F2", linewidth = 0.45),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = "#5A6478", size = 12),
      axis.title        = element_text(color = "#3A4560", size = 13, face = "bold"),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.text       = element_text(color = "#5A6478", size = 11),
      legend.title      = element_text(color = "#3A4560", size = 11, face = "bold"),
      legend.key        = element_rect(fill  = "transparent", color = NA),
      legend.position   = legend_pos,
      plot.margin       = margin(16, 20, 16, 16),
      plot.caption      = element_text(color = "#8A96A8", size = 10, margin = margin(t = 8))
    )
}

# 4. CONFIGURATION DE LA NAVIGATION
# Structure des onglets et sous-menus de graphiques
TABS <- list(
  list(id = "vue", label = "Vue générale",
       plots = list(
         list(id = "p_scores",  label = "Score par partie",
              sub = "Couleur selon la performance — vert=excellent, rouge=faible"),
         list(id = "p_scatter", label = "Durée vs Score",
              sub = "Nuage de points — chaque point est une partie"),
         list(id = "p_lignes",  label = "Lignes complétées",
              sub = "Indicateur d'efficacité — couleur par niveau max atteint")
       )
  ),
  list(id = "prog", label = "Progression",
       plots = list(
         list(id = "p_evol",  label = "Évolution du score",
              sub = "Score cumulé tour par tour — points jaunes = lignes complétées"),
         list(id = "p_ratio", label = "Score ratio / tour",
              sub = "Qualité de jeu par tour — la tendance doit monter")
       )
  ),
  list(id = "comp", label = "Comportement",
       plots = list(
         list(id = "p_actions",  label = "Actions par tour",
              sub = "Nombre moyen de chaque action par tour"),
         list(id = "p_drop",     label = "Taux de drop",
              sub = "Rouge = hésitant, Vert = décisif"),
         list(id = "p_inaction", label = "Inaction vs Score",
              sub = "Taille des points proportionnelle au nombre de tours")
       )
  ),
  list(id = "piece", label = "Pièces",
       plots = list(
         list(id = "p_freq", label = "Fréquence",
              sub = "Distribution des 7 pièces Tetris sur toutes les parties"),
         list(id = "p_perf", label = "Performance",
              sub = "Score ratio moyen selon le type de pièce reçue")
       )
  )
)

# Table id → description (utilisée pour afficher le sous-titre du graphique)
all_subs <- list()
for (tab in TABS)
  for (pl in tab$plots)
    all_subs[[pl$id]] <- pl$sub

# 5. INTERFACE UTILISATEUR (UI)
ui <- fluidPage(
  title = "TETRIS Analytics",
  
  # ---------------------------------------------------------------------------
  # 5a. PAGE D'ACCUEIL — s'affiche au démarrage, disparaît au clic du bouton
  # ---------------------------------------------------------------------------
  div(id = "landing",
      
      # CSS spécifique à la page d'accueil
      tags$style(HTML('
      #landing {
        position:fixed; inset:0; z-index:9999;
        background:#EEF2F8;
        font-family:"Nunito",sans-serif;
        overflow:hidden;
      }
 
      /* --- Header de la page d accueil --- */
      .lnd-hdr {
        background:linear-gradient(120deg,#1565C0 0%,#6A1B9A 100%);
        padding:0 40px; height:60px;
        display:flex; align-items:center; justify-content:space-between;
      }
      .lnd-hdr-l { display:flex; align-items:center; gap:14px; }
      .lnd-blocks { display:grid; grid-template-columns:repeat(4,10px); gap:2px; }
      .lnd-blocks span { width:10px;height:10px;border-radius:2px;animation:lbl 2s infinite; }
      .lnd-blocks span:nth-child(1){background:#00BCD4;animation-delay:0s}
      .lnd-blocks span:nth-child(2){background:#F9A825;animation-delay:.2s}
      .lnd-blocks span:nth-child(3){background:#C62828;animation-delay:.4s}
      .lnd-blocks span:nth-child(4){background:#2E7D32;animation-delay:.6s}
      .lnd-blocks span:nth-child(5){background:#6A1B9A;animation-delay:.1s}
      .lnd-blocks span:nth-child(6){background:#00BCD4;animation-delay:.3s}
      .lnd-blocks span:nth-child(7){background:#F9A825;animation-delay:.5s}
      .lnd-blocks span:nth-child(8){background:#C62828;animation-delay:.7s}
      @keyframes lbl{0%,100%{opacity:1}50%{opacity:.25}}
      .lnd-ttl { font-family:"Space Mono",monospace;font-size:15px;font-weight:700;letter-spacing:3px;color:#fff; }
      .lnd-sub { font-size:9px;color:rgba(255,255,255,.6);letter-spacing:2px;margin-top:2px; }
      .lnd-badge {
        background:rgba(255,255,255,.15);border:1px solid rgba(255,255,255,.3);
        color:#fff;font-size:12px;font-weight:700;
        padding:7px 20px;border-radius:20px;letter-spacing:.5px;white-space:nowrap;
      }
 
      /* --- Corps de la page d accueil : gauche + droite --- */
      .lnd-body {
        display:flex; align-items:center; justify-content:center;
        gap:60px; padding:32px 60px;
        height:calc(100vh - 60px);
      }
      .lnd-left { flex:1; max-width:500px; }
      .lnd-right { width:310px; flex-shrink:0; }
 
      /* --- Titre principal --- */
      .lnd-h1 {
        font-family:"Nunito",sans-serif;font-size:58px;font-weight:900;
        line-height:1.1;color:#1E2A3A;margin-bottom:18px;
        letter-spacing:-1px;word-spacing:0px;
      }
      .lnd-h1 .acc {
        background:linear-gradient(120deg,#1565C0,#6A1B9A);
        -webkit-background-clip:text;-webkit-text-fill-color:transparent;background-clip:text;
      }
 
      /* --- Description sous le titre --- */
      .lnd-desc {
        font-size:15px;color:#5A6478;line-height:1.8;
        margin-bottom:32px;font-weight:600;
        border-left:4px solid #B5D4F4;padding-left:18px;
      }
      .lnd-desc strong { color:#1E2A3A;font-weight:800; }
 
      /* --- Cartes de statistiques clés --- */
      .lnd-stats {
        display:flex;background:#fff;
        border:1px solid #DDE3EE;border-radius:16px;
        overflow:hidden;margin-bottom:34px;
        box-shadow:0 2px 16px rgba(30,42,58,.07);
      }
      .lnd-stat { flex:1;padding:20px 22px;border-right:1px solid #DDE3EE;min-width:0; }
      .lnd-stat:last-child { border-right:none; }
      .lnd-sv {
        font-family:"Space Mono",monospace;
        font-size:26px;font-weight:700;color:#1E2A3A;
        line-height:1;margin-bottom:8px;white-space:nowrap;
      }
      .lnd-sl {
        font-size:10px;font-weight:800;color:#8A96A8;
        letter-spacing:1.2px;white-space:nowrap;
        display:flex;align-items:center;gap:5px;
      }
      .lnd-dot-stat { width:8px;height:8px;border-radius:2px;display:inline-block;flex-shrink:0; }
 
      /* --- Bouton d acces au dashboard --- */
      .lnd-btn {
        display:inline-flex;align-items:center;gap:14px;
        background:linear-gradient(120deg,#1565C0,#6A1B9A);
        color:#fff;font-family:"Nunito",sans-serif;
        font-size:15px;font-weight:800;
        padding:17px 42px;border-radius:14px;border:none;cursor:pointer;
        box-shadow:0 8px 28px rgba(21,101,192,.35);
        transition:transform .15s,box-shadow .15s;
      }
      .lnd-btn:hover { transform:translateY(-3px);box-shadow:0 14px 36px rgba(21,101,192,.45); }
      .lnd-arr {
        width:28px;height:28px;background:rgba(255,255,255,.22);
        border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:16px;
      }
 
      /* --- Plateau Tetris animé (droite) --- */
      .lnd-board-wrap {
        background:#fff;border:1px solid #DDE3EE;border-radius:16px;
        padding:20px 18px 16px;box-shadow:0 4px 24px rgba(30,42,58,.07);
      }
      .lnd-board-title {
        display:flex;align-items:center;gap:8px;
        font-size:10px;font-weight:800;color:#8A96A8;letter-spacing:2px;margin-bottom:16px;
      }
      .lnd-live { width:7px;height:7px;border-radius:50%;background:#2E7D32;animation:lv 1s infinite; }
      @keyframes lv{0%,100%{opacity:1}50%{opacity:.2}}
 
      /* --- Mini-cartes thématiques sous le plateau --- */
      .lnd-mini { display:grid;grid-template-columns:1fr 1fr 1fr;gap:8px;margin-top:14px; }
      .lnd-mc { background:#fff;border:1px solid #DDE3EE;border-radius:12px;padding:12px 8px 10px;text-align:center; }
      .lnd-mc-bar { height:4px;border-radius:2px;margin-bottom:9px; }
      .lnd-mc-t { font-size:12px;font-weight:700;color:#1E2A3A; }
      .lnd-mc-s { font-size:10px;color:#8A96A8;margin-top:3px;font-weight:600; }
    ')),
      
      # Header de la landing page
      div(class = "lnd-hdr",
          div(class = "lnd-hdr-l",
              div(class = "lnd-blocks", lapply(1:8, function(i) tags$span())),
              div(
                div(class = "lnd-ttl", "TETRIS ANALYTICS"),
                div(class = "lnd-sub",  "TABLEAU DE BORD")
              )
          ),
          div(class = "lnd-badge", "BUT Science des Donn\u00e9es \u00b7 Promo 2026")
      ),
      
      # Corps : partie gauche (texte) + partie droite (Tetris animé)
      div(class = "lnd-body",
          
          # Colonne gauche : titre, description, stats, bouton
          div(class = "lnd-left",
              div(class = "lnd-h1",
                  HTML("Comprendre<br>le joueur<br><span class='acc'>par les donn\u00e9es</span>")
              ),
              div(class = "lnd-desc",
                  HTML("Analyse statistique du comportement de jeu :<br>
                <strong>scores, strat\u00e9gies, pi\u00e8ces et progression</strong><br>
                19 parties \u00b7 1\u00a0458 tours enregistr\u00e9s.")
              ),
              # 4 indicateurs clés
              div(class = "lnd-stats",
                  div(class = "lnd-stat",
                      div(class = "lnd-sv", "3\u00a0518"),
                      div(class = "lnd-sl",
                          tags$span(class = "lnd-dot-stat", style = "background:#1565C0"),
                          "SCORE MAX"
                      )
                  ),
                  div(class = "lnd-stat",
                      div(class = "lnd-sv", "66"),
                      div(class = "lnd-sl",
                          tags$span(class = "lnd-dot-stat", style = "background:#6A1B9A"),
                          "LIGNES MAX"
                      )
                  ),
                  div(class = "lnd-stat",
                      div(class = "lnd-sv", "19"),
                      div(class = "lnd-sl",
                          tags$span(class = "lnd-dot-stat", style = "background:#2E7D32"),
                          "PARTIES"
                      )
                  ),
                  div(class = "lnd-stat",
                      div(class = "lnd-sv", "1\u00a0458"),
                      div(class = "lnd-sl",
                          tags$span(class = "lnd-dot-stat", style = "background:#F9A825"),
                          "TOURS"
                      )
                  )
              ),
              # Bouton : cache la landing et affiche le dashboard
              tags$button(class = "lnd-btn",
                          onclick = "document.getElementById('landing').style.display='none';",
                          "Acc\u00e9der au dashboard",
                          tags$span(class = "lnd-arr", "\u2192")
              )
          ),
          
          # Colonne droite : plateau Tetris + mini-cartes
          div(class = "lnd-right",
              div(class = "lnd-board-wrap",
                  div(class = "lnd-board-title",
                      tags$span(class = "lnd-live"),
                      "SIMULATION EN DIRECT"
                  ),
                  div(id = "lnd-board")  # plateau généré par JS
              ),
              div(class = "lnd-mini",
                  div(class = "lnd-mc",
                      div(class = "lnd-mc-bar", style = "background:linear-gradient(90deg,#1565C0,#6A1B9A)"),
                      div(class = "lnd-mc-t", "Scores"),
                      div(class = "lnd-mc-s", "Vue g\u00e9n\u00e9rale")
                  ),
                  div(class = "lnd-mc",
                      div(class = "lnd-mc-bar", style = "background:linear-gradient(90deg,#6A1B9A,#C62828)"),
                      div(class = "lnd-mc-t", "Comportement"),
                      div(class = "lnd-mc-s", "Actions")
                  ),
                  div(class = "lnd-mc",
                      div(class = "lnd-mc-bar", style = "background:linear-gradient(90deg,#2E7D32,#F9A825)"),
                      div(class = "lnd-mc-t", "Pi\u00e8ces"),
                      div(class = "lnd-mc-s", "7 types")
                  )
              )
          )
      ),
      
      # JS : moteur du Tetris animé sur la page d'accueil
      tags$script(HTML('
      var LC  = ["#00BCD4","#1565C0","#EF6C00","#F9A825","#2E7D32","#6A1B9A","#C62828"];
      var LNR = 13, LNC = 7;
      var lgrid = new Array(LNR * LNC).fill(0);
 
      // Création de la grille dans le DOM
      var lb = document.getElementById("lnd-board");
      lb.style.cssText = "display:grid;grid-template-columns:repeat("+LNC+",30px);grid-template-rows:repeat("+LNR+",22px);gap:3px;";
      for (var i = 0; i < LNR * LNC; i++) {
        var d = document.createElement("div");
        d.style.cssText = "border-radius:3px;background:#F0F4FA;transition:background .22s;";
        d.id = "lc" + i;
        lb.appendChild(d);
      }
 
      // Les 7 formes de pièces Tetris
      var LS = [
        [[1,1,1,1]],
        [[1,1],[1,1]],
        [[1,1,1],[0,0,1]],
        [[1,1,1],[1,0,0]],
        [[0,1,1],[1,1,0]],
        [[1,1,0],[0,1,1]],
        [[1,1,1],[0,1,0]]
      ];
      var lcur = null;
 
      // Génère une nouvelle pièce aléatoire
      function lnew() {
        var t = Math.floor(Math.random() * LS.length);
        lcur = { t: t, x: Math.floor(Math.random() * (LNC - LS[t][0].length + 1)), y: 0, col: Math.floor(Math.random() * 7) + 1 };
        if (!lcan(0, 0)) { lgrid = new Array(LNR * LNC).fill(0); }
      }
 
      // Vérifie si le mouvement est possible
      function lcan(dx, dy) {
        var p = LS[lcur.t];
        for (var r = 0; r < p.length; r++)
          for (var c = 0; c < p[r].length; c++)
            if (p[r][c]) {
              var nx = lcur.x + c + dx, ny = lcur.y + r + dy;
              if (nx < 0 || nx >= LNC || ny >= LNR) return false;
              if (ny >= 0 && lgrid[ny * LNC + nx] > 0) return false;
            }
        return true;
      }
 
      // Place la pièce et efface les lignes complètes
      function lplace() {
        var p = LS[lcur.t];
        for (var r = 0; r < p.length; r++)
          for (var c = 0; c < p[r].length; c++)
            if (p[r][c]) { var idx = (lcur.y + r) * LNC + (lcur.x + c); if (idx < LNR * LNC) lgrid[idx] = lcur.col; }
        var ng = [];
        for (var r = 0; r < LNR; r++) {
          var full = true;
          for (var c = 0; c < LNC; c++) if (!lgrid[r * LNC + c]) { full = false; break; }
          if (!full) for (var c = 0; c < LNC; c++) ng.push(lgrid[r * LNC + c]);
        }
        while (ng.length < LNR * LNC) ng.unshift(0);
        lgrid = ng;
        lnew();
      }
 
      // Dessine l état actuel de la grille
      function ldraw() {
        var tmp = lgrid.slice();
        var p = LS[lcur.t];
        for (var r = 0; r < p.length; r++)
          for (var c = 0; c < p[r].length; c++)
            if (p[r][c]) { var idx = (lcur.y + r) * LNC + (lcur.x + c); if (idx >= 0 && idx < LNR * LNC) tmp[idx] = lcur.col; }
        for (var i = 0; i < LNR * LNC; i++) {
          var el = document.getElementById("lc" + i);
          if (el) el.style.background = tmp[i] > 0 ? LC[tmp[i] - 1] : "#F0F4FA";
        }
      }
 
      lnew();
      setInterval(function() { ldraw(); if (lcan(0, 1)) lcur.y++; else lplace(); }, 420);
    '))
  ),
  # ---------------------------------------------------------------------------
  # 5b. DASHBOARD PRINCIPAL — affiché après clic sur "Accéder au dashboard"
  # ---------------------------------------------------------------------------
  
  # CSS et polices globales du dashboard
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;600;700;800;900&family=Space+Mono:wght@700&display=swap"),
    tags$style(HTML('
      * { box-sizing:border-box; margin:0; padding:0; }
      html, body { height:100%; overflow:hidden; }
      body { font-family:"Nunito",sans-serif; background:#EEF2F8; color:#1E2A3A; }
      .wrap { display:flex; flex-direction:column; height:100vh; overflow:hidden; }
 
      /* Header du dashboard */
      .hdr {
        background:linear-gradient(120deg,#1565C0 0%,#6A1B9A 100%);
        padding:0 32px; height:62px;
        display:flex; align-items:center; justify-content:space-between; flex-shrink:0;
      }
      .hdr-l { display:flex; align-items:center; gap:14px; }
      .tblocks { display:grid; grid-template-columns:repeat(4,10px); gap:2px; }
      .tblocks span { width:10px;height:10px;border-radius:2px;animation:bl 2s infinite; }
      .tblocks span:nth-child(1){background:#00BCD4;animation-delay:0s}
      .tblocks span:nth-child(2){background:#F9A825;animation-delay:.2s}
      .tblocks span:nth-child(3){background:#C62828;animation-delay:.4s}
      .tblocks span:nth-child(4){background:#2E7D32;animation-delay:.6s}
      .tblocks span:nth-child(5){background:#6A1B9A;animation-delay:.1s}
      .tblocks span:nth-child(6){background:#00BCD4;animation-delay:.3s}
      .tblocks span:nth-child(7){background:#F9A825;animation-delay:.5s}
      .tblocks span:nth-child(8){background:#C62828;animation-delay:.7s}
      @keyframes bl{0%,100%{opacity:1}50%{opacity:.25}}
      .hdr-title { font-family:"Space Mono",monospace;font-size:16px;font-weight:700;letter-spacing:3px;color:#fff; }
      .hdr-sub   { font-size:9px;color:rgba(255,255,255,.6);letter-spacing:2px;margin-top:2px; }
 
      /* KPI dans le header */
      .kpi-bar { display:flex; gap:24px; }
      .kpi-item { text-align:right; }
      .kpi-v { font-family:"Space Mono",monospace;font-size:17px;font-weight:700;color:#fff;line-height:1; }
      .kpi-l { font-size:9px;color:rgba(255,255,255,.55);letter-spacing:1.5px;margin-top:2px; }
 
      /* Onglets de navigation principaux */
      .tab-bar {
        background:#fff; border-bottom:1px solid #DDE3EE;
        padding:0 32px; display:flex; flex-shrink:0;
      }
      .tbtn {
        font-family:"Nunito",sans-serif; font-size:13px; font-weight:700;
        color:#8A96A8; background:transparent; border:none;
        padding:0 22px; height:48px; cursor:pointer;
        border-bottom:3px solid transparent; transition:all .15s;
      }
      .tbtn:hover { color:#1565C0; }
      .tbtn.active { color:#1565C0; border-bottom-color:#1565C0; }
 
      /* Sous-menu de sélection du graphique */
      .plot-bar {
        background:#F4F7FC; border-bottom:1px solid #DDE3EE;
        padding:10px 32px; display:flex; gap:8px;
        flex-wrap:wrap; flex-shrink:0; align-items:center;
      }
      .plot-bar-label {
        font-size:10px; font-weight:800; color:#8A96A8;
        letter-spacing:2px; text-transform:uppercase; margin-right:6px;
      }
      .pbtn {
        font-family:"Nunito",sans-serif; font-size:12px; font-weight:700;
        color:#5A6478; background:#fff;
        border:1.5px solid #DDE3EE;
        padding:6px 16px; border-radius:20px; cursor:pointer; transition:all .15s;
      }
      .pbtn:hover  { border-color:#1565C0; color:#1565C0; }
      .pbtn.active { background:#1565C0; border-color:#1565C0; color:#fff; }
 
      /* Zone principale contenant le graphique */
      .main {
        flex:1; padding:20px 32px 24px;
        display:flex; flex-direction:column; gap:14px;
        overflow:hidden; min-height:0;
      }
      .chart-info  { display:flex; align-items:baseline; gap:12px; flex-shrink:0; }
      .chart-title { font-size:17px; font-weight:800; color:#1E2A3A; }
      .chart-sub   { font-size:12px; color:#8A96A8; font-weight:600; }
      .ctrl-row    { display:flex; align-items:center; gap:12px; flex-shrink:0; }
 
      /* Carte blanche contenant le graphique */
      .chart-card {
        flex:1; background:#fff; border-radius:14px;
        border:1px solid #DDE3EE;
        box-shadow:0 2px 16px rgba(30,42,58,.06);
        padding:16px 20px; min-height:0; overflow:hidden;
      }
 
      /* Style du sélecteur de partie */
      select, .form-control {
        background:#F4F7FC !important; border:1.5px solid #DDE3EE !important;
        color:#1E2A3A !important; border-radius:9px !important;
        font-family:"Nunito",sans-serif !important;
        font-size:13px !important; font-weight:600 !important;
        padding:7px 14px !important;
      }
    '))
  ),
  
  # Structure principale du dashboard
  div(class = "wrap",
      
      # Header avec logo animé et KPI
      div(class = "hdr",
          div(class = "hdr-l",
              div(class = "tblocks", lapply(1:8, function(i) tags$span())),
              div(
                div(class = "hdr-title", "TETRIS ANALYTICS"),
                div(class = "hdr-sub",   "TABLEAU DE BORD \u2014 BUT Science des Donn\u00e9es")
              )
          ),
          div(class = "kpi-bar",
              div(class = "kpi-item",
                  div(class = "kpi-v", format(max(by_game$score_final), big.mark = " ")),
                  div(class = "kpi-l", "MEILLEUR SCORE")),
              div(class = "kpi-item",
                  div(class = "kpi-v", max(by_game$lignes_total)),
                  div(class = "kpi-l", "LIGNES MAX")),
              div(class = "kpi-item",
                  div(class = "kpi-v", nrow(by_game)),
                  div(class = "kpi-l", "PARTIES")),
              div(class = "kpi-item",
                  div(class = "kpi-v", format(nrow(df), big.mark = " ")),
                  div(class = "kpi-l", "TOURS"))
          )
      ),
      
      # Onglets principaux (Vue générale / Progression / Comportement / Pièces)
      div(class = "tab-bar",
          lapply(TABS, function(tab)
            tags$button(
              class   = paste0("tbtn", if (tab$id == "vue") " active" else ""),
              id      = paste0("tb_", tab$id),
              onclick = paste0("switchTab('", tab$id, "')"),
              tab$label
            )
          )
      ),
      
      # Sous-menu : boutons de sélection du graphique (généré dynamiquement)
      div(class = "plot-bar",
          div(class = "plot-bar-label", "Graphique"),
          uiOutput("plot_buttons")
      ),
      
      # Zone principale : titre du graphique + sélecteur + graphique
      div(class = "main",
          
          # Titre et sous-titre du graphique actif
          div(class = "chart-info",
              div(class = "chart-title", textOutput("chart_title", inline = TRUE)),
              div(class = "chart-sub",   textOutput("chart_sub",   inline = TRUE))
          ),
          
          # Sélecteur de partie (visible uniquement pour les graphiques de progression)
          conditionalPanel(
            condition = "input.active_plot == 'p_evol' || input.active_plot == 'p_ratio'",
            div(class = "ctrl-row",
                selectInput("sel_game", label = NULL,
                            choices = setNames(
                              long_games$game_id,
                              paste0("Partie ", long_games$game_id,
                                     "  \u2014  score : ", format(long_games$score_final, big.mark = " "),
                                     "  \u2014  ", long_games$nb_tours, " tours")
                            ),
                            selected = long_games$game_id[which.max(long_games$score_final)],
                            width = "360px"
                )
            )
          ),
          
          # Carte contenant le graphique ggplot
          div(class = "chart-card",
              plotOutput("main_plot", height = "100%", width = "100%")
          )
      )
  ),
  
  # JS : gestion des onglets et de la sélection de graphique
  tags$script(HTML('
    // Change l onglet actif et informe Shiny
    function switchTab(tabId) {
      document.querySelectorAll(".tbtn").forEach(function(e) { e.classList.remove("active"); });
      document.getElementById("tb_" + tabId).classList.add("active");
      Shiny.setInputValue("active_tab", tabId, { priority: "event" });
    }
 
    // Change le graphique actif et informe Shiny
    function selectPlot(plotId) {
      document.querySelectorAll(".pbtn").forEach(function(e) { e.classList.remove("active"); });
      var btn = document.getElementById("pb_" + plotId);
      if (btn) btn.classList.add("active");
      Shiny.setInputValue("active_plot", plotId, { priority: "event" });
    }
 
    // Reçoit l ordre de Shiny d activer un graphique précis
    Shiny.addCustomMessageHandler("setActivePlot", function(plotId) {
      selectPlot(plotId);
    });
 
    // Initialisation au démarrage : onglet Vue générale, graphique Scores
    document.addEventListener("shiny:connected", function() {
      Shiny.setInputValue("active_tab",  "vue",      { priority: "event" });
      Shiny.setInputValue("active_plot", "p_scores", { priority: "event" });
    });
  '))
)

# 6. SERVEUR
server <- function(input, output, session) {
  
  # --- Onglet courant (défaut : Vue générale) ---
  cur_tab <- reactive({
    if (is.null(input$active_tab) || input$active_tab == "") "vue"
    else input$active_tab
  })
  
  # --- Liste des graphiques du tab courant ---
  tab_plots <- reactive({
    tab <- Filter(function(t) t$id == cur_tab(), TABS)
    if (length(tab) == 0) return(list())
    tab[[1]]$plots
  })
  
  # --- Génère les boutons de sélection de graphique ---
  output$plot_buttons <- renderUI({
    plots <- tab_plots()
    if (length(plots) == 0) return(NULL)
    lapply(seq_along(plots), function(i) {
      pl <- plots[[i]]
      tags$button(
        class   = paste0("pbtn", if (i == 1) " active" else ""),
        id      = paste0("pb_", pl$id),
        onclick = paste0("selectPlot('", pl$id, "')"),
        pl$label
      )
    })
  })
  
  # --- Quand on change d'onglet, activer le 1er graphique du tab ---
  observeEvent(input$active_tab, ignoreNULL = FALSE, {
    plots <- tab_plots()
    if (length(plots) > 0)
      session$sendCustomMessage("setActivePlot", plots[[1]]$id)
  })
  
  # --- Graphique actif (défaut : p_scores) ---
  active_plot <- reactive({
    if (is.null(input$active_plot) || input$active_plot == "") "p_scores"
    else input$active_plot
  })
  
  # --- Titre du graphique actif ---
  output$chart_title <- renderText({
    for (tab in TABS)
      for (pl in tab$plots)
        if (pl$id == active_plot()) return(pl$label)
    ""
  })
  
  # --- Sous-titre du graphique actif ---
  output$chart_sub <- renderText({
    sub <- all_subs[[active_plot()]]
    if (is.null(sub)) "" else sub
  })
  
  # --- Données de la partie sélectionnée (onglet Progression) ---
  sel_data <- reactive({
    req(input$sel_game)
    df %>% filter(game_id == as.integer(input$sel_game))
  })
  
  # --- Rendu du graphique principal ---
  output$main_plot <- renderPlot(bg = "transparent", res = 96, {
    id <- active_plot()
    
    # Graphique 1 : Score final par partie
    if (id == "p_scores") {
      by_game %>%
        mutate(col = score_color(score_final)) %>%
        ggplot(aes(factor(game_id), score_final, fill = col)) +
        geom_col(width = 0.72) +
        geom_text(aes(label = ifelse(score_final > 50,
                                     format(score_final, big.mark = " "), "")),
                  vjust = -0.4, color = "#1E2A3A", size = 4, fontface = "bold") +
        scale_fill_identity() +
        scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, .12))) +
        labs(x = "Numéro de partie", y = "Score final") +
        theme_tetris()
      
      # Graphique 2 : Durée vs Score (nuage de points + régression)
    } else if (id == "p_scatter") {
      by_game %>% filter(duree > 0) %>%
        ggplot(aes(duree, score_final)) +
        geom_point(color = "#1565C0", size = 5, alpha = 0.75) +
        geom_smooth(method = "lm", se = TRUE, color = "#C62828",
                    fill = "#C62828", alpha = 0.08, linewidth = 1.4, linetype = "dashed") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Durée (secondes)", y = "Score final") +
        theme_tetris()
      
      # Graphique 3 : Lignes complétées par partie
    } else if (id == "p_lignes") {
      by_game %>%
        ggplot(aes(factor(game_id), lignes_total, fill = factor(niveau_max))) +
        geom_col(width = 0.72) +
        scale_fill_manual(
          values = c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,"5"=COL_T,"6"=COL_Z),
          name = "Niveau max") +
        scale_y_continuous(expand = expansion(mult = c(0, .1))) +
        labs(x = "Partie", y = "Lignes complétées") +
        theme_tetris(legend_pos = "right")
      
      # Graphique 4 : Évolution du score tour par tour
    } else if (id == "p_evol") {
      d <- sel_data()
      ggplot(d, aes(turn_id, score)) +
        geom_area(fill = "#1565C0", alpha = 0.1) +
        geom_line(color = "#1565C0", linewidth = 1.6) +
        geom_point(
          data = d %>% filter(full_lines > lag(full_lines, default = 0)),
          aes(turn_id, score),
          color = COL_O, size = 4, shape = 21, fill = COL_O, stroke = 1.5) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Tour", y = "Score cumulé",
             caption = "Points jaunes = tours avec lignes complétées") +
        theme_tetris()
      
      # Graphique 5 : Score ratio avec tendance linéaire
    } else if (id == "p_ratio") {
      d <- sel_data() %>% filter(!is.na(score_ratio))
      ggplot(d, aes(turn_id, score_ratio)) +
        geom_point(color = "#6A1B9A", size = 2.5, alpha = 0.55) +
        geom_smooth(method = "lm", se = TRUE, color = "#C62828",
                    fill = "#C62828", alpha = 0.08, linewidth = 1.6) +
        labs(x = "Tour", y = "Score ratio") +
        theme_tetris()
      
      # Graphique 6 : Répartition des actions (barres horizontales)
    } else if (id == "p_actions") {
      data.frame(
        action = c("Inaction", "Mvt gauche", "Mvt droite", "Rotation", "Drop", "Descente"),
        val    = c(mean(df$num_none), mean(df$num_move_left), mean(df$num_move_right),
                   mean(df$num_rotate), mean(df$num_drop), mean(df$num_down)),
        col    = c(COL_J, COL_I, COL_S, COL_T, COL_Z, COL_L)
      ) %>%
        ggplot(aes(reorder(action, val), val, fill = col)) +
        geom_col(width = 0.65, show.legend = FALSE) +
        geom_text(aes(label = round(val, 1)),
                  hjust = -0.2, color = "#1E2A3A", size = 5, fontface = "bold") +
        scale_fill_identity() +
        scale_y_continuous(expand = expansion(mult = c(0, .22))) +
        coord_flip() +
        labs(x = NULL, y = "Moyenne par tour") +
        theme_tetris()
      
      # Graphique 7 : Taux de drop par partie
    } else if (id == "p_drop") {
      by_game %>% filter(nb_tours > 5) %>%
        ggplot(aes(factor(game_id), drop_rate, fill = drop_rate)) +
        geom_col(width = 0.72, show.legend = FALSE) +
        scale_fill_gradient(low = "#C62828", high = "#2E7D32") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, 1.1),
                           expand = expansion(mult = c(0, .05))) +
        labs(x = "Partie", y = "Taux de drop") +
        theme_tetris()
      
      # Graphique 8 : Inaction vs Score (bulles)
    } else if (id == "p_inaction") {
      dat <- by_game %>% filter(nb_tours > 5)
      ggplot(dat, aes(inaction_moy, score_final,
                      color = factor(niveau_max), size = nb_tours)) +
        geom_point(alpha = 0.85) +
        geom_smooth(data = dat, aes(inaction_moy, score_final),
                    method = "lm", se = FALSE, inherit.aes = FALSE,
                    color = "#C62828", linewidth = 1.4, linetype = "dashed") +
        scale_color_manual(
          values = c("1"=COL_J,"2"=COL_I,"3"=COL_S,"4"=COL_O,"5"=COL_T,"6"=COL_Z),
          name = "Niveau") +
        scale_size_continuous(name = "Nb tours", range = c(4, 12)) +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "Inaction moyenne / tour", y = "Score final") +
        theme_tetris(legend_pos = "right")
      
      # Graphique 9 : Fréquence des pièces reçues
    } else if (id == "p_freq") {
      df %>% count(shape_id) %>%
        mutate(piece = SHAPES[shape_id + 1], col = PIECE_COLORS[piece]) %>%
        ggplot(aes(reorder(piece, -n), n, fill = col)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = n), vjust = -0.4,
                  color = "#1E2A3A", size = 5, fontface = "bold") +
        scale_fill_identity() +
        scale_y_continuous(expand = expansion(mult = c(0, .12))) +
        labs(x = "Type de pièce", y = "Nombre d'apparitions") +
        theme_tetris()
      
      # Graphique 10 : Score ratio moyen par type de pièce
    } else if (id == "p_perf") {
      df %>% filter(!is.na(score_ratio)) %>%
        group_by(shape_id) %>%
        summarise(sr = mean(score_ratio, na.rm = TRUE), .groups = "drop") %>%
        mutate(piece = SHAPES[shape_id + 1], col = PIECE_COLORS[piece]) %>%
        ggplot(aes(reorder(piece, sr), sr, fill = col)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = round(sr, 1)), vjust = -0.4,
                  color = "#1E2A3A", size = 5, fontface = "bold") +
        scale_fill_identity() +
        scale_y_continuous(expand = expansion(mult = c(0, .14)), limits = c(0, 14)) +
        labs(x = "Type de pièce", y = "Score ratio moyen") +
        theme_tetris()
    }
  })
}

# 7. LANCEMENT DE L'APPLICATION

shinyApp(ui, server)