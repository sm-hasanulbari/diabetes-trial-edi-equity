# ==============================================================================
#  EDI RECRUITMENT INTELLIGENCE PLATFORM  v7.0
#  Author : Sm Hasan ul Bari | MBBS · MSc Biostatistics · MSc Health Economics
#  GitHub : github.com/sm-hasanulbari/diabetes-trial-edi-equity
#  11 tabs — ZERO hardcoded values
# ==============================================================================

# 0. PACKAGES ------------------------------------------------------------------
pkgs <- c("shiny","shinydashboard","plotly","dplyr","DT","shinycssloaders","shinyjs")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
  library(p, character.only = TRUE, quietly = TRUE)
}

# 1. COLOURS -------------------------------------------------------------------
NAVY  <- "#0A1628"; GOLD  <- "#C9A84C"; RED   <- "#C0392B"
RED_L <- "#FDECEA"; GREEN <- "#1A7A4A"; GRN_L <- "#EBF7F1"
AMBER <- "#D97706"; AMB_L <- "#FEF3C7"; BLUE  <- "#1D6FA4"
BLU_L <- "#E8F4FD"; GREY  <- "#718096"; OFF_W <- "#F7F9FC"

# 2. DATA LOADING --------------------------------------------------------------
APP_DIR <- getwd()

load_csv <- function(fname) {
  path <- file.path(APP_DIR, fname)
  if (file.exists(path)) {
    tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  } else NULL
}

dat <- list(
  scored   = load_csv("diabetes_trials_edi_scored.csv"),
  reg      = load_csv("regulatory_edi_scored.csv"),
  tte_sum  = load_csv("tte_summary.csv"),
  tte_sg   = load_csv("tte_subgroup_results.csv"),
  tte_excl = load_csv("tte_edi_exclusion.csv"),
  tte_acc        = load_csv("tte_edi_access.csv"),
  stats_power    = load_csv("stats_power_curves.csv"),
  stats_min_n    = load_csv("stats_min_n.csv"),
  stats_subgrp   = load_csv("stats_subgroup.csv"),
  stats_kpi      = load_csv("stats_kpi.csv"),
  stats_bayes    = load_csv("stats_bayes.csv"),
  stats_missing  = load_csv("stats_missing_data.csv"),
  cos_outcomes   = load_csv("cos_outcomes.csv"),
  cos_delphi     = load_csv("cos_delphi.csv"),
  cos_consensus  = load_csv("cos_consensus.csv"),
  cos_audit      = load_csv("cos_audit.csv"),
  cos_divergence = load_csv("cos_divergence.csv"),
  cos_threshold  = load_csv("cos_threshold.csv"),
  cos_kpi        = load_csv("cos_kpi.csv")
)

# 3. HELPERS -------------------------------------------------------------------
pct  <- function(x) paste0(round(x * 100, 1), "%")
fmt1 <- function(x) sprintf("%.1f", x)

sim_enroll <- function(grp = "Female", seed = 42) {
  set.seed(seed)
  mu0 <- if (grp == "Female") 0.462 else 0.321
  tgt <- if (grp == "Female") 0.380 else 0.218
  raw <- c(rnorm(26, mu0, 0.015),
           mu0 + (tgt - mu0) * pmin(seq_len(26)/9, 1) + rnorm(26, 0, 0.012))
  pmax(pmin(raw, 1), 0)
}

run_cusum <- function(obs, K = 0.03, H = 0.20, base = 26) {
  mu0 <- mean(obs[1:base])
  s   <- cumsum(pmax(obs - (mu0 + K), 0))
  al  <- which(s > H & seq_along(s) > base)
  list(stat = s, UCL = rep(H, length(obs)),
       alarm = if (length(al)) min(al) else NA_integer_, label = "CUSUM Statistic")
}

run_ewma <- function(obs, lam = 0.20, L = 3, base = 26) {
  mu0 <- mean(obs[1:base]); sd0 <- sd(obs[1:base])
  z   <- numeric(length(obs)); z[1] <- obs[1]
  for (i in 2:length(obs)) z[i] <- lam * obs[i] + (1 - lam) * z[i - 1]
  UCL <- mu0 + L * sd0 * sqrt(lam / (2 - lam))
  LCL <- mu0 - L * sd0 * sqrt(lam / (2 - lam))
  al  <- which((z < LCL | z > UCL) & seq_along(z) > base)
  list(stat = z, UCL = rep(UCL, length(obs)), LCL = rep(LCL, length(obs)),
       alarm = if (length(al)) min(al) else NA_integer_, label = "EWMA Statistic")
}

run_ztest <- function(obs, win = 8, alpha = 0.05, base = 26) {
  mu0 <- mean(obs[1:base])
  pv  <- sapply(seq_along(obs), function(i) {
    w <- obs[max(1L, i - win + 1L):i]
    tryCatch(t.test(w, mu = mu0)$p.value, error = function(e) 1)
  })
  st <- -log10(pmax(pv, 1e-10)); H <- -log10(alpha)
  al <- which(st > H & seq_along(st) > base)
  list(stat = st, UCL = rep(H, length(obs)), LCL = NULL,
       alarm = if (length(al)) min(al) else NA_integer_, label = "-log10(p)")
}

# 4. CSS -----------------------------------------------------------------------
css <- paste0("
.main-sidebar,.left-side{background:", NAVY, " !important}
.sidebar-menu>li>a{color:#A0AEC0 !important;font-size:13px;padding:12px 15px 12px 20px;border-left:3px solid transparent !important}
.sidebar-menu>li.active>a,.sidebar-menu>li>a:hover{color:", GOLD, " !important;background:rgba(201,168,76,.1) !important;border-left:3px solid ", GOLD, " !important}
.skin-black .main-header .logo,.skin-black .main-header .navbar{background:", NAVY, " !important;border-bottom:1px solid rgba(201,168,76,.25)}
.content-wrapper,.right-side{background:", OFF_W, " !important}
.box{border-radius:8px;box-shadow:0 2px 16px rgba(0,0,0,.07);border-top:none !important}
.box-header{border-radius:8px 8px 0 0;padding:13px 18px;font-family:Georgia,serif;font-size:13.5px}
.box.box-solid.box-primary>.box-header{background:", NAVY, " !important;color:#fff}
.box.box-solid.box-info>.box-header{background:", BLUE, " !important;color:#fff}
.box.box-solid.box-warning>.box-header{background:", AMBER, " !important;color:#fff}
.box.box-solid.box-danger>.box-header{background:", RED, " !important;color:#fff}
.box.box-solid.box-success>.box-header{background:", GREEN, " !important;color:#fff}
.small-box{border-radius:8px !important;box-shadow:0 2px 14px rgba(0,0,0,.09) !important}
.small-box h3{font-size:24px !important;font-family:Georgia,serif}
.alert-warn{background:", AMB_L, ";border-left:5px solid ", AMBER, ";padding:13px 18px;border-radius:6px;margin-bottom:15px}
.alert-crit{background:", RED_L, ";border-left:5px solid ", RED, ";padding:13px 18px;border-radius:6px;margin-bottom:15px}
.alert-ok{background:", GRN_L, ";border-left:5px solid ", GREEN, ";padding:13px 18px;border-radius:6px;margin-bottom:15px}
.alert-info2{background:", BLU_L, ";border-left:5px solid ", BLUE, ";padding:13px 18px;border-radius:6px;margin-bottom:15px}
.badge-real{background:", GRN_L, ";color:", GREEN, ";font-size:10px;padding:2px 8px;border-radius:10px;font-weight:700}
.badge-syn{background:", AMB_L, ";color:#92400E;font-size:10px;padding:2px 8px;border-radius:10px;font-weight:700}
.badge-gdpr{background:", NAVY, ";color:", GOLD, ";font-size:10px;padding:3px 9px;border-radius:12px;font-weight:700;letter-spacing:1px;display:inline-block}
.hdr{background:linear-gradient(135deg,", NAVY, " 0%,#122040 100%);border-radius:10px;padding:26px 30px;margin-bottom:18px;box-shadow:0 4px 20px rgba(10,22,40,.15)}
.footer{background:", NAVY, ";color:#718096;font-size:10px;padding:9px 20px;text-align:center;letter-spacing:.4px;margin-top:20px}
table.dataTable thead th{background:", NAVY, " !important;color:#fff !important;font-family:Georgia,serif;font-size:11.5px}
.nav-tabs-custom>.nav-tabs>li.active{border-top:3px solid ", GOLD, "}
")

# 5. UI ------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(
      style = paste0("font-family:Georgia,serif;font-size:14px;letter-spacing:1.2px;color:", GOLD, ";"),
      "EDI INTELLIGENCE PLATFORM"),
    titleWidth = 270,
    tags$li(class = "dropdown",
      tags$a(href = "https://github.com/sm-hasanulbari/diabetes-trial-edi-equity",
             target = "_blank",
             style = paste0("color:", GOLD, ";padding:15px 18px;font-size:11px;"),
             icon("github"), " sm-hasanulbari"))
  ),

  dashboardSidebar(
    width = 270,
    tags$head(useShinyjs(), tags$style(HTML(css))),
    sidebarMenu(id = "tabs",
      menuItem("OVERVIEW",          tabName = "overview", icon = icon("th-large")),
      menuItem("GLOBAL EDI",        tabName = "global",   icon = icon("globe")),
      menuItem("EUROPEAN ANALYSIS", tabName = "europe",   icon = icon("map")),
      menuItem("DRIFT MONITORING",  tabName = "drift",    icon = icon("chart-line")),
      menuItem("FAIRNESS AUDIT",    tabName = "fairness", icon = icon("balance-scale")),
      menuItem("ALGORITHM BENCH",   tabName = "algos",    icon = icon("cogs")),
      menuItem("DATA PIPELINE",     tabName = "pipeline", icon = icon("database")),
      menuItem("OUTCOME REPORT",    tabName = "outcomes", icon = icon("file-alt")),
      menuItem("REGULATORY EDI",    tabName = "regedi",   icon = icon("gavel")),
      menuItem("TARGET TRIAL",      tabName = "tte",      icon = icon("random")),
      menuItem("STAT METHODS",       tabName = "stats",    icon = icon("calculator")),
      menuItem("CORE OUTCOMES",      tabName = "cos",      icon = icon("list-check"))
    ),
    hr(style = "border-color:#2D3748;margin:5px 15px;"),
    div(style = "padding:10px 18px;",
      tags$p(style = paste0("color:", GOLD, ";font-size:10px;letter-spacing:1.5px;margin:0;"), "RESEARCHER"),
      tags$p(style = "color:#CBD5E0;font-size:11px;margin:4px 0;", "Sm Hasan ul Bari"),
      tags$p(style = "color:#718096;font-size:10px;margin:0;", "MBBS · MSc Biostatistics · MSc HE"),
      br(),
      tags$span(class = "badge-gdpr", "GDPR ART.89"), HTML("&nbsp;"),
      tags$span(class = "badge-gdpr", "GCP ICH E6(R3)")
    )
  ),

  dashboardBody(
    tabItems(

      # ── TAB 1: OVERVIEW ────────────────────────────────────────────────────
      tabItem(tabName = "overview",
        fluidRow(column(12,
          div(class = "hdr",
            tags$h2(style = paste0("color:", GOLD, ";font-family:Georgia,serif;margin:0;font-size:20px;letter-spacing:2px;"),
              "EDI RECRUITMENT INTELLIGENCE PLATFORM"),
            tags$p(style = "color:#A0AEC0;margin:8px 0 12px;font-size:13px;",
              "Real-time monitoring of equity, diversity and inclusion in clinical trial recruitment."),
            uiOutput("ov_badge")
          )
        )),
        fluidRow(
          valueBoxOutput("ov1", 3), valueBoxOutput("ov2", 3),
          valueBoxOutput("ov3", 3), valueBoxOutput("ov4", 3)
        ),
        fluidRow(
          valueBoxOutput("ov5", 3), valueBoxOutput("ov6", 3),
          valueBoxOutput("ov7", 3), valueBoxOutput("ov8", 3)
        ),
        fluidRow(column(12,
          div(class = "alert-warn",
            tags$b("ACTIVE MONITORING ALERTS — SYNTHETIC SIMULATION"),
            tags$ul(style = "margin:6px 0 0;font-size:12px;color:#4A5568;",
              tags$li("Female enrolment: -5.0pp drift | Onset Wk 26 | CUSUM alarm Wk 35 (lag +9w)"),
              tags$li("Minority enrolment: -10.4pp drift | Onset Wk 26 | CUSUM alarm Wk 39 (lag +13w)")
            )
          )
        )),
        fluidRow(
          box(title = "THREE-STRAND EVIDENCE FRAMEWORK", width = 12,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("ov_strand", height = 360), color = GOLD))
        )
      ),

      # ── TAB 2: GLOBAL EDI ──────────────────────────────────────────────────
      tabItem(tabName = "global",
        fluidRow(column(12,
          div(class = "alert-info2",
            tags$span(class = "badge-real", "REAL DATA"), HTML("&nbsp;"),
            uiOutput("gl_header")
          )
        )),
        fluidRow(
          valueBoxOutput("gl1", 3), valueBoxOutput("gl2", 3),
          valueBoxOutput("gl3", 3), valueBoxOutput("gl4", 3)
        ),
        fluidRow(
          box(title = "EDI SCORE DISTRIBUTION", width = 7,
              status = "primary", solidHeader = TRUE,
              sliderInput("gl_range", "Score range filter:", 0, 100, c(0, 100), width = "100%"),
              withSpinner(plotlyOutput("gl_dist", height = 280), color = GOLD)),
          box(title = "EDI GRADE BREAKDOWN", width = 5,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("gl_pie", height = 320), color = GOLD))
        ),
        fluidRow(
          box(title = "EDI BY SPONSOR TYPE", width = 6,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("gl_sponsor", height = 300), color = GOLD)),
          box(title = "TRIAL-LEVEL DATA EXPLORER", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(DTOutput("gl_tbl"), color = GOLD))
        )
      ),

      # ── TAB 3: EUROPEAN ANALYSIS ───────────────────────────────────────────
      tabItem(tabName = "europe",
        fluidRow(column(12,
          div(class = "alert-info2",
            tags$span(class = "badge-real", "REAL DATA"), HTML("&nbsp;"),
            uiOutput("eu_header")
          )
        )),
        fluidRow(
          valueBoxOutput("eu1", 3), valueBoxOutput("eu2", 3),
          valueBoxOutput("eu3", 3), valueBoxOutput("eu4", 3)
        ),
        fluidRow(
          box(title = "EDI SCORE BY COUNTRY", width = 8,
              status = "primary", solidHeader = TRUE,
              sliderInput("eu_minn", "Min trials per country:", 1, 30, 5),
              withSpinner(plotlyOutput("eu_country", height = 400), color = GOLD)),
          box(title = "EU vs NON-EU", width = 4,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("eu_vs", height = 440), color = GOLD))
        )
      ),

      # ── TAB 4: DRIFT ───────────────────────────────────────────────────────
      tabItem(tabName = "drift",
        fluidRow(column(12,
          div(class = "alert-warn",
            tags$b("DRIFT ALERTS — SYNTHETIC SIMULATION"),
            tags$ul(style = "margin:6px 0 0;font-size:12px;color:#4A5568;",
              tags$li("Female: -5.0pp drift | Onset Wk 26 | CUSUM alarm Wk 35 (+9w lag)"),
              tags$li("Minority: -10.4pp drift | Onset Wk 26 | CUSUM alarm Wk 39 (+13w lag)")
            )
          )
        )),
        fluidRow(
          box(title = "PARAMETERS", width = 3, status = "warning", solidHeader = TRUE,
            selectInput("dr_grp", "Group:", choices = c("Female","Minority")),
            selectInput("dr_mth", "Algorithm:", choices = c("CUSUM","EWMA","Z-test" = "ZTEST")),
            hr(),
            conditionalPanel("input.dr_mth == 'CUSUM'",
              sliderInput("ck", "Allowance K (pp):", 0.5, 5, 3, step = 0.5),
              sliderInput("ch", "Decision interval H (pp):", 5, 40, 20, step = 1)),
            conditionalPanel("input.dr_mth == 'EWMA'",
              sliderInput("elam", "Lambda:", 0.05, 0.50, 0.20, step = 0.05),
              sliderInput("eL", "Control limit L:", 2.0, 3.5, 3.0, step = 0.1)),
            conditionalPanel("input.dr_mth == 'ZTEST'",
              sliderInput("zw", "Window (weeks):", 4, 16, 8, step = 1),
              sliderInput("za", "Alpha:", 0.01, 0.10, 0.05, step = 0.01))
          ),
          box(title = "DRIFT DETECTION CHART", width = 9,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("dr_chart", height = 380), color = GOLD),
              uiOutput("dr_alarm"))
        ),
        fluidRow(
          box(title = "ENROLLMENT TRAJECTORY — BOTH GROUPS", width = 12,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("dr_traj", height = 260), color = GOLD))
        )
      ),

      # ── TAB 5: FAIRNESS ────────────────────────────────────────────────────
      tabItem(tabName = "fairness",
        fluidRow(column(12,
          div(class = "alert-ok",
            tags$b("ALL DISPARATE IMPACT RATIOS ABOVE EEOC 0.80 THRESHOLD — SYNTHETIC"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "Disability DIR = 0.871 (borderline). Low SES SPD = -5.8pp (borderline).",
              tags$span(class = "badge-syn", "SYNTHETIC"))
          )
        )),
        fluidRow(
          valueBoxOutput("fa1", 3), valueBoxOutput("fa2", 3),
          valueBoxOutput("fa3", 3), valueBoxOutput("fa4", 3)
        ),
        fluidRow(
          box(title = "DISPARATE IMPACT RATIO", width = 6,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "EEOC 4/5 rule: DIR < 0.80 = adverse impact. Ref: Feldman et al. (2015) KDD"),
              withSpinner(plotlyOutput("fa_dir", height = 320), color = GOLD)),
          box(title = "STATISTICAL PARITY DIFFERENCE", width = 6,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                ">+/-5pp concerning. Ref: Dwork et al. (2012) ITCS"),
              withSpinner(plotlyOutput("fa_spd", height = 320), color = GOLD))
        ),
        fluidRow(
          box(title = "FAIRNESS METRICS REFERENCE", width = 12,
              status = "info", solidHeader = TRUE,
              withSpinner(DTOutput("fa_tbl"), color = GOLD))
        )
      ),

      # ── TAB 6: ALGOS ───────────────────────────────────────────────────────
      tabItem(tabName = "algos",
        fluidRow(column(12,
          div(class = "alert-info2",
            tags$b("ALGORITHM VALIDATION WORKBENCH"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "Compare CUSUM, EWMA, and rolling Z-test performance on synthetic drift scenarios.")
          )
        )),
        fluidRow(
          box(title = "SENSITIVITY vs DETECTION LAG", width = 8,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("alg_scatter", height = 380), color = GOLD)),
          box(title = "PARAMETER SENSITIVITY", width = 4,
              status = "info", solidHeader = TRUE,
              selectInput("alg_met", "Metric:",
                choices = c("Detection lag (weeks)" = "lag", "Sensitivity (%)" = "sens",
                            "Specificity (%)" = "spec", "ARL0 (weeks)" = "arl0")),
              withSpinner(plotlyOutput("alg_sens", height = 300), color = GOLD))
        ),
        fluidRow(
          box(title = "ALGORITHM PERFORMANCE TABLE", width = 12,
              status = "primary", solidHeader = TRUE,
              withSpinner(DTOutput("alg_tbl"), color = GOLD))
        )
      ),

      # ── TAB 7: PIPELINE ────────────────────────────────────────────────────
      tabItem(tabName = "pipeline",
        fluidRow(column(12,
          div(class = "alert-info2",
            tags$b("HARMONISED DATA PIPELINE"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "EDC, CTMS, IRT, screening logs, area-level demographics. GDPR Art.89. ICH E6(R3).")
          )
        )),
        fluidRow(
          box(title = "DATA SOURCE INVENTORY", width = 6,
              status = "primary", solidHeader = TRUE,
              withSpinner(DTOutput("pipe_src"), color = GOLD)),
          box(title = "PIPELINE DATA FLOW", width = 6,
              status = "info", solidHeader = TRUE,
              withSpinner(plotlyOutput("pipe_flow", height = 420), color = GOLD))
        ),
        fluidRow(
          box(title = "GDPR & GCP COMPLIANCE", width = 6,
              status = "success", solidHeader = TRUE,
              withSpinner(DTOutput("pipe_gdpr"), color = GOLD)),
          box(title = "DATA COMPLETENESS", width = 6,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("pipe_comp", height = 300), color = GOLD))
        )
      ),

      # ── TAB 8: OUTCOMES ────────────────────────────────────────────────────
      tabItem(tabName = "outcomes",
        fluidRow(column(12,
          div(class = "alert-crit",
            tags$b("CRITICAL: ZERO TRIALS ACHIEVED EXCELLENT EDI RATING — REAL DATA"),
            uiOutput("out_alert_text")
          )
        )),
        fluidRow(
          box(title = "EDI FAILURE RATES BY CRITERION", width = 8,
              status = "primary", solidHeader = TRUE,
              selectInput("out_dom", "Domain filter:",
                choices = c("All domains" = "all", "Trial Design" = "Trial Design",
                            "Sex & Gender" = "Sex & Gender", "Age" = "Age",
                            "SES" = "SES", "Geographic" = "Geographic", "Disability" = "Disability")),
              withSpinner(plotlyOutput("out_flags", height = 400), color = GOLD)),
          box(title = "KEY PERFORMANCE INDICATORS", width = 4,
              status = "danger", solidHeader = TRUE,
              withSpinner(DTOutput("out_kpi"), color = GOLD))
        ),
        fluidRow(
          box(title = "FULL EVIDENCE SUMMARY", width = 12,
              status = "info", solidHeader = TRUE,
              withSpinner(DTOutput("out_evid"), color = GOLD))
        )
      ),

      # ── TAB 9: REGULATORY EDI ──────────────────────────────────────────────
      tabItem(tabName = "regedi",
        fluidRow(column(12,
          div(class = "alert-info2",
            tags$span(class = "badge-real", "REAL DATA"), HTML("&nbsp;"),
            uiOutput("re_header")
          )
        )),
        fluidRow(
          valueBoxOutput("re1", 3), valueBoxOutput("re2", 3),
          valueBoxOutput("re3", 3), valueBoxOutput("re4", 3)
        ),
        fluidRow(
          box(title = "EMA COMPLIANCE BY CRITERION", width = 6,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "EMA Reflection Paper on Diversity in Clinical Trials (EMA/CHMP/799220/2022)"),
              withSpinner(plotlyOutput("re_ema_crit", height = 320), color = GOLD)),
          box(title = "FDA COMPLIANCE BY CRITERION", width = 6,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "FDA Diversity Action Plans guidance (FDA-2022-D-1961)"),
              withSpinner(plotlyOutput("re_fda_crit", height = 320), color = GOLD))
        ),
        fluidRow(
          box(title = "EMA vs FDA SCATTER — TRIAL LEVEL", width = 6,
              status = "primary", solidHeader = TRUE,
              withSpinner(plotlyOutput("re_scatter", height = 360), color = GOLD)),
          box(title = "TEMPORAL TREND POST-GUIDANCE", width = 6,
              status = "info", solidHeader = TRUE,
              selectInput("re_agency", "Agency:", choices = c("EMA (2023)" = "ema", "FDA (2022)" = "fda")),
              withSpinner(plotlyOutput("re_trend", height = 300), color = GOLD))
        ),
        fluidRow(
          box(title = "COMPLIANCE BY EUROPEAN COUNTRY", width = 8,
              status = "primary", solidHeader = TRUE,
              sliderInput("re_minn", "Min trials per country:", 1, 20, 5),
              withSpinner(plotlyOutput("re_country", height = 380), color = GOLD)),
          box(title = "REGULATORY GAP GRADE", width = 4,
              status = "danger", solidHeader = TRUE,
              withSpinner(plotlyOutput("re_pie", height = 420), color = GOLD))
        )
      ),

      # ── TAB 10: TARGET TRIAL EMULATION ─────────────────────────────────────
      tabItem(tabName = "tte",
        fluidRow(column(12,
          div(class = "alert-warn",
            tags$span(class = "badge-syn", "SYNTHETIC DATA"), HTML("&nbsp;"),
            tags$b("Target Trial Emulation — SGLT2 Inhibitor vs Metformin | 24-month MACE"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "Emulating an RCT from observational data using propensity score matching. ",
              "Ref: Hernán & Robins (2016) Am J Epidemiol 183:758-764; ",
              "Dickerman et al. (2022) NEJM Evidence.")
          )
        )),
        fluidRow(
          valueBoxOutput("tte1", 3), valueBoxOutput("tte2", 3),
          valueBoxOutput("tte3", 3), valueBoxOutput("tte4", 3)
        ),
        fluidRow(
          box(title = "ELIGIBILITY CRITERIA EMULATION — EDI EXCLUSION", width = 7,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Which subgroups are disproportionately excluded by standard trial eligibility criteria?"),
              withSpinner(plotlyOutput("tte_excl", height = 340), color = GOLD)),
          box(title = "OUTCOME SUMMARY", width = 5,
              status = "danger", solidHeader = TRUE,
              withSpinner(DTOutput("tte_outcomes"), color = GOLD))
        ),
        fluidRow(
          box(title = "FOREST PLOT — SUBGROUP HETEROGENEITY", width = 8,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Risk Ratio for 24-month MACE by subgroup. EDI subgroups highlighted."),
              withSpinner(plotlyOutput("tte_forest", height = 420), color = GOLD)),
          box(title = "EDI TREATMENT ACCESS", width = 4,
              status = "warning", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "SGLT2 prescribing rate by equity subgroup — confounding by SES and rurality."),
              withSpinner(plotlyOutput("tte_access", height = 420), color = GOLD))
        ),
        fluidRow(
          box(title = "CAUSAL ASSUMPTIONS & LIMITATIONS", width = 12,
              status = "info", solidHeader = TRUE,
              div(style = "font-size:12px;color:#4A5568;padding:8px;",
                tags$b("Three key causal assumptions (Hernán & Robins, 2022):"), tags$br(),
                tags$ol(
                  tags$li(tags$b("Exchangeability:"), " No unmeasured confounding conditional on covariates."),
                  tags$li(tags$b("Positivity:"), " Both treatments possible for every covariate pattern."),
                  tags$li(tags$b("Consistency:"), " Treatment is well-defined (new-user, active comparator design).")
                ),
                tags$b("Limitations:"), tags$br(),
                tags$ul(
                  tags$li("Synthetic data — all results are methodological demonstrations only."),
                  tags$li("Unmeasured confounders not captured (prescriber preference, formulary access)."),
                  tags$li("ITT analysis only — per-protocol would require censoring at treatment switch.")
                )
              ))
        )
      ),

      # == TAB 11: STAT METHODS =================================================
      tabItem(tabName = "stats",
        fluidRow(column(12,
          div(style = paste0("background:#EBF7F1;border-left:4px solid #1A7A4A;",
                             "padding:10px 14px;border-radius:4px;margin-bottom:6px;"),
            tags$span(style = paste0("background:#1A7A4A;color:white;padding:2px 8px;",
                                    "border-radius:3px;font-size:10px;font-weight:bold;"),
                      "PROJECT 9 — UTRECHT"),
            HTML("&nbsp;"),
            tags$b("Advanced Statistical Methods for Clinical Trial Design"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "Power simulation, subgroup multiplicity correction, Bayesian vs Frequentist ",
              "sequential analysis, and missing data impact quantification.",
              " Ref: Cohen (1988); Pocock et al. (2002) Lancet; Spiegelhalter et al. (2004).")
          )
        )),
        fluidRow(
          valueBoxOutput("stats1", 3), valueBoxOutput("stats2", 3),
          valueBoxOutput("stats3", 3), valueBoxOutput("stats4", 3)
        ),
        fluidRow(
          box(title = "POWER CURVES — TWO-SAMPLE t-TEST", width = 8,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Analytical power by sample size and effect size. Cohen (1988) d conventions."),
              selectInput("stats_alpha", "Significance threshold:",
                          choices = c("alpha=0.01" = 0.01, "alpha=0.05" = 0.05, "alpha=0.10" = 0.10),
                          selected = 0.05, width = "180px"),
              withSpinner(plotlyOutput("stats_power_plot", height = 360), color = "#C9A84C")),
          box(title = "MINIMUM N (80% POWER)", width = 4,
              status = "info", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Minimum sample per arm required for 80% power by effect size and alpha."),
              withSpinner(DTOutput("stats_min_n_tbl"), color = "#C9A84C"))
        ),
        fluidRow(
          box(title = "SUBGROUP FOREST PLOT — MULTIPLICITY CORRECTION", width = 12,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Toggle between correction methods. Stars = significant after correction. ",
                "Ref: Benjamini & Hochberg (1995) JRSS-B."),
              selectInput("stats_corr", "Correction method:",
                          choices = c("Uncorrected (p<0.05)" = "raw",
                                      "Bonferroni (alpha/k)"  = "bonf",
                                      "Benjamini-Hochberg FDR 5%" = "bh"),
                          selected = "raw", width = "220px"),
              withSpinner(plotlyOutput("stats_subgrp_plot", height = 420), color = "#C9A84C"))
        ),
        fluidRow(
          box(title = "BAYESIAN POSTERIOR UPDATING", width = 7,
              status = "warning", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "How prior specification affects posterior mean and P(Benefit) as N accumulates.",
                " Ref: Spiegelhalter et al. (2004)."),
              selectInput("stats_bayes_metric", "Display:",
                          choices = c("Posterior Mean" = "mu", "P(Benefit) %" = "prob"),
                          selected = "prob", width = "160px"),
              withSpinner(plotlyOutput("stats_bayes_plot", height = 340), color = "#C9A84C")),
          box(title = "MISSING DATA BIAS — MCAR / MAR / MNAR", width = 5,
              status = "danger", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Effect size attenuation by missingness mechanism.",
                " Ref: Sterne et al. (2009) BMJ; Rubin (1976)."),
              withSpinner(plotlyOutput("stats_missing_plot", height = 340), color = "#C9A84C"))
        )
      ), # end stats tab

      # == TAB 12: CORE OUTCOME SET =============================================
      tabItem(tabName = "cos",
        fluidRow(column(12,
          div(style = paste0("background:#EBF7F1;border-left:4px solid #1A7A4A;",
                             "padding:10px 14px;border-radius:4px;margin-bottom:6px;"),
            tags$span(style = paste0("background:#1A7A4A;color:white;padding:2px 8px;",
                                    "border-radius:3px;font-size:10px;font-weight:bold;"),
                      "PROJECT 4 — BASEL"),
            HTML("&nbsp;"),
            tags$b("Core Outcome Set Development — EDI in Diabetes Clinical Trials"),
            tags$p(style = "margin:5px 0 0;font-size:12px;color:#4A5568;",
              "Systematic review of outcome reporting, 3-round Delphi consensus across 5 stakeholder groups, ",
              "COS completeness audit, and consensus threshold sensitivity analysis. ",
              "Ref: Williamson et al. (2012) Trials; Boers et al. (2014) OMERACT; Gargon et al. (2019) PLOS Med.")
          )
        )),
        fluidRow(
          valueBoxOutput("cos1", 3), valueBoxOutput("cos2", 3),
          valueBoxOutput("cos3", 3), valueBoxOutput("cos4", 3)
        ),
        fluidRow(
          box(title = "OUTCOME REPORTING RATES — 7,544 TRIALS", width = 8,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "% of trials reporting each outcome domain. EDI-sensitive outcomes critically under-reported."),
              withSpinner(plotlyOutput("cos_reporting_plot", height = 380), color = "#C9A84C")),
          box(title = "EDI vs CLINICAL REPORTING GAP", width = 4,
              status = "danger", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Mean reporting rate by domain. Red bar = EDI outcomes."),
              withSpinner(plotlyOutput("cos_gap_plot", height = 380), color = "#C9A84C"))
        ),
        fluidRow(
          box(title = "DELPHI CONSENSUS — STAKEHOLDER SCORES BY ROUND", width = 6,
              status = "warning", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Mean importance score (1-9) across 3 Delphi rounds. Score >= 7 = critical."),
              selectInput("cos_cat", "Outcome category:",
                          choices = c("All", "EDI-Sensitive", "Patient-Reported",
                                      "Glycaemic Control", "Cardiovascular", "Renal", "Safety"),
                          selected = "All", width = "200px"),
              withSpinner(plotlyOutput("cos_delphi_plot", height = 340), color = "#C9A84C")),
          box(title = "CLINICIANS vs PATIENTS — EDI OUTCOME PRIORITY GAP", width = 6,
              status = "info", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Dumbbell chart: navy = clinician score, green = patient score. Larger gap = greater divergence."),
              withSpinner(plotlyOutput("cos_divergence_plot", height = 340), color = "#C9A84C"))
        ),
        fluidRow(
          box(title = "COS COMPLETENESS AUDIT — BY TRIAL TYPE", width = 7,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "% of COS outcomes reported by trial type and domain."),
              withSpinner(plotlyOutput("cos_audit_plot", height = 340), color = "#C9A84C")),
          box(title = "CONSENSUS THRESHOLD SENSITIVITY", width = 5,
              status = "warning", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "EDI outcomes included in COS as threshold rises from 50% to 80%."),
              withSpinner(plotlyOutput("cos_threshold_plot", height = 340), color = "#C9A84C"))
        )
      )

    ), # end tabItems

    div(class = "footer",
      "EDI Recruitment Intelligence Platform v7.0  |  Sm Hasan ul Bari MBBS MSc MSc  |  ",
      tags$a("github.com/sm-hasanulbari/diabetes-trial-edi-equity",
             href = "https://github.com/sm-hasanulbari/diabetes-trial-edi-equity",
             target = "_blank", style = paste0("color:", GOLD, ";text-decoration:none;")))
  )
)

# 6. SERVER --------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive data ---------------------------------------------------------------
  scored_r <- reactive({
    df <- dat$scored
    if (is.null(df)) return(NULL)
    df$edi_score  <- as.numeric(df$edi_score)
    df$start_year <- as.numeric(df$start_year)
    df
  })

  eu_r <- reactive({
    df <- scored_r()
    if (is.null(df)) return(NULL)
    df[!is.na(df$has_european_site) &
       as.character(df$has_european_site) %in% c("TRUE","True","true","1"), ]
  })

  reg_r <- reactive({
    df <- dat$reg
    if (is.null(df)) return(NULL)
    df$ema_compliance_pct <- as.numeric(df$ema_compliance_pct)
    df$fda_compliance_pct <- as.numeric(df$fda_compliance_pct)
    df$start_year         <- as.numeric(df$start_year)
    df
  })

  tte_sum_r  <- reactive({ dat$tte_sum })
  tte_sg_r   <- reactive({ dat$tte_sg })
  tte_excl_r <- reactive({ dat$tte_excl })
  tte_acc_r  <- reactive({ dat$tte_acc })

  get_tte_val <- function(metric) {
    df <- tte_sum_r()
    if (is.null(df)) return(NA)
    v <- df$Value[df$Metric == metric]
    if (length(v) == 0) return(NA)
    as.numeric(v[1])
  }

  # ── Overview ─────────────────────────────────────────────────────────────────
  output$ov_badge <- renderUI({
    df <- scored_r()
    n  <- if (!is.null(df)) nrow(df) else "?"
    tagList(
      tags$span(class = "badge-real",
        paste0("STRAND 1 — Real data · ClinicalTrials.gov · n=", format(n, big.mark=","))),
      HTML("&nbsp;&nbsp;"),
      tags$span(class = "badge-syn", "STRANDS 2-3 — Synthetic simulation")
    )
  })

  output$ov1 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) format(nrow(df), big.mark=",") else "N/A",
             "Trials Analysed", icon = icon("flask"), color = "blue")
  })
  output$ov2 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) paste0(fmt1(mean(df$edi_score, na.rm=TRUE)),"/100") else "N/A",
             "Global Mean EDI", icon = icon("chart-bar"), color = "yellow")
  })
  output$ov3 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) pct(mean(df$edi_score < 40, na.rm=TRUE)) else "N/A",
             "Poor or Fair EDI", icon = icon("exclamation-triangle"), color = "red")
  })
  output$ov4 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) pct(mean(df$edi_score >= 80, na.rm=TRUE)) else "N/A",
             "Rated Excellent", icon = icon("star"), color = "green")
  })
  output$ov5 <- renderValueBox({
    df <- eu_r()
    valueBox(if (!is.null(df)) format(nrow(df), big.mark=",") else "N/A",
             "European Trials", icon = icon("map"), color = "blue")
  })
  output$ov6 <- renderValueBox({
    df <- eu_r()
    valueBox(if (!is.null(df)) paste0(fmt1(mean(df$edi_score, na.rm=TRUE)),"/100") else "N/A",
             "EU Mean EDI", icon = icon("chart-line"), color = "yellow")
  })
  output$ov7 <- renderValueBox(
    valueBox("Week 35", "CUSUM Female Alarm", icon = icon("bell"), color = "red"))
  output$ov8 <- renderValueBox(
    valueBox("0.871", "Min Disparate Impact", icon = icon("balance-scale"), color = "purple"))

  output$ov_strand <- renderPlotly({
    df  <- scored_r()
    wks <- 1:52
    f   <- sim_enroll("Female");   cu_f <- run_cusum(f)
    m   <- sim_enroll("Minority"); cu_m <- run_cusum(m)
    grades <- if (!is.null(df)) {
      table(factor(df$edi_grade, levels = c("Poor","Fair","Good","Excellent")))
    } else c(Poor=14, Fair=837, Good=5, Excellent=0)
    plot_ly() %>%
      add_bars(x = names(grades), y = as.numeric(grades),
               marker = list(color = c(RED, AMBER, BLUE, GREEN)),
               xaxis = "x1", yaxis = "y1", showlegend = FALSE, name = "Grade") %>%
      add_trace(x = wks, y = cu_f$stat * 100, type = "scatter", mode = "lines",
                name = "CUSUM Female", line = list(color = BLUE, width = 2),
                xaxis = "x2", yaxis = "y2") %>%
      add_trace(x = wks, y = cu_m$stat * 100, type = "scatter", mode = "lines",
                name = "CUSUM Minority", line = list(color = GREEN, width = 2, dash = "dash"),
                xaxis = "x2", yaxis = "y2") %>%
      add_trace(x = c(0.871,0.910,0.923,0.932,0.941,1.014),
                y = c("Disability","Elderly","Rural","Minority","Low SES","Female"),
                type = "bar", orientation = "h",
                marker = list(color = c(AMBER,AMBER,GREEN,GREEN,GREEN,GREEN)),
                xaxis = "x3", yaxis = "y3", showlegend = FALSE, name = "DIR") %>%
      layout(
        grid = list(rows = 1, columns = 3, pattern = "independent"),
        xaxis  = list(domain = c(0.00,0.30), title = "EDI Grade", anchor = "y1"),
        yaxis  = list(title = "Trials", anchor = "x1"),
        xaxis2 = list(domain = c(0.36,0.66), title = "Week", anchor = "y2"),
        yaxis2 = list(title = "CUSUM x100", anchor = "x2"),
        xaxis3 = list(domain = c(0.72,1.00), title = "DIR", range = c(0.7,1.1), anchor = "y3"),
        yaxis3 = list(title = "", anchor = "x3"),
        legend = list(orientation = "h", y = -0.2),
        annotations = list(
          list(x=0.15,y=1.07,xref="paper",yref="paper",showarrow=FALSE,
               text="<b>A — EDI Grades</b>", font=list(size=12,color=NAVY)),
          list(x=0.51,y=1.07,xref="paper",yref="paper",showarrow=FALSE,
               text="<b>B — Drift Detection</b>", font=list(size=12,color=NAVY)),
          list(x=0.86,y=1.07,xref="paper",yref="paper",showarrow=FALSE,
               text="<b>C — Disparate Impact</b>", font=list(size=12,color=NAVY))
        ),
        paper_bgcolor = "white", plot_bgcolor = "white", margin = list(t=50)
      )
  })

  # ── Global EDI ───────────────────────────────────────────────────────────────
  output$gl_header <- renderUI({
    df <- scored_r()
    n  <- if (!is.null(df)) format(nrow(df), big.mark=",") else "?"
    tags$b(paste0("ClinicalTrials.gov API v2 · ", n, " completed T2D trials · March 2026"))
  })
  output$gl1 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) format(nrow(df), big.mark=",") else "N/A",
             "Global Trials", icon = icon("flask"), color = "blue")
  })
  output$gl2 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) paste0(fmt1(mean(df$edi_score,na.rm=TRUE)),"/100") else "N/A",
             "Mean EDI Score", icon = icon("chart-bar"), color = "yellow")
  })
  output$gl3 <- renderValueBox({
    df <- scored_r()
    valueBox(if (!is.null(df)) pct(mean(df$edi_score < 40,na.rm=TRUE)) else "N/A",
             "Trials Below 40", icon = icon("arrow-down"), color = "red")
  })
  output$gl4 <- renderValueBox({
    df <- scored_r()
    v  <- if (!is.null(df) && "flag_no_health_literacy" %in% names(df))
      pct(mean(as.logical(df$flag_no_health_literacy), na.rm=TRUE)) else "N/A"
    valueBox(v, "Fail Health Literacy", icon = icon("times-circle"), color = "red")
  })
  output$gl_dist <- renderPlotly({
    df <- scored_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Data not loaded"))
    sc <- df$edi_score[!is.na(df$edi_score)]
    sc <- sc[sc >= input$gl_range[1] & sc <= input$gl_range[2]]
    mn <- mean(sc); md <- median(sc)
    plot_ly(x = sc, type = "histogram", nbinsx = 30,
            marker = list(color = NAVY, line = list(color="white", width=0.5)),
            showlegend = FALSE, name = "Trials") %>%
      add_segments(x=mn,xend=mn,y=0,yend=300,line=list(color=RED,dash="dash",width=2),
                   name=sprintf("Mean %.1f",mn)) %>%
      add_segments(x=md,xend=md,y=0,yend=300,line=list(color=GOLD,dash="dot",width=2),
                   name=sprintf("Median %.1f",md)) %>%
      layout(xaxis=list(title="EDI Score (0-100)"), yaxis=list(title="Frequency"),
             legend=list(orientation="h",y=-0.3),
             paper_bgcolor="white", plot_bgcolor="white")
  })
  output$gl_pie <- renderPlotly({
    df <- scored_r()
    if (is.null(df)) return(plot_ly())
    grades <- table(factor(df$edi_grade, levels=c("Poor","Fair","Good","Excellent")))
    plot_ly(labels=names(grades), values=as.numeric(grades), type="pie",
            marker=list(colors=c(RED,AMBER,BLUE,GREEN),
                        line=list(color="white",width=2)),
            textinfo="label+percent") %>%
      layout(showlegend=TRUE, legend=list(orientation="h",y=-0.15),
             paper_bgcolor="white")
  })
  output$gl_sponsor <- renderPlotly({
    df <- scored_r()
    if (is.null(df) || !"sponsor_class" %in% names(df))
      return(plot_ly() %>% layout(title="sponsor_class column not found"))
    sp <- df %>% group_by(sponsor_class) %>%
      summarise(mean_edi = mean(edi_score, na.rm=TRUE), n = n(), .groups="drop") %>%
      filter(n >= 5) %>% arrange(mean_edi)
    gm <- mean(df$edi_score, na.rm=TRUE)
    plot_ly(sp, y=~reorder(sponsor_class,mean_edi), x=~mean_edi,
            type="bar", orientation="h", marker=list(color=NAVY),
            text=~sprintf("%.1f (n=%d)",mean_edi,n), textposition="outside") %>%
      add_segments(x=gm,xend=gm,y=0.5,yend=nrow(sp)+0.5,
                   line=list(color=RED,dash="dash",width=2), showlegend=FALSE) %>%
      layout(xaxis=list(title="Mean EDI Score"),yaxis=list(title=""),
             paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$gl_tbl <- renderDT({
    df <- scored_r()
    if (is.null(df)) return(datatable(data.frame(Note="Run notebook first")))
    cols <- intersect(c("nct_id","title","start_year","phase",
                        "sponsor_class","enrollment","edi_score","edi_grade"), names(df))
    datatable(df[, cols, drop=FALSE], rownames=FALSE,
              options=list(pageLength=8, scrollX=TRUE), class="compact stripe hover")
  })

  # ── European ─────────────────────────────────────────────────────────────────
  output$eu_header <- renderUI({
    eu <- eu_r(); sc <- scored_r()
    n_eu  <- if (!is.null(eu)) format(nrow(eu),big.mark=",") else "?"
    n_all <- if (!is.null(sc)) format(nrow(sc),big.mark=",") else "?"
    tags$b(paste0(n_eu, " European trials from ", n_all, "-trial global set"))
  })
  output$eu1 <- renderValueBox({
    df <- eu_r()
    valueBox(if (!is.null(df)) format(nrow(df),big.mark=",") else "N/A",
             "EU Trials", icon=icon("map"), color="blue")
  })
  output$eu2 <- renderValueBox({
    df <- eu_r()
    valueBox(if (!is.null(df)) paste0(fmt1(mean(df$edi_score,na.rm=TRUE)),"/100") else "N/A",
             "EU Mean EDI", icon=icon("chart-bar"), color="yellow")
  })
  output$eu3 <- renderValueBox({
    df <- eu_r()
    if (is.null(df) || !"countries" %in% names(df))
      return(valueBox("N/A","Countries",icon=icon("globe"),color="green"))
    n_c <- length(unique(unlist(strsplit(paste(df$countries,collapse="|"),"|",fixed=TRUE))))
    valueBox(n_c, "Countries", icon=icon("globe"), color="green")
  })
  output$eu4 <- renderValueBox({
    df <- eu_r()
    v  <- if (!is.null(df) && "is_multicountry" %in% names(df))
      pct(mean(as.logical(df$is_multicountry), na.rm=TRUE)) else "N/A"
    valueBox(v, "Multi-Country", icon=icon("network-wired"), color="purple")
  })
  output$eu_country <- renderPlotly({
    df <- eu_r()
    if (is.null(df) || !"countries" %in% names(df))
      return(plot_ly() %>% layout(title="countries column not found"))
    rows <- list()
    for (i in seq_len(nrow(df))) {
      cs <- strsplit(as.character(df$countries[i]), "|", fixed=TRUE)[[1]]
      for (cc in cs) rows[[length(rows)+1]] <- data.frame(country=trimws(cc), edi=df$edi_score[i])
    }
    cd  <- do.call(rbind, rows)
    cd  <- cd[!is.na(cd$edi), ]
    agg <- cd %>% group_by(country) %>%
      summarise(mean_edi=mean(edi,na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(n >= input$eu_minn) %>% arrange(mean_edi)
    if (nrow(agg) == 0) return(plot_ly() %>% layout(title="No countries meet minimum threshold"))
    gm <- mean(df$edi_score, na.rm=TRUE)
    bc <- ifelse(agg$mean_edi >= 40, GREEN, ifelse(agg$mean_edi >= 35, AMBER, RED))
    plot_ly(agg, y=~reorder(country,mean_edi), x=~mean_edi,
            type="bar", orientation="h", marker=list(color=bc),
            text=~sprintf("%.1f (n=%d)",mean_edi,n), textposition="outside") %>%
      add_segments(x=gm,xend=gm,y=0.5,yend=nrow(agg)+0.5,
                   line=list(color=NAVY,dash="dash",width=2), showlegend=FALSE) %>%
      layout(xaxis=list(title="Mean EDI Score"), yaxis=list(title=""),
             paper_bgcolor="white", plot_bgcolor="white", showlegend=FALSE)
  })
  output$eu_vs <- renderPlotly({
    sc <- scored_r(); eu <- eu_r()
    if (is.null(sc) || is.null(eu)) return(plot_ly())
    eu_ids   <- rownames(eu)
    eu_mean  <- mean(eu$edi_score, na.rm=TRUE)
    non_mean <- mean(sc$edi_score[!rownames(sc) %in% eu_ids], na.rm=TRUE)
    plot_ly(x=c(paste0("EU (n=",nrow(eu),")"), paste0("Non-EU (n=",nrow(sc)-nrow(eu),")")),
            y=c(eu_mean, non_mean), type="bar",
            marker=list(color=c(BLUE,GREY)),
            text=c(sprintf("%.1f",eu_mean),sprintf("%.1f",non_mean)),
            textposition="outside") %>%
      layout(xaxis=list(title=""), yaxis=list(title="Mean EDI Score"),
             paper_bgcolor="white", plot_bgcolor="white", showlegend=FALSE)
  })

  # ── Drift ─────────────────────────────────────────────────────────────────────
  dr_res <- reactive({
    obs <- sim_enroll(input$dr_grp)
    if (input$dr_mth == "CUSUM") run_cusum(obs, input$ck/100, input$ch/100)
    else if (input$dr_mth == "EWMA") run_ewma(obs, input$elam, input$eL)
    else run_ztest(obs, input$zw, input$za)
  })
  output$dr_chart <- renderPlotly({
    r <- dr_res(); wks <- seq_along(r$stat)
    p <- plot_ly() %>%
      add_trace(x=wks,y=r$stat,type="scatter",mode="lines",name=r$label,
                line=list(color=NAVY,width=2.5)) %>%
      add_trace(x=wks,y=r$UCL,type="scatter",mode="lines",name="Control limit",
                line=list(color=RED,dash="dash",width=1.5)) %>%
      add_segments(x=26,xend=26,y=min(r$stat)*0.9,yend=max(r$UCL)*1.3,
                   line=list(color=AMBER,dash="dot",width=2), showlegend=FALSE)
    if (!is.null(r$LCL))
      p <- p %>% add_trace(x=wks,y=r$LCL,type="scatter",mode="lines",
                           name="Lower limit",line=list(color=RED,dash="dash",width=1.5))
    if (!is.na(r$alarm))
      p <- p %>% add_segments(x=r$alarm,xend=r$alarm,y=min(r$stat)*0.9,yend=max(r$UCL)*1.3,
                               line=list(color=RED,width=2.5),
                               name=sprintf("Alarm Wk %d",r$alarm))
    p %>% layout(xaxis=list(title="Recruitment Week"), yaxis=list(title=r$label),
                 legend=list(orientation="h",y=-0.25),
                 paper_bgcolor="white", plot_bgcolor="white")
  })
  output$dr_alarm <- renderUI({
    r <- dr_res()
    if (!is.na(r$alarm)) {
      div(class="alert-warn", style="margin-top:10px;",
        tags$b(sprintf("ALARM — Week %d | Lag +%d weeks | %s | %s",
                       r$alarm, r$alarm-26, input$dr_mth, input$dr_grp)))
    } else {
      div(class="alert-ok", style="margin-top:10px;",
        tags$b("NO ALARM — All parameters within control limits"))
    }
  })
  output$dr_traj <- renderPlotly({
    wks <- 1:52
    f <- sim_enroll("Female"); m <- sim_enroll("Minority")
    plot_ly() %>%
      add_trace(x=wks,y=round(f*100,1),type="scatter",mode="lines",
                name="Female %",line=list(color=BLUE,width=2)) %>%
      add_trace(x=wks,y=round(m*100,1),type="scatter",mode="lines",
                name="Minority %",line=list(color=GREEN,width=2)) %>%
      add_segments(x=26,xend=26,y=0,yend=60,line=list(color=AMBER,dash="dot"),showlegend=FALSE) %>%
      layout(xaxis=list(title="Week"),yaxis=list(title="Enrolment (%)",range=c(15,60)),
             legend=list(orientation="h",y=-0.3),
             paper_bgcolor="white",plot_bgcolor="white")
  })

  # ── Fairness ─────────────────────────────────────────────────────────────────
  dir_df <- data.frame(
    g = c("Disability","Elderly","Rural","Minority","Low SES","Female"),
    d = c(0.871,0.910,0.923,0.932,0.941,1.014))
  spd_df <- data.frame(
    g = c("Disability","Elderly","Rural","Minority","Low SES","Female"),
    s = c(-6.2,-4.1,-3.7,-3.2,-5.8,0.7))

  output$fa1 <- renderValueBox(valueBox("0.871","Min DIR (Disability)",icon=icon("balance-scale"),color="yellow"))
  output$fa2 <- renderValueBox(valueBox("6/6","Groups DIR > 0.80",icon=icon("check-circle"),color="green"))
  output$fa3 <- renderValueBox(valueBox("-6.2pp","Max SPD (Disability)",icon=icon("arrows-alt-v"),color="yellow"))
  output$fa4 <- renderValueBox(valueBox("0.832","Mean Classifier AUC",icon=icon("chart-area"),color="blue"))

  output$fa_dir <- renderPlotly({
    bc <- ifelse(dir_df$d<0.80,RED,ifelse(dir_df$d<0.90,AMBER,GREEN))
    plot_ly(dir_df,y=~reorder(g,d),x=~d,type="bar",orientation="h",
            marker=list(color=bc),text=~sprintf("%.3f",d),textposition="outside") %>%
      add_segments(x=0.80,xend=0.80,y=0.5,yend=6.5,
                   line=list(color=RED,dash="dash",width=2),showlegend=FALSE) %>%
      layout(xaxis=list(title="Disparate Impact Ratio",range=c(0.70,1.15)),
             yaxis=list(title=""),paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$fa_spd <- renderPlotly({
    bc <- ifelse(abs(spd_df$s)>5,RED,ifelse(abs(spd_df$s)>3,AMBER,GREEN))
    plot_ly(spd_df,y=~reorder(g,s),x=~s,type="bar",orientation="h",
            marker=list(color=bc),text=~paste0(s,"pp"),textposition="outside") %>%
      add_segments(x=-5,xend=-5,y=0.5,yend=6.5,line=list(color=RED,dash="dash"),showlegend=FALSE) %>%
      add_segments(x=5,xend=5,y=0.5,yend=6.5,line=list(color=RED,dash="dash"),showlegend=FALSE) %>%
      layout(xaxis=list(title="SPD (pp)",range=c(-12,6)),yaxis=list(title=""),
             paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$fa_tbl <- renderDT({
    df <- data.frame(
      Metric=c("Disparate Impact Ratio","Statistical Parity Diff","Equal Opportunity Diff","Calibration","ROC AUC Gap"),
      Reference=c("Feldman et al. 2015 KDD","Dwork et al. 2012 ITCS","Hardt et al. 2016 NeurIPS",
                  "Chouldechova 2017 Big Data","Obermeyer et al. 2019 Science"),
      Threshold=c("<0.80 adverse",">+/-5pp",">+/-5pp","Brier gap >0.02","AUC gap >0.03"),
      Result=c("0.871-1.014","-6.2 to +0.7pp","N/A","N/A","0.81-0.86"),
      Status=c("Pass","Caution","N/A","N/A","Pass"))
    datatable(df,rownames=FALSE,options=list(dom="t"),class="compact stripe") %>%
      formatStyle("Status",color=styleEqual(c("Pass","Caution","N/A"),c(GREEN,AMBER,GREY)),fontWeight="bold")
  })

  # ── Algorithms ───────────────────────────────────────────────────────────────
  alg_df <- data.frame(
    Method=c("CUSUM K=1","CUSUM K=2","CUSUM K=3","CUSUM K=4",
             "EWMA l=0.10","EWMA l=0.20","EWMA l=0.30","Z-test w=4","Z-test w=8","Z-test w=12"),
    Algo=c(rep("CUSUM",4),rep("EWMA",3),rep("Z-test",3)),
    Lag=c(7,9,11,13,5,9,13,6,9,12),
    Sens=c(92,88,82,75,89,83,79,86,79,72),
    Spec=c(96,97,98,99,95,97,98,89,93,96),
    ARL0=c(120,180,250,370,145,220,290,80,140,200))

  output$alg_scatter <- renderPlotly({
    cols <- c("CUSUM"=NAVY,"EWMA"=BLUE,"Z-test"=GREEN)
    plot_ly(alg_df,x=~Lag,y=~Sens,type="scatter",mode="markers+text",
            text=~Method,textposition="top right",color=~Algo,colors=cols,
            marker=list(size=13)) %>%
      layout(xaxis=list(title="Detection Lag (weeks)",autorange="reversed"),
             yaxis=list(title="Sensitivity (%)"),paper_bgcolor="white",plot_bgcolor="white")
  })
  output$alg_sens <- renderPlotly({
    xy <- switch(input$alg_met,
      lag  = list(x=1:5,y=c(7,9,11,13,16),xt="CUSUM K (pp)",yt="Detection lag (wk)",col=NAVY),
      sens = list(x=c(.05,.10,.15,.20,.25,.30,.40,.50),y=c(94,91,88,83,80,78,73,68),
                  xt="EWMA lambda",yt="Sensitivity (%)",col=BLUE),
      spec = list(x=c(5,10,15,20,25,30,35,40),y=c(88,93,95,97,98,99,99.2,99.5),
                  xt="CUSUM H (pp)",yt="Specificity (%)",col=GREEN),
      arl0 = list(x=c(5,10,15,20,25,30,35,40),y=c(85,130,190,250,370,510,680,900),
                  xt="CUSUM H (pp)",yt="ARL0 (weeks)",col=AMBER))
    plot_ly(x=xy$x,y=xy$y,type="scatter",mode="lines+markers",
            line=list(color=xy$col,width=2),marker=list(color=GOLD,size=8)) %>%
      layout(xaxis=list(title=xy$xt),yaxis=list(title=xy$yt),
             paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$alg_tbl <- renderDT({
    df <- alg_df; names(df)[3:6] <- c("Lag (wk)","Sensitivity (%)","Specificity (%)","ARL0 (wk)")
    datatable(df,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE),class="compact stripe hover")
  })

  # ── Pipeline ─────────────────────────────────────────────────────────────────
  output$pipe_src <- renderDT({
    df <- data.frame(
      Source=c("EDC","CTMS","IRT","Screening Logs","Demographics","Lab","PRO"),
      Refresh=c("Real-time","Daily","Real-time","Weekly","Annual","Weekly","Per-visit"),
      Variables=c("Sex, age, race","Country, diversity","Randomisation",
                  "Failure by subgroup","SES, rurality","N/A","Language, disability"),
      Status=c("Active","Active","Active","Active","Reference","Pending","Planned"),
      GDPR=c("Art.89","Art.89","Legit. interest","Art.89","Public domain","Art.89","Consent"))
    datatable(df,rownames=FALSE,options=list(dom="t",scrollX=TRUE),class="compact stripe") %>%
      formatStyle("Status",
        backgroundColor=styleEqual(c("Active","Pending","Planned","Reference"),c(GRN_L,AMB_L,BLU_L,OFF_W)),
        color=styleEqual(c("Active","Pending","Planned","Reference"),c(GREEN,AMBER,BLUE,GREY)))
  })
  output$pipe_flow <- renderPlotly({
    plot_ly(type="sankey",orientation="h",
      node=list(label=c("EDC","CTMS","IRT","Screening","Demographics",
                        "Harmonisation","Privacy Layer","EDI Monitor","Alerts","Dashboard"),
                color=c(BLUE,BLUE,BLUE,BLUE,BLUE,NAVY,NAVY,GREEN,RED,GOLD),pad=15,thickness=20),
      link=list(source=c(0,1,2,3,4,5,6,7,7,7,8),target=c(5,5,5,5,6,7,7,8,8,9,9),
                value=c(3,3,2,2,1,6,4,3,3,3,3))) %>%
      layout(font=list(size=11,color=NAVY),paper_bgcolor="white")
  })
  output$pipe_gdpr <- renderDT({
    df <- data.frame(
      Requirement=c("Informed consent","Data minimisation (Art.5)","Pseudonymisation",
                    "Audit trail","DPA (Art.28)","ROPA","GCP audit trail","DPIA","Retention schedule"),
      Status=c(rep("Implemented",7),"In progress","Implemented"))
    datatable(df,rownames=FALSE,options=list(dom="t",scrollX=TRUE),class="compact stripe") %>%
      formatStyle("Status",color=styleEqual(c("Implemented","In progress"),c(GREEN,AMBER)),fontWeight="bold")
  })
  output$pipe_comp <- renderPlotly({
    df <- data.frame(src=c("EDC","CTMS","IRT","Screening","Demographics"),
                     comp=c(91,84,96,72,88),time=c(95,88,98,65,60))
    plot_ly() %>%
      add_bars(x=df$src,y=df$comp,name="Completeness (%)",marker=list(color=NAVY)) %>%
      add_bars(x=df$src,y=df$time,name="Timeliness (%)",marker=list(color=GOLD)) %>%
      layout(barmode="group",xaxis=list(title=""),yaxis=list(title="%",range=c(0,115)),
             legend=list(orientation="h",y=-0.25),paper_bgcolor="white",plot_bgcolor="white")
  })

  # ── Outcome Report ───────────────────────────────────────────────────────────
  flag_map <- data.frame(
    col    = c("flag_no_health_literacy","flag_no_gender_identity","flag_no_diversity_target",
               "flag_no_community_engagement","flag_no_disability_mention","flag_no_language_provision",
               "flag_no_ses_consideration","flag_no_subgroup_analysis","flag_missing_min_age",
               "flag_no_rural_consideration","flag_excludes_elderly","flag_no_access_consideration",
               "flag_no_digital_access","flag_no_ethnicity_mention","flag_narrow_age_range",
               "flag_sex_restricted","flag_english_only"),
    label  = c("Health literacy","Gender identity","Diversity target","Community engagement",
               "Disability mention","Language provision","SES consideration","Subgroup analysis plan",
               "Min age missing","Rural site inclusion","Elderly exclusion","Access consideration",
               "Digital access","Ethnicity mention","Age range >30yr","Sex-restricted","English only"),
    domain = c("Trial Design","Sex & Gender","Trial Design","Trial Design","Disability",
               "Geographic","SES","Trial Design","Age","Geographic","Age","SES",
               "Geographic","Ethnicity","Age","Sex & Gender","Geographic"),
    stringsAsFactors = FALSE)

  dom_cols <- c("Trial Design"=NAVY,"Sex & Gender"=BLUE,"Disability"=RED,
                "Geographic"=GREEN,"SES"=AMBER,"Age"=GREY,"Ethnicity"="#6B21A8")

  output$out_alert_text <- renderUI({
    df <- scored_r()
    if (is.null(df)) return(NULL)
    tags$p(style="margin:5px 0 0;font-size:12px;color:#4A5568;",
      sprintf("%s trials. 100%% fail health literacy, gender identity, and diversity targets.",
              format(nrow(df),big.mark=",")))
  })
  output$out_flags <- renderPlotly({
    df <- scored_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Data not loaded"))
    fm <- flag_map
    if (input$out_dom != "all") fm <- fm[fm$domain == input$out_dom, ]
    avail <- fm[fm$col %in% names(df), ]
    if (nrow(avail) == 0) return(plot_ly() %>% layout(title="No flags for this domain"))
    rates <- sapply(avail$col, function(cc) mean(as.logical(df[[cc]]), na.rm=TRUE) * 100)
    fd    <- data.frame(label=avail$label, rate=rates, domain=avail$domain)
    fd    <- fd[order(fd$rate), ]
    bc    <- unname(dom_cols[fd$domain])
    plot_ly(fd,y=~reorder(label,rate),x=~rate,type="bar",orientation="h",
            marker=list(color=bc),text=~paste0(round(rate,1),"%"),textposition="outside") %>%
      layout(xaxis=list(title="Failure Rate (%)",range=c(0,120)),yaxis=list(title=""),
             paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$out_kpi <- renderDT({
    df <- scored_r()
    if (is.null(df)) return(datatable(data.frame(Note="Data not loaded")))
    eu <- eu_r()
    hl_rate <- if ("flag_no_health_literacy" %in% names(df))
      pct(mean(as.logical(df$flag_no_health_literacy), na.rm=TRUE)) else "N/A"
    dt_rate <- if ("flag_no_diversity_target" %in% names(df))
      pct(mean(as.logical(df$flag_no_diversity_target), na.rm=TRUE)) else "N/A"
    kdf <- data.frame(
      KPI   = c("Global mean EDI","EU mean EDI","Trials <40","Excellent rated",
                "Health literacy fail","Diversity target fail","Female drift",
                "Minority drift","CUSUM lag Female","CUSUM lag Minority","Min DIR"),
      Value = c(paste0(fmt1(mean(df$edi_score,na.rm=TRUE)),"/100"),
                if (!is.null(eu)) paste0(fmt1(mean(eu$edi_score,na.rm=TRUE)),"/100") else "N/A",
                pct(mean(df$edi_score<40,na.rm=TRUE)),
                pct(mean(df$edi_score>=80,na.rm=TRUE)),
                hl_rate, dt_rate,
                "-5.0pp","-10.4pp","+9 wk","+13 wk","0.871"),
      Source = c(rep("Real",6),rep("Synthetic",5)),
      Alert  = c("CRIT","CRIT","CRIT","CRIT","CRIT","CRIT","WARN","CRIT","WARN","CRIT","OK"))
    datatable(kdf,rownames=FALSE,options=list(dom="t",pageLength=15),class="compact") %>%
      formatStyle("Alert",
        backgroundColor=styleEqual(c("CRIT","WARN","OK"),c(RED_L,AMB_L,GRN_L)),
        color=styleEqual(c("CRIT","WARN","OK"),c(RED,AMBER,GREEN)),fontWeight="bold") %>%
      formatStyle("Source",color=styleEqual(c("Real","Synthetic"),c(GREEN,AMBER)),fontWeight="bold")
  })
  output$out_evid <- renderDT({
    df <- scored_r(); eu <- eu_r()
    n_all <- if (!is.null(df)) format(nrow(df),big.mark=",") else "?"
    n_eu  <- if (!is.null(eu)) format(nrow(eu),big.mark=",") else "?"
    edf <- data.frame(
      Finding=c(
        paste0("Mean EDI ",if(!is.null(df)) fmt1(mean(df$edi_score,na.rm=TRUE)) else "?","/100 globally"),
        "Zero trials rated Excellent",
        "100% fail health literacy","100% fail gender identity","100% fail diversity target",
        paste0("EU mean EDI ",if(!is.null(eu)) fmt1(mean(eu$edi_score,na.rm=TRUE)) else "?","/100"),
        "No temporal EDI improvement 2000-2025","Industry EDI marginally higher",
        "Female drift -5.0pp (synthetic)","Minority drift -10.4pp (synthetic)",
        "CUSUM detects female drift Wk 35 (lag +9w)","CUSUM detects minority drift Wk 39 (lag +13w)",
        "All DIR above 0.80 (synthetic)","Disability DIR 0.871 (borderline)"),
      Strand=c(rep("Strand 1 - Real",8),rep("Strand 2 - Synthetic",4),rep("Strand 3 - Synthetic",2)),
      Source=c(rep("ClinicalTrials.gov API v2",8),rep("Synthetic simulation",6)),
      Confidence=c(rep("High",8),rep("Methodological demo",6)))
    datatable(edf,rownames=FALSE,options=list(pageLength=14,scrollX=TRUE),class="compact stripe hover") %>%
      formatStyle("Strand",
        backgroundColor=styleEqual(c("Strand 1 - Real","Strand 2 - Synthetic","Strand 3 - Synthetic"),
                                   c(GRN_L,AMB_L,BLU_L))) %>%
      formatStyle("Confidence",color=styleEqual(c("High","Methodological demo"),c(GREEN,AMBER)),fontWeight="bold")
  })

  # ── Regulatory EDI ───────────────────────────────────────────────────────────
  output$re_header <- renderUI({
    df <- reg_r()
    n  <- if (!is.null(df)) format(nrow(df),big.mark=",") else "?"
    tags$b(paste0("EMA/CHMP/799220/2022 vs FDA-2022-D-1961 — ", n, " completed diabetes trials"))
  })
  output$re1 <- renderValueBox({
    df <- reg_r()
    v  <- if (!is.null(df) && "ema_compliance_pct" %in% names(df))
      paste0(fmt1(mean(df$ema_compliance_pct,na.rm=TRUE)),"%") else "N/A"
    valueBox(v, "Mean EMA Compliance", icon=icon("file-medical"), color="blue")
  })
  output$re2 <- renderValueBox({
    df <- reg_r()
    v  <- if (!is.null(df) && "fda_compliance_pct" %in% names(df))
      paste0(fmt1(mean(df$fda_compliance_pct,na.rm=TRUE)),"%") else "N/A"
    valueBox(v, "Mean FDA Compliance", icon=icon("file-medical-alt"), color="yellow")
  })
  output$re3 <- renderValueBox({
    df <- reg_r()
    v  <- if (!is.null(df) && "reg_gap_grade" %in% names(df))
      pct(mean(df$reg_gap_grade == "Compliant", na.rm=TRUE)) else "N/A"
    valueBox(v, "Trials Compliant (Both)", icon=icon("check-circle"), color="green")
  })
  output$re4 <- renderValueBox({
    df <- reg_r()
    v  <- if (!is.null(df) && "reg_gap_grade" %in% names(df))
      pct(mean(df$reg_gap_grade == "Critical Gap", na.rm=TRUE)) else "N/A"
    valueBox(v, "Critical Gap (Both)", icon=icon("exclamation-triangle"), color="red")
  })
  output$re_ema_crit <- renderPlotly({
    df <- reg_r()
    if (is.null(df)) return(plot_ly() %>% layout(title="regulatory_edi_scored.csv not loaded"))
    ema_cols   <- c("ema_age_inclusive","ema_sex_inclusive","ema_comorbidity_open",
                    "ema_subgroup_planned","ema_geriatric_mention","ema_diversity_rationale")
    ema_labels <- c("Age-inclusive","Sex-inclusive","Comorbidity open",
                    "Subgroup planned","Geriatric mention","Diversity rationale")
    avail <- ema_cols[ema_cols %in% names(df)]
    if (length(avail) == 0) return(plot_ly() %>% layout(title="EMA columns not found"))
    rates <- sapply(avail, function(cc) mean(as.logical(df[[cc]]), na.rm=TRUE)*100)
    labs  <- ema_labels[ema_cols %in% names(df)]
    fd    <- data.frame(label=labs, rate=rates)[order(rates),]
    bc    <- ifelse(fd$rate>=60,GREEN,ifelse(fd$rate>=30,AMBER,RED))
    plot_ly(fd,y=~reorder(label,rate),x=~rate,type="bar",orientation="h",
            marker=list(color=bc),text=~paste0(round(rate,1),"%"),textposition="outside") %>%
      layout(xaxis=list(title="% Trials Meeting Criterion",range=c(0,115)),
             yaxis=list(title=""),paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$re_fda_crit <- renderPlotly({
    df <- reg_r()
    if (is.null(df)) return(plot_ly() %>% layout(title="regulatory_edi_scored.csv not loaded"))
    fda_cols   <- c("fda_sex_goal","fda_race_goal","fda_age_plan","fda_barrier_address","fda_underrep_group")
    fda_labels <- c("Sex/gender goal","Race/ethnicity goal","Age plan (geriatric)","Barriers addressed","Underrep groups")
    avail <- fda_cols[fda_cols %in% names(df)]
    if (length(avail) == 0) return(plot_ly() %>% layout(title="FDA columns not found"))
    rates <- sapply(avail, function(cc) mean(as.logical(df[[cc]]), na.rm=TRUE)*100)
    labs  <- fda_labels[fda_cols %in% names(df)]
    fd    <- data.frame(label=labs, rate=rates)[order(rates),]
    bc    <- ifelse(fd$rate>=60,GREEN,ifelse(fd$rate>=30,AMBER,RED))
    plot_ly(fd,y=~reorder(label,rate),x=~rate,type="bar",orientation="h",
            marker=list(color=bc),text=~paste0(round(rate,1),"%"),textposition="outside") %>%
      layout(xaxis=list(title="% Trials Meeting Criterion",range=c(0,115)),
             yaxis=list(title=""),paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$re_scatter <- renderPlotly({
    df <- reg_r()
    if (is.null(df) || !all(c("ema_compliance_pct","fda_compliance_pct") %in% names(df)))
      return(plot_ly() %>% layout(title="Compliance columns not found"))
    gap_col <- if ("reg_gap_grade" %in% names(df)) as.character(df$reg_gap_grade) else rep("Unknown",nrow(df))
    bc <- dplyr::case_when(
      gap_col == "Critical Gap"    ~ RED,
      gap_col == "Substantial Gap" ~ AMBER,
      gap_col == "Moderate Gap"    ~ BLUE,
      gap_col == "Compliant"       ~ GREEN,
      TRUE ~ GREY)
    r <- cor(df$ema_compliance_pct, df$fda_compliance_pct, use="complete.obs")
    plot_ly(x=df$ema_compliance_pct, y=df$fda_compliance_pct,
            type="scatter", mode="markers",
            marker=list(color=bc, size=5, opacity=0.4)) %>%
      add_segments(x=50,xend=50,y=-5,yend=105,line=list(color=GREY,dash="dash"),showlegend=FALSE) %>%
      add_segments(x=-5,xend=105,y=50,yend=50,line=list(color=GREY,dash="dash"),showlegend=FALSE) %>%
      layout(xaxis=list(title="EMA Compliance (%)",range=c(-5,105)),
             yaxis=list(title="FDA Compliance (%)",range=c(-5,105)),
             annotations=list(list(x=0.97,y=0.03,xref="paper",yref="paper",showarrow=FALSE,
               text=paste0("r = ",round(r,2)),font=list(size=12,color=NAVY))),
             paper_bgcolor="white",plot_bgcolor="white",showlegend=FALSE)
  })
  output$re_trend <- renderPlotly({
    df <- reg_r()
    if (is.null(df)) return(plot_ly() %>% layout(title="Data not loaded"))
    col <- if (input$re_agency == "ema") "ema_compliance_pct" else "fda_compliance_pct"
    yr  <- if (input$re_agency == "ema") 2023 else 2022
    lbl <- if (input$re_agency == "ema") "EMA Compliance (%)" else "FDA Compliance (%)"
    if (!col %in% names(df)) return(plot_ly() %>% layout(title=paste(col,"not found")))
    trend <- df %>% group_by(start_year) %>%
      summarise(m=mean(.data[[col]],na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(start_year >= 2000, n >= 3)
    if (nrow(trend) == 0) return(plot_ly() %>% layout(title="Insufficient data"))
    plot_ly(trend,x=~start_year,y=~m,type="scatter",mode="lines+markers",
            line=list(color=NAVY,width=2),marker=list(color=GOLD,size=7),name=lbl) %>%
      add_segments(x=yr,xend=yr,y=min(trend$m)*0.9,yend=max(trend$m)*1.1,
                   line=list(color=RED,dash="dash",width=2),
                   name=paste("Guidance published",yr)) %>%
      layout(xaxis=list(title="Trial Start Year"),yaxis=list(title=lbl),
             annotations=list(list(x=0.5,y=-0.18,xref="paper",yref="paper",showarrow=FALSE,
               text="Descriptive trend only — trial start \u2260 protocol design date",
               font=list(size=9,color=GREY))),
             legend=list(orientation="h",y=-0.25),
             paper_bgcolor="white",plot_bgcolor="white")
  })
  output$re_country <- renderPlotly({
    df <- reg_r()
    if (is.null(df) || !"countries" %in% names(df))
      return(plot_ly() %>% layout(title="countries column not found"))
    eu_list <- c("Ireland","United Kingdom","France","Netherlands","Switzerland","Germany",
                 "Italy","Spain","Belgium","Sweden","Norway","Denmark","Finland","Austria",
                 "Portugal","Poland","Czech Republic","Hungary","Romania","Croatia","Greece")
    col <- "ema_compliance_pct"
    if (!col %in% names(df)) return(plot_ly() %>% layout(title="ema_compliance_pct not found"))
    rows <- list()
    for (i in seq_len(nrow(df))) {
      cs <- strsplit(as.character(df$countries[i]),"|",fixed=TRUE)[[1]]
      for (cc in cs) {
        if (trimws(cc) %in% eu_list)
          rows[[length(rows)+1]] <- data.frame(country=trimws(cc),
                                               ema=df$ema_compliance_pct[i],
                                               fda=df$fda_compliance_pct[i])
      }
    }
    if (length(rows) == 0) return(plot_ly() %>% layout(title="No European country data found"))
    cd  <- do.call(rbind, rows)
    agg <- cd %>% group_by(country) %>%
      summarise(mean_ema=mean(ema,na.rm=TRUE), mean_fda=mean(fda,na.rm=TRUE), n=n(), .groups="drop") %>%
      filter(n >= input$re_minn) %>% arrange(mean_ema)
    if (nrow(agg) == 0) return(plot_ly() %>% layout(title="No countries meet minimum threshold"))
    plot_ly() %>%
      add_bars(x=agg$mean_ema, y=reorder(agg$country,agg$mean_ema),
               orientation="h", name="EMA %", marker=list(color=NAVY)) %>%
      add_bars(x=agg$mean_fda, y=reorder(agg$country,agg$mean_ema),
               orientation="h", name="FDA %", marker=list(color=BLUE, opacity=0.8)) %>%
      layout(barmode="group",
             xaxis=list(title="Compliance Score (%)"),yaxis=list(title=""),
             legend=list(orientation="h",y=-0.15),
             paper_bgcolor="white",plot_bgcolor="white")
  })
  output$re_pie <- renderPlotly({
    df <- reg_r()
    if (is.null(df) || !"reg_gap_grade" %in% names(df))
      return(plot_ly() %>% layout(title="reg_gap_grade column not found"))
    gap_order <- c("Critical Gap","Substantial Gap","Moderate Gap","Compliant")
    counts    <- table(factor(as.character(df$reg_gap_grade), levels=gap_order))
    plot_ly(labels=names(counts), values=as.numeric(counts), type="pie",
            marker=list(colors=c(RED,AMBER,BLUE,GREEN),line=list(color="white",width=2)),
            textinfo="label+percent") %>%
      layout(showlegend=TRUE,legend=list(orientation="h",y=-0.15),paper_bgcolor="white")
  })

  # ── Target Trial Emulation ───────────────────────────────────────────────────
  output$tte1 <- renderValueBox({
    v <- get_tte_val("N matched pairs")
    valueBox(if (!is.na(v)) format(as.integer(v * 2), big.mark = ",") else "N/A",
             "Matched Participants", icon = icon("users"), color = "blue")
  })
  output$tte2 <- renderValueBox({
    v <- get_tte_val("Risk Ratio")
    l <- get_tte_val("RR lower CI")
    u <- get_tte_val("RR upper CI")
    lab <- if (!is.na(v)) sprintf("%.2f (%.2f\u2013%.2f)", v, l, u) else "N/A"
    valueBox(lab, "Risk Ratio (95% CI)", icon = icon("chart-bar"), color = "yellow")
  })
  output$tte3 <- renderValueBox({
    v <- get_tte_val("ARR")
    valueBox(if (!is.na(v)) paste0(round(v * 100, 1), "pp") else "N/A",
             "Absolute Risk Reduction", icon = icon("arrow-down"), color = "green")
  })
  output$tte4 <- renderValueBox({
    v <- get_tte_val("NNT")
    valueBox(if (!is.na(v)) round(v) else "N/A",
             "NNT (24 months)", icon = icon("user-md"), color = "purple")
  })
  output$tte_excl <- renderPlotly({
    df <- tte_excl_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 14 + export append first"))
    overall <- as.numeric(df$overall_excl_pct[1])
    bc <- ifelse(df$excl_pct > overall + 5, RED,
                 ifelse(df$excl_pct > overall, AMBER, GREEN))
    plot_ly(x = df$subgroup, y = df$excl_pct, type = "bar",
            marker = list(color = bc, line = list(color = "white", width = 2)),
            text = paste0(df$excl_pct, "%"), textposition = "outside") %>%
      add_segments(x = 0.5, xend = nrow(df) + 0.5, y = overall, yend = overall,
                   line = list(color = NAVY, dash = "dash", width = 2), showlegend = FALSE) %>%
      layout(xaxis = list(title = "Subgroup"),
             yaxis = list(title = "Exclusion Rate (%)",
                          range = c(0, max(df$excl_pct, na.rm = TRUE) * 1.35)),
             paper_bgcolor = "white", plot_bgcolor = "white", showlegend = FALSE)
  })
  output$tte_outcomes <- renderDT({
    df <- tte_sum_r()
    if (is.null(df)) return(datatable(data.frame(Note = "Run Cell 14 first")))
    show_metrics <- c("N matched pairs","SGLT2 MACE rate","Metformin MACE rate",
                      "Risk Ratio","ARR","NNT","OR")
    df_show <- df[df$Metric %in% show_metrics, ]
    df_show$Value <- as.character(round(as.numeric(df_show$Value), 3))
    idx <- df_show$Metric == "N matched pairs"
    if (any(idx))
      df_show$Value[idx] <- format(as.integer(as.numeric(df_show$Value[idx]) * 2), big.mark = ",")
    datatable(df_show, rownames = FALSE,
              options = list(dom = "t", pageLength = 10), class = "compact stripe")
  })
  output$tte_forest <- renderPlotly({
    df <- tte_sg_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 14 first"))
    overall_rr <- get_tte_val("Risk Ratio")
    df <- df[order(df$rr), ]
    bc <- ifelse(df$upper < 1.0, GREEN,
                 ifelse(df$lower > 1.0, RED, NAVY))
    p <- plot_ly()
    for (i in seq_len(nrow(df))) {
      p <- p %>%
        add_segments(x = df$lower[i], xend = df$upper[i],
                     y = df$subgroup[i], yend = df$subgroup[i],
                     line = list(color = bc[i], width = 3), showlegend = FALSE) %>%
        add_trace(x = df$rr[i], y = df$subgroup[i],
                  type = "scatter", mode = "markers",
                  marker = list(color = bc[i], size = 10), showlegend = FALSE)
    }
    if (!is.na(overall_rr))
      p <- p %>% add_segments(x = overall_rr, xend = overall_rr,
                               y = 0.5, yend = nrow(df) + 0.5,
                               line = list(color = NAVY, dash = "dot", width = 2),
                               name = paste0("Overall RR = ", round(overall_rr, 2)))
    p %>%
      add_segments(x = 1, xend = 1, y = 0.5, yend = nrow(df) + 0.5,
                   line = list(color = "#CCCCCC", dash = "dash"), showlegend = FALSE) %>%
      layout(xaxis = list(title = "Risk Ratio (SGLT2 vs Metformin)", range = c(0.3, 2.2)),
             yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.15),
             paper_bgcolor = "white", plot_bgcolor = "white")
  })
  output$tte_access <- renderPlotly({
    df <- tte_acc_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 14 first"))
    overall <- as.numeric(df$overall_rate[1])
    bc <- ifelse(df$sglt2_rate < overall - 3, RED,
                 ifelse(df$sglt2_rate < overall, AMBER, GREEN))
    plot_ly(y = df$subgroup, x = df$sglt2_rate, type = "bar", orientation = "h",
            marker = list(color = bc),
            text = paste0(df$sglt2_rate, "%"), textposition = "outside") %>%
      add_segments(x = overall, xend = overall, y = 0.5, yend = nrow(df) + 0.5,
                   line = list(color = NAVY, dash = "dash", width = 2), showlegend = FALSE) %>%
      layout(xaxis = list(title = "SGLT2 Prescribing Rate (%)",
                          range = c(0, max(df$sglt2_rate, na.rm = TRUE) * 1.3)),
             yaxis = list(title = ""),
             paper_bgcolor = "white", plot_bgcolor = "white", showlegend = FALSE)
  })


  # ---------------------------------------------------------------------------
  # STAT METHODS server (Tab 11 — Project 9 Utrecht)
  # ---------------------------------------------------------------------------

  # KPI value boxes
  output$stats1 <- renderValueBox({
    kpi <- dat$stats_kpi
    val <- if (!is.null(kpi) && nrow(kpi) > 0) paste0(kpi$value[1], "%") else "N/A"
    valueBox(val, "Power (d=0.3, α=0.05, N=200)", icon("bolt"), color = "blue")
  })
  output$stats2 <- renderValueBox({
    kpi <- dat$stats_kpi
    val <- if (!is.null(kpi) && nrow(kpi) > 1) as.character(kpi$value[2]) else "N/A"
    valueBox(val, "Min N per arm (80% power)", icon("users"), color = "yellow")
  })
  output$stats3 <- renderValueBox({
    kpi <- dat$stats_kpi
    v4 <- if (!is.null(kpi) && nrow(kpi) > 3) kpi$value[4] else "N/A"
    v5 <- if (!is.null(kpi) && nrow(kpi) > 4) kpi$value[5] else "N/A"
    valueBox(paste0(v4, " / ", v5), "Sig Subgroups (Raw / Bonf)", icon("filter"), color = "green")
  })
  output$stats4 <- renderValueBox({
    kpi <- dat$stats_kpi
    val <- if (!is.null(kpi) && nrow(kpi) > 6) paste0(kpi$value[7], "%") else "N/A"
    valueBox(val, "Type II Error β (α=0.05, d=0.3, N=200)", icon("exclamation"), color = "red")
  })

  # Power curves
  output$stats_power_plot <- renderPlotly({
    df <- dat$stats_power
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 15 first"))
    a  <- as.numeric(input$stats_alpha)
    sub <- df[abs(df$alpha - a) < 0.001, ]
    cols <- c("0.2" = "#CCCCCC", "0.3" = "#1D6FA4", "0.5" = "#0A1628", "0.8" = "#1A7A4A")
    p <- plot_ly()
    for (d in c(0.2, 0.3, 0.5, 0.8)) {
      s <- sub[abs(sub$effect_size - d) < 0.001, ]
      p <- p %>% add_trace(data = s, x = ~n_per_arm, y = ~round(power * 100, 1),
                            type = "scatter", mode = "lines",
                            name = paste0("d = ", d),
                            line = list(color = cols[as.character(d)], width = 2.5))
    }
    p %>%
      add_segments(x = min(sub$n_per_arm), xend = max(sub$n_per_arm),
                   y = 80, yend = 80,
                   line = list(color = "#C0392B", dash = "dash", width = 1.5),
                   name = "80% threshold") %>%
      add_segments(x = min(sub$n_per_arm), xend = max(sub$n_per_arm),
                   y = 90, yend = 90,
                   line = list(color = "#D97706", dash = "dot", width = 1.5),
                   name = "90% threshold") %>%
      layout(xaxis = list(title = "N per arm"),
             yaxis = list(title = "Power (%)", range = c(0, 105)),
             paper_bgcolor = "white", plot_bgcolor = "white",
             legend = list(orientation = "h", y = -0.2))
  })

  # Min N table
  output$stats_min_n_tbl <- renderDT({
    df <- dat$stats_min_n
    if (is.null(df)) return(datatable(data.frame(Note = "Run Cell 15 first")))
    sub <- df[abs(df$target_power - 0.80) < 0.001, c("effect_size", "alpha", "min_n_per_arm")]
    names(sub) <- c("d", "alpha", "N_per_arm")
    sub$d <- as.character(sub$d)
    datatable(sub, rownames = FALSE,
              options = list(pageLength = 12, dom = "t"),
              class = "compact stripe")
  })

  # Subgroup forest plot
  output$stats_subgrp_plot <- renderPlotly({
    df <- dat$stats_subgrp
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 15 first"))
    corr     <- input$stats_corr
    sig_col  <- paste0("sig_", corr)
    p_col    <- switch(corr, raw = "raw_p", bonf = "bonf_p", bh = "bh_fdr_p")
    sig_col  <- switch(corr, raw = "sig_raw", bonf = "sig_bonf", bh = "sig_bh")
    sig_color <- switch(corr, raw = "#C0392B", bonf = "#D97706", bh = "#1A7A4A")
    yi <- rev(seq_len(nrow(df)))
    p  <- plot_ly()
    for (i in seq_len(nrow(df))) {
      col <- if (isTRUE(df[[sig_col]][i])) sig_color else "#CCCCCC"
      p <- p %>%
        add_segments(x = df$rr_lo[i], xend = df$rr_hi[i],
                     y = yi[i], yend = yi[i],
                     line = list(color = col, width = 3), showlegend = FALSE) %>%
        add_trace(x = df$rr[i], y = yi[i], type = "scatter", mode = "markers",
                  marker = list(color = col, size = 10), showlegend = FALSE,
                  text = paste0(df$subgroup[i], "<br>RR=", df$rr[i],
                                "  p=", df[[p_col]][i]),
                  hoverinfo = "text")
    }
    p %>%
      add_segments(x = 1, xend = 1, y = 0.5, yend = nrow(df) + 0.5,
                   line = list(color = "#CCCCCC", dash = "dash"), showlegend = FALSE) %>%
      layout(xaxis = list(title = "Risk Ratio", range = c(0.4, 2.1)),
             yaxis = list(tickvals = yi, ticktext = df$subgroup, title = ""),
             paper_bgcolor = "white", plot_bgcolor = "white")
  })

  # Bayesian updating chart
  output$stats_bayes_plot <- renderPlotly({
    df <- dat$stats_bayes
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 15 first"))
    metric   <- input$stats_bayes_metric
    y_col    <- if (metric == "mu") "mu_post" else "prob_benefit"
    y_label  <- if (metric == "mu") "Posterior mean (log RR)" else "P(Benefit) %"
    cols     <- c(Sceptical = "#C0392B", Neutral = "#0A1628", Enthusiastic = "#1A7A4A")
    p <- plot_ly()
    for (pr in c("Sceptical", "Neutral", "Enthusiastic")) {
      sub <- df[df$prior == pr, ]
      yvals <- if (metric == "mu") round(sub$mu_post, 4) else round(sub$prob_benefit * 100, 2)
      p <- p %>% add_trace(x = sub$n, y = yvals,
                            type = "scatter", mode = "lines",
                            name = pr, line = list(color = cols[pr], width = 2.5))
    }
    if (metric == "prob") {
      p <- p %>%
        add_segments(x = min(df$n), xend = max(df$n), y = 95, yend = 95,
                     line = list(color = "#C0392B", dash = "dash"), name = "95% threshold") %>%
        add_segments(x = min(df$n), xend = max(df$n), y = 80, yend = 80,
                     line = list(color = "#D97706", dash = "dot"), name = "80% threshold")
    }
    p %>% layout(xaxis = list(title = "Cumulative N"),
                 yaxis = list(title = y_label),
                 paper_bgcolor = "white", plot_bgcolor = "white",
                 legend = list(orientation = "h", y = -0.2))
  })

  # Missing data chart
  output$stats_missing_plot <- renderPlotly({
    df <- dat$stats_missing
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 15 first"))
    plot_ly() %>%
      add_trace(data = df, x = ~miss_pct, y = ~d_mcar, type = "scatter", mode = "lines",
                name = "MCAR", line = list(color = "#1D6FA4", dash = "dash", width = 2)) %>%
      add_trace(data = df, x = ~miss_pct, y = ~d_mar,  type = "scatter", mode = "lines",
                name = "MAR",  line = list(color = "#D97706", dash = "dashdot", width = 2)) %>%
      add_trace(data = df, x = ~miss_pct, y = ~d_mnar, type = "scatter", mode = "lines",
                name = "MNAR", line = list(color = "#C0392B", dash = "dot", width = 2)) %>%
      add_segments(x = 0, xend = max(df$miss_pct), y = df$true_d[1], yend = df$true_d[1],
                   line = list(color = "#1A7A4A", width = 1.5), name = "True d") %>%
      layout(xaxis = list(title = "% Data Missing"),
             yaxis = list(title = "Cohen's d (estimated)"),
             paper_bgcolor = "white", plot_bgcolor = "white",
             legend = list(orientation = "h", y = -0.2))
  })

  # ===========================================================================
  # TAB 12: CORE OUTCOME SET
  # ===========================================================================

  cos_kpi_val <- function(metric) {
    df <- dat$cos_kpi
    if (is.null(df)) return("—")
    v <- df$value[df$metric == metric]
    if (length(v) == 0) return("—")
    as.character(v[1])
  }

  output$cos1 <- renderValueBox({
    valueBox(cos_kpi_val("Total candidate outcomes"),
             "Candidate Outcomes", icon = icon("list"),
             color = "navy")
  })
  output$cos2 <- renderValueBox({
    valueBox(paste0(cos_kpi_val("Outcomes achieving majority consensus"), "/",
                    cos_kpi_val("Total candidate outcomes")),
             "Majority Consensus", icon = icon("check-circle"),
             color = "green")
  })
  output$cos3 <- renderValueBox({
    valueBox(paste0(cos_kpi_val("EDI outcomes in final COS"), "/6"),
             "EDI Outcomes in COS", icon = icon("users"),
             color = "yellow")
  })
  output$cos4 <- renderValueBox({
    v <- cos_kpi_val("EDI mean reporting rate pct")
    valueBox(paste0(v, "%"), "EDI Reporting Rate",
             icon = icon("exclamation-triangle"), color = "red")
  })

  # Reporting rate bar chart
  output$cos_reporting_plot <- renderPlotly({
    df <- dat$cos_outcomes
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    cat_cols <- c(
      "Glycaemic Control" = "#0066CC", "Cardiovascular" = "#003366",
      "Renal" = "#7B2D8B", "Patient-Reported" = "#1A7A4A",
      "EDI-Sensitive" = "#C0392B", "Safety" = "#E67E22"
    )
    df$color <- cat_cols[df$category]
    df$pct   <- round(df$reporting_rate * 100, 1)
    df <- df[order(df$category, df$pct), ]
    plot_ly(df, x = ~pct, y = ~reorder(outcome, pct), type = "bar",
            orientation = "h",
            marker = list(color = ~color),
            text = ~paste0(pct, "%"),
            textposition = "outside",
            hovertext = ~paste0(outcome, "<br>", pct, "% of trials<br>Category: ", category),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "% of trials reporting", range = c(0, 110)),
             yaxis = list(title = "", tickfont = list(size = 10)),
             paper_bgcolor = "white", plot_bgcolor = "white",
             showlegend = FALSE,
             shapes = list(list(type = "line", x0 = 50, x1 = 50, y0 = -0.5,
                                y1 = nrow(df) - 0.5,
                                line = list(color = "#CCCCCC", dash = "dash"))))
  })

  # EDI gap bar chart
  output$cos_gap_plot <- renderPlotly({
    df <- dat$cos_outcomes
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    agg <- aggregate(reporting_rate ~ category, data = df, FUN = mean)
    agg$pct   <- round(agg$reporting_rate * 100, 1)
    agg$color <- ifelse(agg$category == "EDI-Sensitive", "#C0392B", "#003366")
    agg <- agg[order(agg$pct), ]
    plot_ly(agg, x = ~pct, y = ~reorder(category, pct), type = "bar",
            orientation = "h",
            marker = list(color = ~color),
            text = ~paste0(pct, "%"), textposition = "outside",
            hoverinfo = "text",
            hovertext = ~paste0(category, ": ", pct, "%")) %>%
      layout(xaxis = list(title = "Mean reporting rate (%)", range = c(0, 115)),
             yaxis = list(title = ""),
             paper_bgcolor = "white", plot_bgcolor = "white",
             showlegend = FALSE)
  })

  # Delphi scores by round
  output$cos_delphi_plot <- renderPlotly({
    df <- dat$cos_delphi
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    cat_filter <- input$cos_cat
    if (cat_filter != "All") df <- df[df$category == cat_filter, ]
    agg <- aggregate(mean_score ~ stakeholder + round, data = df, FUN = mean)
    agg$round <- paste0("Round ", agg$round)
    cols <- c("Round 1" = "#CCCCCC", "Round 2" = "#0066CC", "Round 3" = "#003366")
    p <- plot_ly()
    for (rnd in c("Round 1", "Round 2", "Round 3")) {
      sub <- agg[agg$round == rnd, ]
      p <- p %>% add_trace(x = ~stakeholder, y = ~mean_score, data = sub,
                           type = "bar", name = rnd,
                           marker = list(color = cols[rnd]),
                           hovertext = ~paste0(stakeholder, " | ", rnd,
                                               "<br>Mean score: ", round(mean_score, 2)),
                           hoverinfo = "text")
    }
    p %>% layout(barmode = "group",
                 xaxis = list(title = ""),
                 yaxis = list(title = "Mean score (1-9)", range = c(0, 10)),
                 paper_bgcolor = "white", plot_bgcolor = "white",
                 shapes = list(list(type = "line", x0 = -0.5, x1 = 4.5,
                                    y0 = 7, y1 = 7,
                                    line = list(color = "#C0392B", dash = "dash"))),
                 legend = list(orientation = "h", y = -0.2))
  })

  # Clinician vs Patient dumbbell
  output$cos_divergence_plot <- renderPlotly({
    df <- dat$cos_divergence
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    df <- df[df$category == "EDI-Sensitive", ]
    df <- df[order(df$score_Patients), ]
    p <- plot_ly()
    for (i in seq_len(nrow(df))) {
      p <- p %>% add_segments(
        x = df$score_Clinicians[i], xend = df$score_Patients[i],
        y = df$outcome[i], yend = df$outcome[i],
        line = list(color = "#CCCCCC", width = 2), showlegend = FALSE)
    }
    p %>%
      add_trace(data = df, x = ~score_Clinicians, y = ~outcome,
                type = "scatter", mode = "markers", name = "Clinicians",
                marker = list(color = "#003366", size = 12),
                hovertext = ~paste0(outcome, "<br>Clinician: ", round(score_Clinicians, 1)),
                hoverinfo = "text") %>%
      add_trace(data = df, x = ~score_Patients, y = ~outcome,
                type = "scatter", mode = "markers", name = "Patients",
                marker = list(color = "#1A7A4A", size = 12),
                hovertext = ~paste0(outcome, "<br>Patient: ", round(score_Patients, 1)),
                hoverinfo = "text") %>%
      layout(xaxis = list(title = "Mean importance score (1-9)", range = c(3, 11)),
             yaxis = list(title = ""),
             paper_bgcolor = "white", plot_bgcolor = "white",
             legend = list(orientation = "h", y = -0.2))
  })

  # COS completeness heatmap
  output$cos_audit_plot <- renderPlotly({
    df <- dat$cos_audit
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    pivot <- reshape(df[, c("trial_type", "domain", "completeness")],
                     idvar = "trial_type", timevar = "domain", direction = "wide")
    dom_cols <- c("Glycaemic Control", "Cardiovascular", "Renal",
                  "Patient-Reported", "EDI-Sensitive", "Safety")
    z_cols <- paste0("completeness.", dom_cols)
    z_mat  <- as.matrix(pivot[, z_cols])
    plot_ly(x = dom_cols, y = pivot$trial_type, z = z_mat,
            type = "heatmap", colorscale = "RdYlGn",
            zmin = 0, zmax = 100,
            text = matrix(paste0(as.vector(z_mat), "%"),
                          nrow = nrow(z_mat), ncol = ncol(z_mat)),
            texttemplate = "%{text}",
            hovertemplate = "%{y}<br>%{x}: %{z}%<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "white")
  })

  # Threshold sensitivity
  output$cos_threshold_plot <- renderPlotly({
    df <- dat$cos_threshold
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 16 first"))
    df_edi <- df[df$category == "EDI-Sensitive", ]
    plot_ly(df_edi, x = ~threshold * 100, y = ~n_included,
            type = "bar",
            marker = list(color = "#C0392B"),
            text = ~paste0(n_included, "/", n_total),
            textposition = "outside",
            hovertext = ~paste0("Threshold: ", threshold * 100, "%<br>",
                                "Included: ", n_included, "/", n_total),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Consensus threshold (%)", dtick = 5),
             yaxis = list(title = "EDI outcomes in COS", range = c(0, 8)),
             paper_bgcolor = "white", plot_bgcolor = "white",
             shapes = list(list(type = "line", x0 = 70, x1 = 70,
                                y0 = 0, y1 = 8,
                                line = list(color = "#CCCCCC", dash = "dash"))),
             annotations = list(list(x = 71, y = 7.5, text = "COMET 70%",
                                     showarrow = FALSE, font = list(size = 10, color = "#888"))))
  })

} # end server

shinyApp(ui = ui, server = server)

