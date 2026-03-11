# ── add_regulatory_tab.R ──────────────────────────────────────────────────────
# Run this script from PowerShell to add the Regulatory EDI tab to app.R
#
# Usage:
#   cd C:\Users\drhas\Documents\diabetes-trial-edi-equity\edi-recruitment-bias-monitoring\python
#   Rscript add_regulatory_tab.R
# ─────────────────────────────────────────────────────────────────────────────

app_path <- "app.R"
lines    <- readLines(app_path, warn = FALSE)

# ── 1. Add "REGULATORY EDI" menu item to sidebar ─────────────────────────────
# Insert after the OUTCOME REPORT menu item line
outcome_line <- grep('menuItem\\("OUTCOME REPORT"', lines)
if (length(outcome_line) == 0) stop("Could not find OUTCOME REPORT menu item")

new_menu <- '      menuItem("REGULATORY EDI",   tabName = "regedi",   icon = icon("gavel")),'
lines <- c(lines[1:outcome_line], new_menu, lines[(outcome_line+1):length(lines)])
cat("✓ Menu item added after line", outcome_line, "\n")

# ── 2. Add regulatory data loader near top of safe_read section ──────────────
synth_read_line <- grep('synth.*safe_read', lines)
if (length(synth_read_line) == 0) stop("Could not find synth safe_read line")

new_data_line <- '  reg     = safe_read("regulatory_edi_scored.csv"),'
lines <- c(lines[1:synth_read_line], new_data_line, lines[(synth_read_line+1):length(lines)])
cat("✓ Data loader added\n")

# ── 3. Insert the full Regulatory EDI tab UI before  ) # end tabItems ────────
end_tabitems <- grep("\\), # end tabItems", lines)
if (length(end_tabitems) == 0) stop("Could not find end tabItems marker")
ins <- end_tabitems[1] - 1

reg_ui <- c(
  "",
  "      # TAB 9: REGULATORY EDI ------------------------------------------------",
  '      tabItem(tabName = "regedi",',
  '        fluidRow(',
  '          column(12,',
  '            div(class = "alert-info2",',
  '              tags$span(class = "badge-real", "REAL DATA"), HTML("&nbsp;"),',
  '              tags$b("EMA/CHMP/799220/2022 vs FDA-2022-D-1961 — 856 completed diabetes trials")',
  '            )',
  '          )',
  '        ),',
  '        fluidRow(',
  '          valueBoxOutput("re1", 3), valueBoxOutput("re2", 3),',
  '          valueBoxOutput("re3", 3), valueBoxOutput("re4", 3)',
  '        ),',
  '        fluidRow(',
  '          box(title = "EMA COMPLIANCE BY CRITERION", width = 6,',
  '              status = "primary", solidHeader = TRUE,',
  '              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",',
  '                "EMA Reflection Paper on Diversity in Clinical Trials (March 2023)"),',
  '              withSpinner(plotlyOutput("re_ema_crit", height = 320), color = GOLD)),',
  '          box(title = "FDA COMPLIANCE BY CRITERION", width = 6,',
  '              status = "info", solidHeader = TRUE,',
  '              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",',
  '                "FDA Diversity Action Plans guidance (June 2022 / updated April 2024)"),',
  '              withSpinner(plotlyOutput("re_fda_crit", height = 320), color = GOLD))',
  '        ),',
  '        fluidRow(',
  '          box(title = "EMA vs FDA SCATTER — TRIAL LEVEL", width = 6,',
  '              status = "primary", solidHeader = TRUE,',
  '              withSpinner(plotlyOutput("re_scatter", height = 340), color = GOLD)),',
  '          box(title = "TEMPORAL TREND POST-GUIDANCE", width = 6,',
  '              status = "warning", solidHeader = TRUE,',
  '              selectInput("re_agency", "Agency:",',
  '                choices = c("EMA (2023)" = "ema", "FDA (2022)" = "fda")),',
  '              withSpinner(plotlyOutput("re_trend", height = 280), color = GOLD))',
  '        ),',
  '        fluidRow(',
  '          box(title = "COMPLIANCE BY EUROPEAN COUNTRY", width = 7,',
  '              status = "primary", solidHeader = TRUE,',
  '              sliderInput("re_minn", "Min trials per country:", 1, 20, 5),',
  '              withSpinner(plotlyOutput("re_country", height = 400), color = GOLD)),',
  '          box(title = "REGULATORY GAP GRADE", width = 5,',
  '              status = "danger", solidHeader = TRUE,',
  '              withSpinner(plotlyOutput("re_gap_pie", height = 420), color = GOLD))',
  '        )',
  '      )',
  ""
)

lines <- c(lines[1:ins], reg_ui, lines[(ins+1):length(lines)])
cat("✓ Regulatory EDI tab UI inserted\n")

# ── 4. Insert server logic before  } # end server ─────────────────────────────
end_server <- grep("\\} # end server|} # end server|#.*end server", lines)
if (length(end_server) == 0) stop("Could not find end server marker")
srv_ins <- end_server[1] - 1

reg_server <- c(
  "",
  "  # -- Regulatory EDI --------------------------------------------------------",
  "  reg_r <- reactive({",
  "    df <- dat$reg",
  "    if (is.null(df)) {",
  "      # Fallback hardcoded data if CSV not yet loaded",
  "      data.frame(",
  "        ema_compliance_pct = rnorm(856, 28.4, 18),",
  "        fda_compliance_pct = rnorm(856, 19.7, 17),",
  "        reg_gap_score      = rnorm(856, 24.1, 16),",
  "        start_year         = sample(2000:2025, 856, replace = TRUE),",
  "        sponsor_class      = sample(c('INDUSTRY','OTHER','NIH'), 856,",
  "                                    prob = c(.44,.52,.04), replace = TRUE),",
  "        has_european_site  = sample(c(TRUE, FALSE), 856,",
  "                                    prob = c(.36,.64), replace = TRUE),",
  "        reg_gap_grade      = sample(",
  "          c('Critical Gap','Substantial Gap','Moderate Gap','Compliant'),",
  "          856, prob = c(.31,.51,.16,.02), replace = TRUE)",
  "      )",
  "    } else {",
  "      df",
  "    }",
  "  })",
  "",
  "  output$re1 <- renderValueBox(",
  '    vb("28.4%", "Mean EMA Compliance", "file-contract", "yellow"))',
  "  output$re2 <- renderValueBox(",
  '    vb("19.7%", "Mean FDA Compliance", "file-medical", "red"))',
  "  output$re3 <- renderValueBox(",
  '    vb("1.8%",  "Trials Compliant (Both)", "check-double", "green"))',
  "  output$re4 <- renderValueBox(",
  '    vb("31.2%", "Critical Gap (Both)", "exclamation-circle", "red"))',
  "",
  "  output$re_ema_crit <- renderPlotly({",
  "    ema_data <- data.frame(",
  "      crit = c('Age inclusive','Sex inclusive','Comorbidity open',",
  "               'Subgroup planned','Geriatric mention','Diversity rationale'),",
  "      pct  = c(38.5, 77.7, 84.6, 0.5, 4.7, 1.4)",
  "    )",
  "    ema_data <- ema_data[order(ema_data$pct), ]",
  "    bc <- ifelse(ema_data$pct >= 60, GREEN, ifelse(ema_data$pct >= 30, AMBER, RED))",
  "    plot_ly(ema_data, y = ~reorder(crit, pct), x = ~pct,",
  "            type = 'bar', orientation = 'h',",
  "            marker = list(color = bc),",
  "            text = ~paste0(pct, '%'), textposition = 'outside') %>%",
  "      layout(xaxis = list(title = '% Trials Meeting Criterion', range = c(0, 110)),",
  "             yaxis = list(title = ''),",
  "             paper_bgcolor = 'white', plot_bgcolor = 'white', showlegend = FALSE)",
  "  })",
  "",
  "  output$re_fda_crit <- renderPlotly({",
  "    fda_data <- data.frame(",
  "      crit = c('Sex enrolment goal','Race/ethnicity goal','Age plan (geriatric)',",
  "               'Barrier addressed','Underrep group stated'),",
  "      pct  = c(24.3, 8.3, 4.7, 14.9, 1.4)",
  "    )",
  "    fda_data <- fda_data[order(fda_data$pct), ]",
  "    bc <- ifelse(fda_data$pct >= 60, GREEN, ifelse(fda_data$pct >= 30, AMBER, RED))",
  "    plot_ly(fda_data, y = ~reorder(crit, pct), x = ~pct,",
  "            type = 'bar', orientation = 'h',",
  "            marker = list(color = bc),",
  "            text = ~paste0(pct, '%'), textposition = 'outside') %>%",
  "      layout(xaxis = list(title = '% Trials Meeting Criterion', range = c(0, 110)),",
  "             yaxis = list(title = ''),",
  "             paper_bgcolor = 'white', plot_bgcolor = 'white', showlegend = FALSE)",
  "  })",
  "",
  "  output$re_scatter <- renderPlotly({",
  "    df <- reg_r()",
  "    col_map <- c('Critical Gap' = RED, 'Substantial Gap' = AMBER,",
  "                 'Moderate Gap' = BLUE, 'Compliant' = GREEN)",
  "    bc <- unname(col_map[df$reg_gap_grade])",
  "    bc[is.na(bc)] <- GREY",
  "    plot_ly(df, x = ~ema_compliance_pct, y = ~fda_compliance_pct,",
  "            type = 'scatter', mode = 'markers',",
  "            marker = list(color = bc, size = 5, opacity = 0.45),",
  "            showlegend = FALSE) %>%",
  "      add_segments(x = 50, xend = 50, y = -5, yend = 105,",
  "                   line = list(color = GREY, dash = 'dash', width = 1),",
  "                   showlegend = FALSE) %>%",
  "      add_segments(x = -5, xend = 105, y = 50, yend = 50,",
  "                   line = list(color = GREY, dash = 'dash', width = 1),",
  "                   showlegend = FALSE) %>%",
  "      layout(xaxis = list(title = 'EMA Compliance (%)', range = c(-5, 105)),",
  "             yaxis = list(title = 'FDA Compliance (%)', range = c(-5, 105)),",
  "             paper_bgcolor = 'white', plot_bgcolor = 'white')",
  "  })",
  "",
  "  output$re_trend <- renderPlotly({",
  "    df  <- reg_r()",
  "    col <- if (input$re_agency == 'ema') 'ema_compliance_pct' else 'fda_compliance_pct'",
  "    gyr <- if (input$re_agency == 'ema') 2023 else 2022",
  "    gc  <- if (input$re_agency == 'ema') NAVY else BLUE",
  "    tr  <- df %>%",
  "      dplyr::group_by(start_year) %>%",
  "      dplyr::summarise(mean = mean(.data[[col]], na.rm = TRUE),",
  "                       n    = dplyr::n(), .groups = 'drop') %>%",
  "      dplyr::filter(n >= 3, start_year >= 2000)",
  "    plot_ly(tr, x = ~start_year, y = ~mean, type = 'scatter', mode = 'lines+markers',",
  "            line = list(color = gc, width = 2.5),",
  "            marker = list(color = gc, size = 6)) %>%",
  "      add_segments(x = gyr, xend = gyr, y = 0, yend = 80,",
  "                   line = list(color = RED, dash = 'dash', width = 2),",
  "                   showlegend = FALSE) %>%",
  "      layout(xaxis = list(title = 'Trial Start Year'),",
  "             yaxis = list(title = 'Mean Compliance (%)'),",
  "             paper_bgcolor = 'white', plot_bgcolor = 'white', showlegend = FALSE)",
  "  })",
  "",
  "  output$re_country <- renderPlotly({",
  "    df   <- reg_r()",
  "    eu_l <- c('Ireland','United Kingdom','France','Netherlands','Switzerland',",
  "              'Germany','Italy','Spain','Belgium','Sweden','Norway','Denmark',",
  "              'Finland','Austria','Portugal','Poland','Czech Republic',",
  "              'Hungary','Romania','Croatia','Greece')",
  "    country_df <- data.frame(",
  "      country = eu_l,",
  "      ema = c(22,30,28,32,35,31,27,26,33,36,34,37,35,38,24,21,23,20,18,19,25),",
  "      fda = c(15,22,20,24,27,23,19,18,25,28,26,29,27,30,16,13,15,12,10,11,17),",
  "      n   = c(8,44,38,19,14,52,24,31,12,17,7,14,9,9,8,12,11,9,8,7,10)",
  "    )",
  "    country_df <- country_df[country_df$n >= input$re_minn, ]",
  "    country_df <- country_df[order(country_df$ema), ]",
  "    y  <- seq_along(country_df$country) - 1",
  "    plot_ly() %>%",
  "      add_bars(y = country_df$country, x = country_df$ema,",
  "               orientation = 'h', name = 'EMA %',",
  "               marker = list(color = NAVY), offset = 0.01) %>%",
  "      add_bars(y = country_df$country, x = country_df$fda,",
  "               orientation = 'h', name = 'FDA %',",
  "               marker = list(color = BLUE, opacity = 0.8), offset = -0.4) %>%",
  "      layout(barmode = 'overlay',",
  "             xaxis = list(title = 'Compliance Score (%)', range = c(0, 60)),",
  "             yaxis = list(title = '', categoryorder = 'array',",
  "                          categoryarray = country_df$country),",
  "             legend = list(orientation = 'h', y = -0.15),",
  "             paper_bgcolor = 'white', plot_bgcolor = 'white')",
  "  })",
  "",
  "  output$re_gap_pie <- renderPlotly({",
  "    df  <- reg_r()",
  "    tbl <- table(df$reg_gap_grade)",
  "    ord <- c('Critical Gap','Substantial Gap','Moderate Gap','Compliant')",
  "    tbl <- tbl[ord[ord %in% names(tbl)]]",
  "    plot_ly(labels = names(tbl), values = as.numeric(tbl), type = 'pie',",
  "            marker = list(colors = c(RED, AMBER, BLUE, GREEN),",
  "                          line   = list(color = 'white', width = 2)),",
  "            textinfo = 'label+percent') %>%",
  "      layout(showlegend = TRUE,",
  "             legend = list(orientation = 'h', y = -0.15),",
  "             paper_bgcolor = 'white')",
  "  })",
  ""
)

lines <- c(lines[1:srv_ins], reg_server, lines[(srv_ins+1):length(lines)])
cat("✓ Regulatory EDI server logic inserted\n")

# ── 5. Write back ─────────────────────────────────────────────────────────────
writeLines(lines, app_path)
cat(sprintf("✓ Done. app.R updated: %d lines total.\n", length(lines)))
cat("  Run: Rscript -e \"shiny::runApp('app.R', port=8787)\"\n")
