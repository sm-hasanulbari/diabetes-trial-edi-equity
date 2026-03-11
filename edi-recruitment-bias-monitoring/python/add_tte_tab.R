# add_tte_tab.R  (v2 — zero hardcoded values)
# Run once: Rscript add_tte_tab.R

app_file <- "app.R"
lines    <- readLines(app_file)

# ── 1. Add menu item ----------------------------------------------------------
menu_marker <- grep('menuItem\\("REGULATORY EDI"', lines)
if (length(menu_marker) == 0) stop("Could not find REGULATORY EDI menu item")
new_menu <- '      menuItem("TARGET TRIAL",     tabName = "tte",     icon = icon("random"))'
lines <- c(lines[1:(menu_marker-1)],
           sub('(menuItem\\("REGULATORY EDI".*)', '\\1,', lines[menu_marker]),
           new_menu,
           lines[(menu_marker+1):length(lines)])
cat("✓ Menu item added\n")

# ── 2. Add data loaders -------------------------------------------------------
old_dat <- grep('reg    = load_csv', lines)[1]
lines[old_dat] <- sub('(reg    = load_csv.*)', '\\1,', lines[old_dat])
insert_dat <- paste0(
  '  tte_sum  = load_csv("tte_summary.csv"),\n',
  '  tte_sg   = load_csv("tte_subgroup_results.csv"),\n',
  '  tte_excl = load_csv("tte_edi_exclusion.csv"),\n',
  '  tte_acc  = load_csv("tte_edi_access.csv")')
lines <- c(lines[1:old_dat], insert_dat, lines[(old_dat+1):length(lines)])
cat("✓ Data loaders added\n")

# ── 3. Add UI tab -------------------------------------------------------------
end_tabs <- grep("\\), # end tabItems", lines)[1]

tte_ui <- '
      # TAB 10: TARGET TRIAL EMULATION ----------------------------------------
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
              withSpinner(plotlyOutput("tte_excl", height = 340), color = "#C9A84C")),
          box(title = "OUTCOME SUMMARY", width = 5,
              status = "danger", solidHeader = TRUE,
              withSpinner(DTOutput("tte_outcomes"), color = "#C9A84C"))
        ),
        fluidRow(
          box(title = "FOREST PLOT — SUBGROUP HETEROGENEITY", width = 8,
              status = "primary", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "Risk Ratio for 24-month MACE by subgroup. EDI subgroups highlighted."),
              withSpinner(plotlyOutput("tte_forest", height = 420), color = "#C9A84C")),
          box(title = "EDI TREATMENT ACCESS", width = 4,
              status = "warning", solidHeader = TRUE,
              tags$p(style = "font-size:11px;color:#718096;margin-bottom:6px;",
                "SGLT2 prescribing rate by equity subgroup — confounding by SES and rurality."),
              withSpinner(plotlyOutput("tte_access", height = 420), color = "#C9A84C"))
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
'

lines <- c(lines[1:(end_tabs-1)], tte_ui, lines[end_tabs:length(lines)])
cat("✓ TTE UI tab inserted\n")

# ── 4. Add server logic -------------------------------------------------------
end_server <- grep("^} # end server", lines)[1]

tte_server <- '
  # ── Target Trial Emulation --------------------------------------------------
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
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 14 + append block first"))
    overall <- as.numeric(df$overall_excl_pct[1])
    bc <- ifelse(df$excl_pct > overall + 5, "#C0392B",
                 ifelse(df$excl_pct > overall, "#D97706", "#1A7A4A"))
    plot_ly(x = df$subgroup, y = df$excl_pct, type = "bar",
            marker = list(color = bc, line = list(color = "white", width = 2)),
            text = paste0(df$excl_pct, "%"), textposition = "outside") %>%
      add_segments(x = 0.5, xend = nrow(df) + 0.5,
                   y = overall, yend = overall,
                   line = list(color = "#003366", dash = "dash", width = 2),
                   showlegend = FALSE) %>%
      layout(xaxis = list(title = "Subgroup"),
             yaxis = list(title = "Exclusion Rate (%)",
                          range = c(0, max(df$excl_pct, na.rm = TRUE) * 1.3)),
             paper_bgcolor = "white", plot_bgcolor = "white", showlegend = FALSE,
             annotations = list(list(
               x = 0, y = overall + 1.5, xref = "x", yref = "y", showarrow = FALSE,
               text = paste0("Overall: ", overall, "%"),
               font = list(size = 9, color = "#003366"))))
  })

  output$tte_outcomes <- renderDT({
    df <- tte_sum_r()
    if (is.null(df)) return(datatable(data.frame(Note = "Run Cell 14 first")))
    show_metrics <- c("N matched pairs", "SGLT2 MACE rate", "Metformin MACE rate",
                      "Risk Ratio", "ARR", "NNT", "OR")
    df_show <- df[df$Metric %in% show_metrics, ]
    df_show$Value <- as.character(round(as.numeric(df_show$Value), 3))
    idx_pairs <- df_show$Metric == "N matched pairs"
    if (any(idx_pairs))
      df_show$Value[idx_pairs] <- format(
        as.integer(as.numeric(df_show$Value[idx_pairs]) * 2), big.mark = ",")
    datatable(df_show, rownames = FALSE,
              options = list(dom = "t", pageLength = 10), class = "compact stripe")
  })

  output$tte_forest <- renderPlotly({
    df <- tte_sg_r()
    if (is.null(df)) return(plot_ly() %>% layout(title = "Run Cell 14 first"))
    overall_rr <- get_tte_val("Risk Ratio")
    df <- df[order(df$rr), ]
    bc <- ifelse(df$upper < 1.0, "#1A7A4A",
                 ifelse(df$lower > 1.0, "#C0392B", "#003366"))
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
                               line = list(color = "#003366", dash = "dot", width = 2),
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
    bc <- ifelse(df$sglt2_rate < overall - 3, "#C0392B",
                 ifelse(df$sglt2_rate < overall, "#D97706", "#1A7A4A"))
    plot_ly(y = df$subgroup, x = df$sglt2_rate, type = "bar", orientation = "h",
            marker = list(color = bc),
            text = paste0(df$sglt2_rate, "%"), textposition = "outside") %>%
      add_segments(x = overall, xend = overall, y = 0.5, yend = nrow(df) + 0.5,
                   line = list(color = "#003366", dash = "dash", width = 2),
                   showlegend = FALSE) %>%
      layout(xaxis = list(title = "SGLT2 Prescribing Rate (%)",
                          range = c(0, max(df$sglt2_rate, na.rm = TRUE) * 1.3)),
             yaxis = list(title = ""),
             paper_bgcolor = "white", plot_bgcolor = "white", showlegend = FALSE)
  })
'

lines <- c(lines[1:(end_server - 1)], tte_server, lines[end_server:length(lines)])
cat("✓ TTE server logic inserted\n")

# ── 5. Write file -------------------------------------------------------------
writeLines(lines, app_file)
cat(sprintf("✓ Done. app.R updated: %d lines total.\n", length(lines)))
cat('  Run: Rscript -e "shiny::runApp(\'app.R\', port=8787)"\n')
