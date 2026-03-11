# ── Add STAT METHODS Tab to app.R ────────────────────────────────────────────
# Inserts tab 11 with power curves, subgroup analysis, Bayesian updating
# All data read reactively from CSVs — zero hardcoded values

f <- "app.R"
lines <- readLines(f, warn = FALSE)

# ── Find insertion points ──────────────────────────────────────────────────────
menu_idx <- grep('menuItem\\("TARGET TRIAL"', lines)[1]
if (is.na(menu_idx)) menu_idx <- grep('menuItem\\("OUTCOME REPORTING"', lines)[1]

data_idx <- grep('dat <- list\\(', lines)[1] + 1

ui_idx <- grep('tabItem\\(tabName="tte"', lines)[1]
if (is.na(ui_idx)) ui_idx <- grep('tabItem\\(tabName="outcomes"', lines)[1]

server_idx <- grep('# ── TTE server', lines)[1]
if (is.na(server_idx)) server_idx <- grep('# ── Outcomes server', lines)[1]

# ── Menu item ──────────────────────────────────────────────────────────────────
menu_line <- '      menuItem("STAT METHODS",       tabName="stats",    icon=icon("calculator")),'

# ── Data loaders ───────────────────────────────────────────────────────────────
data_lines <- c(
'    stats_power    = safe_read("stats_power_curves.csv"),',
'    stats_min_n    = safe_read("stats_min_n.csv"),',
'    stats_subgrp   = safe_read("stats_subgroup.csv"),',
'    stats_errors   = safe_read("stats_error_tradeoff.csv"),',
'    stats_bayes    = safe_read("stats_bayes.csv"),',
'    stats_freq     = safe_read("stats_freq_sequential.csv"),',
'    stats_miss     = safe_read("stats_missing_data.csv"),',
'    stats_kpi      = safe_read("stats_kpi.csv"),'
)

# ── UI tab ─────────────────────────────────────────────────────────────────────
ui_lines <- c(
'',
'    # ══ TAB 11: STAT METHODS ═════════════════════════════════════════════════',
'    tabItem(tabName="stats",',
'      fluidRow(div(class="col-sm-12",div(class="alert-info2",',
'        tags$b("Advanced Statistical Methods for Clinical Trial Design"),',
'        tags$p(style="margin:5px 0 0;font-size:11px;color:#4A5568;",',
'          "Power simulation, subgroup analysis with multiplicity correction, Bayesian vs frequentist comparison. ",',
'          "All values computed from simulation data — Project 9 (Utrecht).")',
'      ))),',
'      fluidRow(',
'        valueBoxOutput("stats1",3),valueBoxOutput("stats2",3),',
'        valueBoxOutput("stats3",3),valueBoxOutput("stats4",3)',
'      ),',
'      fluidRow(',
'        box(title="POWER CURVES BY EFFECT SIZE",width=8,status="primary",solidHeader=TRUE,',
'          selectInput("stats_alpha","Significance level:",',
'            c("0.01"=0.01,"0.05"=0.05,"0.10"=0.10),selected=0.05,width="160px"),',
'          withSpinner(plotlyOutput("stats_power",height=340),color=GOLD)),',
'        box(title="MINIMUM N (80% POWER)",width=4,status="info",solidHeader=TRUE,',
'          withSpinner(DTOutput("stats_min_n_tbl"),color=GOLD))',
'      ),',
'      fluidRow(',
'        box(title="SUBGROUP ANALYSIS — MULTIPLICITY CORRECTION",width=12,status="primary",solidHeader=TRUE,',
'          selectInput("stats_correction","Correction method:",',
'            c("Uncorrected"="raw","Bonferroni"="bonf","BH-FDR"="bh"),width="180px"),',
'          withSpinner(plotlyOutput("stats_subgrp",height=400),color=GOLD))',
'      ),',
'      fluidRow(',
'        box(title="BAYESIAN SEQUENTIAL UPDATING",width=6,status="primary",solidHeader=TRUE,',
'          withSpinner(plotlyOutput("stats_bayes",height=320),color=GOLD)),',
'        box(title="MISSING DATA BIAS (COMPLETE CASE)",width=6,status="info",solidHeader=TRUE,',
'          withSpinner(plotlyOutput("stats_miss",height=320),color=GOLD))',
'      )',
'    ),'
)

# ── Server logic ───────────────────────────────────────────────────────────────
server_lines <- c(
'',
'  # ── STAT METHODS server ────────────────────────────────────────────────────',
'  stats_kpi <- reactive({',
'    df <- dat$stats_kpi',
'    if (is.null(df) || nrow(df)==0) return(NULL)',
'    df',
'  })',
'',
'  output$stats1 <- renderValueBox({',
'    kpi <- stats_kpi()',
'    val <- if (!is.null(kpi)) paste0(kpi[kpi$metric=="Power at d=0.3 alpha=0.05 N=200","value"],"%") else "—"',
'    valueBox(val,"Power (d=0.3, α=0.05, N=200)","calculator","blue")',
'  })',
'  output$stats2 <- renderValueBox({',
'    kpi <- stats_kpi()',
'    val <- if (!is.null(kpi)) kpi[kpi$metric=="Min N 80pct power d=0.3 alpha=0.05","value"] else "—"',
'    valueBox(val,"Min N/arm (80% power)","users","yellow")',
'  })',
'  output$stats3 <- renderValueBox({',
'    kpi <- stats_kpi()',
'    val <- if (!is.null(kpi)) {',
'      u <- kpi[grepl("Subgroups sig uncorrected",kpi$metric),"value"]',
'      b <- kpi[grepl("Subgroups sig Bonf",kpi$metric),"value"]',
'      paste0(u,"/",b)',
'    } else "—"',
'    valueBox(val,"Subgroups Sig (Raw/Bonf)","filter","green")',
'  })',
'  output$stats4 <- renderValueBox({',
'    kpi <- stats_kpi()',
'    val <- if (!is.null(kpi)) paste0(kpi[grepl("Type2 error",kpi$metric),"value"],"%") else "—"',
'    valueBox(val,"Type II Error (β)","exclamation-triangle","purple")',
'  })',
'',
'  output$stats_power <- renderPlotly({',
'    df <- dat$stats_power',
'    if (is.null(df)) return(NULL)',
'    a <- as.numeric(input$stats_alpha)',
'    sub <- df[df$alpha==a,]',
'    p <- plot_ly()',
'    colors <- list("0.2"="#CCCCCC","0.3"="#0066CC","0.5"="#003366","0.8"="#1A7A4A")',
'    for (d in c(0.2,0.3,0.5,0.8)) {',
'      s <- sub[sub$effect_size==d,]',
'      p <- add_trace(p,data=s,x=~n_per_arm,y=~power*100,type="scatter",mode="lines",',
'        name=paste0("d=",d),line=list(color=colors[[as.character(d)]],width=2.5))',
'    }',
'    p <- add_trace(p,x=c(50,1000),y=c(80,80),type="scatter",mode="lines",',
'      name="80% threshold",line=list(color="#C0392B",dash="dash",width=1.5))',
'    p <- layout(p,xaxis=list(title="N per arm"),yaxis=list(title="Power (%)"),',
'      paper_bgcolor="white",plot_bgcolor="white",showlegend=TRUE)',
'  })',
'',
'  output$stats_min_n_tbl <- renderDT({',
'    df <- dat$stats_min_n',
'    if (is.null(df)) return(NULL)',
'    sub <- df[df$target_power==0.80,]',
'    out <- sub[,c("effect_size","alpha","min_n_per_arm","total_n")]',
'    names(out) <- c("Effect Size","Alpha","N/arm","Total N")',
'    datatable(out,rownames=FALSE,options=list(pageLength=8,dom="t"),class="compact")',
'  })',
'',
'  output$stats_subgrp <- renderPlotly({',
'    df <- dat$stats_subgrp',
'    if (is.null(df)) return(NULL)',
'    corr <- input$stats_correction',
'    sig_col <- paste0("sig_",corr)',
'    p_col   <- paste0(corr,"_p")',
'    if (corr=="raw") p_col <- "raw_p"',
'    colors_map <- list(raw="#C0392B",bonf="#E67E22",bh="#1A7A4A")',
'    sig_color <- colors_map[[corr]]',
'    y <- rev(seq_len(nrow(df)))',
'    p <- plot_ly()',
'    for (i in seq_len(nrow(df))) {',
'      row <- df[i,]',
'      yi  <- y[i]',
'      is_sig <- row[[sig_col]]',
'      col <- if (is_sig) sig_color else "#CCCCCC"',
'      p <- add_segments(p,x=row$rr_lo,xend=row$rr_hi,y=yi,yend=yi,',
'        line=list(color=col,width=2.5),showlegend=FALSE)',
'      p <- add_trace(p,x=row$rr,y=yi,type="scatter",mode="markers",',
'        marker=list(color=col,size=10),showlegend=FALSE)',
'    }',
'    p <- add_trace(p,x=c(1,1),y=c(0.5,nrow(df)+0.5),type="scatter",mode="lines",',
'      line=list(color="#CCCCCC",dash="dash",width=1.5),showlegend=FALSE)',
'    p <- layout(p,xaxis=list(title="Risk Ratio"),',
'      yaxis=list(tickvals=y,ticktext=df$subgroup,title=""),',
'      paper_bgcolor="white",plot_bgcolor="white")',
'  })',
'',
'  output$stats_bayes <- renderPlotly({',
'    df <- dat$stats_bayes',
'    if (is.null(df)) return(NULL)',
'    p <- plot_ly()',
'    colors <- list(Sceptical="#C0392B",Neutral="#003366",Enthusiastic="#1A7A4A")',
'    for (pr in c("Sceptical","Neutral","Enthusiastic")) {',
'      sub <- df[df$prior==pr,]',
'      p <- add_trace(p,data=sub,x=~n,y=~prob_benefit*100,type="scatter",mode="lines",',
'        name=pr,line=list(color=colors[[pr]],width=2.5))',
'    }',
'    p <- add_trace(p,x=c(10,300),y=c(95,95),type="scatter",mode="lines",',
'      name="95% threshold",line=list(color="#C0392B",dash="dash",width=1.5))',
'    p <- layout(p,xaxis=list(title="N (cumulative)"),yaxis=list(title="P(Benefit) %"),',
'      paper_bgcolor="white",plot_bgcolor="white",showlegend=TRUE)',
'  })',
'',
'  output$stats_miss <- renderPlotly({',
'    df <- dat$stats_miss',
'    if (is.null(df)) return(NULL)',
'    p <- plot_ly()',
'    p <- add_trace(p,data=df,x=~miss_pct,y=~d_mcar,type="scatter",mode="lines",',
'      name="MCAR",line=list(color="#0066CC",dash="dash",width=2))',
'    p <- add_trace(p,data=df,x=~miss_pct,y=~d_mar,type="scatter",mode="lines",',
'      name="MAR",line=list(color="#E67E22",dash="dot",width=2))',
'    p <- add_trace(p,data=df,x=~miss_pct,y=~d_mnar,type="scatter",mode="lines",',
'      name="MNAR",line=list(color="#C0392B",dash="dashdot",width=2))',
'    p <- add_trace(p,x=c(0,50),y=c(0.30,0.30),type="scatter",mode="lines",',
'      name="True d",line=list(color="#1A7A4A",width=1))',
'    p <- layout(p,xaxis=list(title="% Data Missing"),yaxis=list(title="Estimated Cohen\'s d"),',
'      paper_bgcolor="white",plot_bgcolor="white",showlegend=TRUE)',
'  })',
''
)

# ── Insert lines ───────────────────────────────────────────────────────────────
lines <- c(
  lines[1:(menu_idx-1)],
  menu_line,
  lines[menu_idx:(data_idx-1)],
  data_lines,
  lines[data_idx:(ui_idx-1)],
  ui_lines,
  lines[ui_idx:(server_idx-1)],
  server_lines,
  lines[server_idx:length(lines)]
)

writeLines(lines, f)

cat("✓ Menu item added\n")
cat("✓ Data loaders added\n")
cat("✓ STAT METHODS UI tab inserted\n")
cat("✓ Server logic inserted\n")
cat("✓ Done. app.R updated:", length(lines), "lines total.\n")
cat("Run: Rscript -e \"shiny::runApp('app.R', port=8787)\"\n")
