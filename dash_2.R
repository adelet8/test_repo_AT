# app.R — SPC Dashboard (GPU-only, ±3σ control limits, Hierarchy Drilldown)
# Thin dataset expected columns:
# ts, asset_id, asset_type, server_id, rack_id, room_id,
# asset_temp_inlet_c, rack_inlet_c, room_temp_c, server_temp_c

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)   # install.packages("plotly") if needed
library(scales)

# ---- SPC helpers (SPC.R must be in the same directory) ----
source("SPC.R")
set_theme()  # from SPC.R

`%||%` <- function(a,b) if (!is.null(a)) a else b
num <- function(x) suppressWarnings(as.numeric(x))

REQ_COLS <- c(
  "ts","asset_id","asset_type","server_id","rack_id","room_id",
  "asset_temp_inlet_c","rack_inlet_c","room_temp_c","server_temp_c"
)

# ---- Control-limit stats (mean ± 3σ) ----
ctrl_limits <- function(x){
  x <- x[is.finite(x)]
  m <- mean(x); s <- sd(x)
  LCL <- m - 3*s; UCL <- m + 3*s
  pct_out <- mean(x < LCL | x > UCL) * 100
  list(mean=m, sd=s, LCL=LCL, UCL=UCL, pct_out=pct_out)
}

# ---- Spec capability (optional overlay; DOES NOT affect control limits) ----
spec_metrics <- function(x, LSL, USL){
  x <- x[is.finite(x)]
  m <- mean(x); s <- sd(x)
  Cp  <- if (is.finite(s) && s>0) (USL - LSL)/(6*s) else NA_real_
  Cpl <- if (is.finite(s) && s>0) (m - LSL)/(3*s)  else NA_real_
  Cpu <- if (is.finite(s) && s>0) (USL - m)/(3*s)  else NA_real_
  Cpk <- suppressWarnings(min(Cpl, Cpu))
  pct_oos <- mean(x < LSL | x > USL) * 100
  list(Cp=Cp, Cpk=Cpk, pct_oos=pct_oos)
}

# ---- Treemap hierarchy builder (Rack -> Server -> Asset) ----
build_treemap_df <- function(df){
  # df already filtered to GPUs in dv()
  racks   <- df %>% count(rack_id, name="n")
  servers <- df %>% count(rack_id, server_id, name="n")
  assets  <- df %>% count(rack_id, server_id, asset_id, name="n")
  
  r <- racks %>% transmute(id = rack_id, label = rack_id, parent = "ROOT", value = n)
  s <- servers %>% transmute(id = paste(rack_id, server_id, sep="|"),
                             label = server_id, parent = rack_id, value = n)
  a <- assets %>% transmute(id = paste(rack_id, server_id, asset_id, sep="|"),
                            label = asset_id, parent = paste(rack_id, server_id, sep="|"),
                            value = n)
  bind_rows(
    tibble(id="ROOT", label="Racks", parent=NA_character_, value=NA_integer_),
    r, s, a
  )
}

# ---- Local helper: rolling Xbar–S for a single asset (if not in SPC.R) ----
asset_xbar_s_window_local <- function(df, asset_id, metric_col, m = 5, show_labels = TRUE){
  d <- df %>%
    dplyr::filter(asset_id == !!asset_id) %>%
    dplyr::arrange(t_idx) %>%
    dplyr::mutate(subgroup = floor((dplyr::row_number() - 1L) / m))
  spc_xbar_s(d, metric_col = metric_col, subgroup_col = "subgroup",
             xlab = paste0("Subgroup (", m, " pts)"),
             show_labels = show_labels)
}

# ---------- Data prep ----------
load_initial <- function(){
  cand <- c(
    "datacenter_assets_timeseries_8h_GPU_DC-Room-1_THIN_SIM.csv",
    "datacenter_assets_thin_8h.csv"
  )
  f <- cand[file.exists(cand)]
  if (length(f)) readr::read_csv(f[1], show_col_types = FALSE) else NULL
}

prep_base <- function(df){
  # Validate columns
  miss <- setdiff(REQ_COLS, names(df))
  validate(need(length(miss)==0,
                paste("CSV must contain:", paste(REQ_COLS, collapse=", "))))
  # Create integer time index from "t_####"
  if (!"t_idx" %in% names(df)) {
    validate(need("ts" %in% names(df), "Missing ts column for time indexing."))
    df <- df %>% mutate(t_idx = as.integer(sub("^t_", "", ts)))
  }
  # Force types
  df %>%
    mutate(
      rack_id   = as.character(rack_id),
      server_id = as.character(server_id),
      asset_id  = as.character(asset_id),
      asset_type = as.character(asset_type),
      asset_temp_inlet_c = num(asset_temp_inlet_c),
      rack_inlet_c       = num(rack_inlet_c),
      room_temp_c        = num(room_temp_c),
      server_temp_c      = num(server_temp_c)
    )
}

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = "Datacenter SPC — GPU Only"),
  dashboardSidebar(
    sidebarMenu(menuItem("SPC", tabName = "spc", icon = icon("chart-line"))),
    hr(),
    fileInput("csv", "Upload CSV", accept = ".csv"),
    checkboxInput("show_labels", "Show SPC captions/labels", TRUE),
    hr(),
    # NEW: Specs UI (optional overlays)
    checkboxInput("show_specs", "Show specification limits (LSL/USL)", FALSE),
    sliderInput("lsl", "Lower Spec Limit (°C)", min = 18, max = 28, value = 22, step = 0.1),
    sliderInput("usl", "Upper Spec Limit (°C)", min = 24, max = 34, value = 30, step = 0.1),
    hr(),
    helpText("Click treemap to drill Rack → Server → Asset. Control limits = mean ± 3σ.")
  ),
  dashboardBody(
    tabItems(
      tabItem("spc",
              fluidRow(
                box(width = 12, title = "Controls", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             selectInput("metric", "Metric",
                                         choices = c("rack_inlet_c","server_temp_c","asset_temp_inlet_c","room_temp_c"),
                                         selected = "rack_inlet_c")
                      ),
                      column(4, helpText("Dataset must contain only GPU assets or will be filtered to GPUs.")),
                      column(4, helpText("Spec sliders affect Cp/Cpk overlays only; control limits remain μ ± 3σ."))
                    )
                )
              ),
              fluidRow(
                box(width = 5, title = "Hierarchy (Rack → Server → Asset)", status = "primary", solidHeader = TRUE,
                    plotlyOutput("tree", height = 420),
                    actionButton("reset", "Reset selection"),
                    br(), br(),
                    verbatimTextOutput("sel_txt")
                ),
                box(width = 7, title = "Time Series", status = "primary", solidHeader = TRUE,
                    plotlyOutput("ts_plot", height = 420)
                )
              ),
              fluidRow(
                box(width = 6, title = "Distribution (Histogram + Normal)", status = "primary", solidHeader = TRUE,
                    plotOutput("hist_plot", height = 320)
                ),
                box(width = 6, title = "Control & Capability Summary", status = "primary", solidHeader = TRUE,
                    uiOutput("caps")
                )
              ),
              fluidRow(
                box(width = 12, title = "SPC (X̄–S or I–MR)", status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(3, selectInput("spc_scope", "SPC scope",
                                            c("Room (Xbar–S)","Rack (Xbar–S)","Server (Xbar–S)","Asset (I–MR / Xbar–S-window)"),
                                            selected = "Rack (Xbar–S)")),
                      column(3, uiOutput("spc_picker")),
                      column(3, checkboxInput("asset_use_xbars", "Asset: use rolling Xbar–S", FALSE)),
                      column(3, numericInput("asset_m", "Rolling subgroup size (m)", value=5, min=2, max=60, step=1))
                    ),
                    plotOutput("spc_plot", height = 520)
                )
              )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  # Keep LSL < USL if user crosses them
  observe({
    if (isTRUE(input$show_specs) && !is.null(input$lsl) && !is.null(input$usl) && input$lsl >= input$usl) {
      updateSliderInput(session, "usl", value = input$lsl + 0.1)
    }
  })
  
  # Uploaded file OR fallback
  current_df <- reactiveVal(load_initial())
  observeEvent(input$csv, {
    req(input$csv$datapath)
    current_df(readr::read_csv(input$csv$datapath, show_col_types = FALSE))
  })
  
  base <- reactive({
    df <- current_df(); req(df)
    prep_base(df)
  })
  
  # Selection state for hierarchy
  sel <- reactiveVal(list(rack=NULL, server=NULL, asset=NULL))
  observeEvent(input$reset, { sel(list(rack=NULL, server=NULL, asset=NULL)) })
  
  output$sel_txt <- renderText({
    s <- sel()
    paste0(
      "Rack: ",   ifelse(is.null(s$rack), "(all)", s$rack), "\n",
      "Server: ", ifelse(is.null(s$server), "(all)", s$server), "\n",
      "Asset: ",  ifelse(is.null(s$asset), "(all)", s$asset)
    )
  })
  
  # Filtered view: GPU-only + current selection
  dv <- reactive({
    df <- base() %>% filter(asset_type == "GPU")
    s <- sel()
    if (!is.null(s$rack))   df <- df %>% filter(rack_id   == s$rack)
    if (!is.null(s$server)) df <- df %>% filter(server_id == s$server)
    if (!is.null(s$asset))  df <- df %>% filter(asset_id  == s$asset)
    df
  })
  
  # Treemap (GPU-only)
  output$tree <- renderPlotly({
    df <- dv()
    validate(need(nrow(df)>0, "No GPU data."))
    tdf <- build_treemap_df(df)
    
    plot_ly(
      source = "tree",
      type = "treemap",
      ids = tdf$id,
      labels = tdf$label,
      parents = tdf$parent,
      values = tdf$value,
      branchvalues = "total",
      hoverinfo = "label+value",
      textinfo = "label+value",
      customdata = tdf$id
    )
  })
  
  # Hierarchy click
  observeEvent(event_data("plotly_click", source = "tree"), {
    ed <- event_data("plotly_click", source = "tree")
    if (is.null(ed)) return()
    id <- ed$customdata %||% ed$id
    if (is.null(id) || identical(id, "ROOT")) {
      sel(list(rack=NULL, server=NULL, asset=NULL)); return()
    }
    parts <- strsplit(as.character(id), "\\|")[[1]]
    if (length(parts) == 1) {
      sel(list(rack=parts[1], server=NULL, asset=NULL))
    } else if (length(parts) == 2) {
      sel(list(rack=parts[1], server=parts[2], asset=NULL))
    } else {
      sel(list(rack=parts[1], server=parts[2], asset=parts[3]))
    }
  })
  
  # Time series — Asset: raw; Server/Rack/All: mean over selection
  output$ts_plot <- renderPlotly({
    df <- dv(); validate(need(nrow(df)>0, ""))
    s <- sel(); metric <- req(input$metric)
    
    if (!is.null(s$asset)) {
      pdat <- df %>% filter(asset_id == s$asset) %>% arrange(t_idx) %>% transmute(t=t_idx, y=.data[[metric]])
      title <- paste("Asset:", s$asset)
    } else {
      pdat <- df %>% group_by(t_idx) %>% summarise(y = mean(.data[[metric]], na.rm=TRUE), .groups="drop")
      if (!is.null(s$server))      title <- paste("Server:", s$server, "(mean)")
      else if (!is.null(s$rack))   title <- paste("Rack:", s$rack, "(mean)")
      else                         title <- "All (mean)"
      pdat <- pdat %>% transmute(t=t_idx, y=y)
    }
    
    # Control limits on current series
    cl <- ctrl_limits(pdat$y)
    
    g <- ggplot(pdat, aes(t, y)) +
      geom_line() +
      geom_point(size=0.8, alpha=0.6) +
      # Control limits (fixed at μ ± 3σ)
      geom_hline(yintercept = cl$LCL, linetype="dashed", color="#d62728") +
      geom_hline(yintercept = cl$UCL, linetype="dashed", color="#d62728") +
      geom_hline(yintercept = cl$mean, linetype="dotdash", color="#1f77b4") +
      # OPTIONAL: Specification limits (do not affect control limits)
      { if (isTRUE(input$show_specs)) geom_hline(yintercept = input$lsl, linetype="longdash") } +
      { if (isTRUE(input$show_specs)) geom_hline(yintercept = input$usl, linetype="longdash") } +
      labs(x="t (minute index)", y="°C", title = title,
           subtitle = paste0(
             "μ=", round(cl$mean,2), "   σ=", round(cl$sd,2),
             "   LCL=", round(cl$LCL,2), "   UCL=", round(cl$UCL,2),
             if (isTRUE(input$show_specs)) paste0("   LSL/USL=", input$lsl, "/", input$usl) else ""
           )) +
      theme_minimal(base_size = 12)
    
    ggplotly(g, tooltip = c("x","y"))
  })
  
  # Histogram + normal overlay
  output$hist_plot <- renderPlot({
    df <- dv(); validate(need(nrow(df)>0, ""))
    x <- df[[req(input$metric)]]
    cl <- ctrl_limits(x)
    m <- cl$mean; s <- cl$sd
    
    ggplot(data.frame(x=x), aes(x)) +
      geom_histogram(aes(y=..density..), bins=30, alpha=0.7) +
      { if (is.finite(s) && s>0) stat_function(fun = dnorm, args = list(mean=m, sd=s), linewidth=1.1) } +
      # Control limits
      geom_vline(xintercept = cl$LCL, linetype="dashed", color="#d62728") +
      geom_vline(xintercept = cl$UCL, linetype="dashed", color="#d62728") +
      geom_vline(xintercept = cl$mean, linetype="dotdash", color="#1f77b4") +
      # OPTIONAL: spec limits
      { if (isTRUE(input$show_specs)) geom_vline(xintercept = input$lsl, linetype="longdash") } +
      { if (isTRUE(input$show_specs)) geom_vline(xintercept = input$usl, linetype="longdash") } +
      labs(x="°C", y="Density",
           subtitle = paste0(
             "μ=", round(m,2), ", σ=", round(s,2),
             " | LCL=", round(cl$LCL,2), ", UCL=", round(cl$UCL,2),
             if (isTRUE(input$show_specs)) paste0(" | LSL/USL=", input$lsl, "/", input$usl) else ""
           )) +
      theme_minimal(base_size = 12)
  })
  
  # Control-limit & Capability summary
  output$caps <- renderUI({
    df <- dv(); validate(need(nrow(df)>0, NULL))
    x  <- df[[req(input$metric)]]
    cl <- ctrl_limits(x)
    
    specs_row <- if (isTRUE(input$show_specs)) {
      sm <- spec_metrics(x, input$lsl, input$usl)
      fluidRow(
        column(4, strong("LSL / USL"), br(), span(paste0(input$lsl, " / ", input$usl))),
        column(4, strong("Cp / Cpk"), br(), span(
          paste0(ifelse(is.na(sm$Cp),"NA", round(sm$Cp,3)),
                 " / ",
                 ifelse(is.na(sm$Cpk),"NA", round(sm$Cpk,3)))
        )),
        column(4, strong("% Out of Spec"), br(), span(paste0(round(sm$pct_oos,2),"%")))
      )
    } else NULL
    
    tagList(
      fluidRow(
        column(3, strong("μ (mean)"), br(), span(round(cl$mean,3))),
        column(3, strong("σ (stdev)"), br(), span(round(cl$sd,3))),
        column(3, strong("LCL / UCL (±3σ)"), br(), span(
          paste0(round(cl$LCL,2), " / ", round(cl$UCL,2)))
        ),
        column(3, strong("% beyond ±3σ"), br(), span(paste0(round(cl$pct_out,2),"%")))
      ),
      specs_row,
      tags$small("Specs (if shown) reflect requirements; control limits reflect process stability.")
    )
  })
  
  # -------- SPC panel (uses SPC.R) --------
  output$spc_picker <- renderUI({
    df <- dv(); req(nrow(df) > 0)  # already GPU-only
    sc <- input$spc_scope %||% "Rack (Xbar–S)"
    if (sc == "Room (Xbar–S)") {
      selectInput("spc_room_id", "Room", choices = sort(unique(df$room_id)),
                  selected = head(sort(unique(df$room_id)), 1))
    } else if (sc == "Rack (Xbar–S)") {
      selectInput("spc_rack_id", "Rack", choices = sort(unique(df$rack_id)),
                  selected = head(sort(unique(df$rack_id)), 1))
    } else if (sc == "Server (Xbar–S)") {
      selectInput("spc_server_id", "Server", choices = sort(unique(df$server_id)),
                  selected = head(sort(unique(df$server_id)), 1))
    } else {
      selectInput("spc_asset_id", "Asset", choices = sort(unique(df$asset_id)),
                  selected = head(sort(unique(df$asset_id)), 1))
    }
  })
  
  output$spc_plot <- renderPlot({
    df <- dv(); req(nrow(df) > 0)
    metric <- req(input$metric)
    
    validate(need("t_idx" %in% names(df),
                  "Missing t_idx (time index). Ensure your ts (t_0..t_479) parsed."))
    
    if (input$spc_scope == "Room (Xbar–S)") {
      rid <- req(input$spc_room_id)
      spc_room(df %>% filter(room_id == rid),
               room_id = rid, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      
    } else if (input$spc_scope == "Rack (Xbar–S)") {
      rk <- req(input$spc_rack_id)
      spc_rack(df %>% filter(rack_id == rk),
               rack_id = rk, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      
    } else if (input$spc_scope == "Server (Xbar–S)") {
      sid <- req(input$spc_server_id)
      spc_server(df %>% filter(server_id == sid),
                 server_id = sid, metric_col = metric,
                 show_labels = isTRUE(input$show_labels))
      
    } else {
      aid <- req(input$spc_asset_id)
      if (isTRUE(input$asset_use_xbars)) {
        if (exists("spc_asset_xbar_s_window", mode = "function")) {
          spc_asset_xbar_s_window(df, asset_id = aid, metric_col = metric,
                                  m = input$asset_m)
        } else {
          asset_xbar_s_window_local(df, asset_id = aid, metric_col = metric,
                                    m = input$asset_m, show_labels = isTRUE(input$show_labels))
        }
      } else {
        ggi_mr(df %>% filter(asset_id == aid) %>% arrange(t_idx),
               asset_id = aid, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      }
    }
  })
}

shinyApp(ui, server)
