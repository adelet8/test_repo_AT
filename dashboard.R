# app.R — SPC Temperature Dashboard (Rack → Server → GPU)
# Click treemap to drill; plots update instantly.
# Packages: shiny, bslib, readr, dplyr, tidyr, ggplot2, plotly, scales, lubridate

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)

`%||%` <- function(a,b) if (!is.null(a)) a else b

# Expect this schema in spc_dataset.csv:
# timestamp, rack_id, server_id, gpu_id, asset_type, temp_c
REQ_COLS <- c("timestamp","rack_id","server_id","gpu_id","asset_type","temp_c")
num <- function(x) suppressWarnings(as.numeric(x))

capability <- function(x, LSL, USL){
  x <- x[is.finite(x)]
  m <- mean(x); s <- sd(x); rng <- USL - LSL
  Cp  <- if (is.finite(s) && s>0) rng/(6*s) else NA_real_
  Cpl <- if (is.finite(s) && s>0) (m - LSL)/(3*s) else NA_real_
  Cpu <- if (is.finite(s) && s>0) (USL - m)/(3*s) else NA_real_
  Cpk <- suppressWarnings(min(Cpl, Cpu))
  pct_oos <- mean(x < LSL | x > USL) * 100
  list(mean=m, sd=s, Cp=Cp, Cpk=Cpk, Pp=Cp, Ppk=Cpk, pct_oos=pct_oos)
}

# Build treemap hierarchy: Rack -> Server -> GPU
build_treemap_df <- function(df){
  racks   <- df %>% count(rack_id, name="n")
  servers <- df %>% count(rack_id, server_id, name="n")
  gpus    <- df %>% filter(!is.na(gpu_id) & gpu_id != "NA" & gpu_id != "") %>%
    count(rack_id, server_id, gpu_id, name="n")
  
  r <- racks %>% transmute(id = rack_id, label = rack_id, parent = "ROOT", value = n)
  s <- servers %>% transmute(id = paste(rack_id, server_id, sep="|"),
                             label = server_id, parent = rack_id, value = n)
  g <- gpus %>% transmute(id = paste(rack_id, server_id, gpu_id, sep="|"),
                          label = gpu_id, parent = paste(rack_id, server_id, sep="|"),
                          value = n)
  
  bind_rows(
    tibble(id="ROOT", label="Racks", parent=NA_character_, value=NA_integer_),
    r, s, g
  )
}

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  h2("SPC — Temperature Control (Rack → Server → GPU)"),
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Controls"),
      checkboxGroupInput("asset_types","Asset types", choices=c("SERVER","GPU"),
                         selected=c("SERVER","GPU"), inline=TRUE),
      sliderInput("lsl", "Lower Spec Limit (°C)", min=18, max=28, value=22, step=0.1),
      sliderInput("usl", "Upper Spec Limit (°C)", min=24, max=40, value=30, step=0.1),
      actionButton("reset", "Reset selection"),
      hr(),
      h4("Drilldown selection"),
      verbatimTextOutput("sel_txt"),
      helpText("Click the treemap to drill Rack → Server → GPU. Reset to go back.")
    ),
    card(
      layout_columns(
        col_widths = c(5,7),
        card(
          card_header("Hierarchy"),
          plotlyOutput("tree", height = 360)
        ),
        card(
          card_header("Time Series (°C)"),
          plotlyOutput("ts_plot", height = 360)
        )
      ),
      layout_columns(
        col_widths = c(6,6),
        card(
          card_header("Distribution (Histogram + Normal)"),
          plotOutput("hist_plot", height = 320)
        ),
        card(
          card_header("Capability"),
          uiOutput("caps")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  # Always load spc_dataset.csv from the app directory
  raw <- reactive({
    path <- c("spc_dataset.csv")[file.exists("spc_dataset.csv")][1]
    validate(need(isTruthy(path),
                  "Place spc_dataset.csv in the same folder as app.R"))
    df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    miss <- setdiff(REQ_COLS, names(df))
    validate(need(length(miss)==0, paste("Missing columns:", paste(miss, collapse=", "))))
    df %>%
      mutate(
        timestamp = lubridate::ymd_hms(timestamp, quiet=TRUE),
        temp_c = num(temp_c),
        rack_id = as.character(rack_id),
        server_id = as.character(server_id),
        gpu_id = ifelse(is.na(gpu_id), NA_character_, as.character(gpu_id)),
        asset_type = as.character(asset_type)
      ) %>%
      filter(!is.na(timestamp), is.finite(temp_c))
  })
  
  # Selection state (Rack -> Server -> GPU)
  sel <- reactiveVal(list(rack=NULL, server=NULL, gpu=NULL))
  observeEvent(input$reset, { sel(list(rack=NULL, server=NULL, gpu=NULL)) })
  
  output$sel_txt <- renderText({
    s <- sel()
    paste0(
      "Rack: ",   ifelse(is.null(s$rack), "(all)", s$rack), "\n",
      "Server: ", ifelse(is.null(s$server), "(all)", s$server), "\n",
      "GPU: ",    ifelse(is.null(s$gpu), "(all)", s$gpu)
    )
  })
  
  # Data view filtered by asset type + selection
  dv <- reactive({
    df <- raw() %>% filter(asset_type %in% input$asset_types)
    s <- sel()
    if (!is.null(s$rack))   df <- df %>% filter(rack_id   == s$rack)
    if (!is.null(s$server)) df <- df %>% filter(server_id == s$server)
    if (!is.null(s$gpu))    df <- df %>% filter(gpu_id    == s$gpu)
    df
  })
  
  # Treemap (Rack -> Server -> GPU); include customdata for reliable clicks
  output$tree <- renderPlotly({
    df <- raw() %>% filter(asset_type %in% input$asset_types)
    validate(need(nrow(df)>0, "No data."))
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
  
  # Click handler — parse Rack | Server | GPU from customdata
  observeEvent(event_data("plotly_click", source = "tree"), {
    ed <- event_data("plotly_click", source = "tree")
    if (is.null(ed)) return()
    id <- ed$customdata %||% ed$id
    if (is.null(id) || identical(id, "ROOT")) {
      sel(list(rack=NULL, server=NULL, gpu=NULL)); return()
    }
    parts <- strsplit(as.character(id), "\\|")[[1]]
    if (length(parts) == 1) {
      sel(list(rack=parts[1], server=NULL, gpu=NULL))
    } else if (length(parts) == 2) {
      sel(list(rack=parts[1], server=parts[2], gpu=NULL))
    } else {
      sel(list(rack=parts[1], server=parts[2], gpu=parts[3]))
    }
  })
  
  # Time series — GPU: raw; Server/Rack/All: mean over selection
  output$ts_plot <- renderPlotly({
    df <- dv(); validate(need(nrow(df)>0, ""))
    s <- sel()
    
    if (!is.null(s$gpu)) {
      pdat <- df %>% filter(gpu_id == s$gpu)
      title <- paste("GPU:", s$gpu)
    } else if (!is.null(s$server)) {
      pdat <- df %>% group_by(timestamp) %>% summarise(temp_c = mean(temp_c), .groups="drop")
      title <- paste("Server:", s$server, "(mean)")
    } else if (!is.null(s$rack)) {
      pdat <- df %>% group_by(timestamp) %>% summarise(temp_c = mean(temp_c), .groups="drop")
      title <- paste("Rack:", s$rack, "(mean)")
    } else {
      pdat <- df %>% group_by(timestamp) %>% summarise(temp_c = mean(temp_c), .groups="drop")
      title <- "All Racks (mean)"
    }
    
    LSL <- input$lsl; USL <- input$usl; med <- median(pdat$temp_c, na.rm=TRUE)
    
    g <- ggplot(pdat, aes(timestamp, temp_c)) +
      geom_line() +
      geom_point(size=0.8, alpha=0.6) +
      geom_hline(yintercept = LSL, linetype="dashed", color="#d62728") +
      geom_hline(yintercept = USL, linetype="dashed", color="#d62728") +
      geom_hline(yintercept = med, linetype="dotdash", color="#1f77b4") +
      labs(x=NULL, y="°C", title = title,
           subtitle = paste0("LSL=", LSL, "   USL=", USL, "   Median=", round(med,2))) +
      theme_minimal(base_size = 12)
    
    ggplotly(g, tooltip = c("x","y"))
  })
  
  # Histogram + normal overlay for current selection
  output$hist_plot <- renderPlot({
    df <- dv(); validate(need(nrow(df)>0, ""))
    x <- df$temp_c
    LSL <- input$lsl; USL <- input$usl; med <- median(x, na.rm=TRUE)
    m <- mean(x); s <- sd(x)
    
    ggplot(data.frame(x=x), aes(x)) +
      geom_histogram(aes(y=..density..), bins=30, alpha=0.7) +
      { if (is.finite(s) && s>0) stat_function(fun = dnorm, args = list(mean=m, sd=s), linewidth=1.1) } +
      geom_vline(xintercept = LSL, linetype="dashed", color="#d62728") +
      geom_vline(xintercept = USL, linetype="dashed", color="#d62728") +
      geom_vline(xintercept = med, linetype="dotdash", color="#1f77b4") +
      labs(x="°C", y="Density",
           subtitle = paste0("Normal overlay: μ=", round(m,2), ", σ=", round(s,2))) +
      theme_minimal(base_size = 12)
  })
  
  # Capability metrics for current selection
  output$caps <- renderUI({
    df <- dv(); validate(need(nrow(df)>0, NULL))
    c <- capability(df$temp_c, input$lsl, input$usl)
    tagList(
      tags$div(style="display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;",
               card(body=div(h4("μ (mean)"), p(round(c$mean,3)))),
               card(body=div(h4("σ (stdev)"), p(round(c$sd,3)))),
               card(body=div(h4("% Out of Spec"), p(paste0(round(c$pct_oos,2),"%")))),
               card(body=div(h4("Cp"),  p(ifelse(is.na(c$Cp),"NA",  round(c$Cp,3))))),
               card(body=div(h4("Cpk"), p(ifelse(is.na(c$Cpk),"NA", round(c$Cpk,3))))),
               card(body=div(h4("Pp"),  p(ifelse(is.na(c$Pp),"NA",  round(c$Pp,3))))),
               card(body=div(h4("Ppk"), p(ifelse(is.na(c$Ppk),"NA", round(c$Ppk,3)))))
      ),
      tags$small("Spec limits (LSL/USL). Want control limits (±3σ) or I-MR/Xbar-R charts? I can add them.")
    )
  })
}

shinyApp(ui, server)
