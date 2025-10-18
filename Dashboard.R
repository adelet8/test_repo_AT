# app.R — SPC-only dashboard for THIN dataset
# Expected columns (thin schema):
# ts, asset_id, asset_type, server_id, rack_id, room_id,
# asset_temp_inlet_c, rack_inlet_c, room_temp_c, server_temp_c
# SPC helpers are provided by SPC.R (same folder).

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)

# ---- SPC helpers (SPC.R must be in the same directory) ----
source("SPC.R")
set_theme()

# ---------- Data helpers ----------
need_cols <- c("ts","asset_id","asset_type","server_id","rack_id","room_id",
               "asset_temp_inlet_c","rack_inlet_c","room_temp_c","server_temp_c")

load_initial <- function(){
  cand <- c(
    "datacenter_assets_timeseries_8h_GPU_DC-Room-1_THIN_SIM.csv",
    "datacenter_assets_thin_8h.csv"
  )
  f <- cand[file.exists(cand)]
  if (length(f)) read_csv(f[1], show_col_types = FALSE) else NULL
}

prep_base <- function(df){
  validate(need(all(need_cols %in% names(df)),
                paste("CSV must contain:", paste(need_cols, collapse=", "))))
  # time index from "t_0".."t_479" (8 hours per minute)
  if (!"t_idx" %in% names(df)) {
    validate(need("ts" %in% names(df), "Missing ts column for time indexing."))
    df <- df %>% mutate(t_idx = as.integer(sub("^t_", "", ts)))
  }
  df
}

# ---------- UI ----------
ui <- dashboardPage(
  dashboardHeader(title = "Datacenter SPC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("SPC", tabName = "spc", icon = icon("chart-line"))
    ),
    hr(),
    fileInput("csv", "Upload CSV", accept = ".csv"),
    checkboxInput("show_labels", "Show captions/labels", TRUE)
  ),
  dashboardBody(
    tabItems(
      tabItem("spc",
              fluidRow(
                box(width = 12, title = "SPC Controls", status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(3, selectInput("scope", "Scope",
                                            c("Room (Xbar–S)",
                                              "Rack (Xbar–S)",
                                              "Server (Xbar–S)",
                                              "Asset (I–MR / windowed Xbar–S)"),
                                            selected = "Room (Xbar–S)")),
                      column(3, uiOutput("picker_ui")),
                      column(3, uiOutput("metric_ui")),
                      column(3, 
                             # Only shown for Asset scope
                             conditionalPanel(
                               condition = "input.scope == 'Asset (I–MR / windowed Xbar–S)'",
                               radioButtons("asset_method", "Asset method",
                                            c("Individuals–MR" = "imr",
                                              "Xbar–S (windowed)" = "xbar_s"),
                                            selected = "imr"),
                               sliderInput("asset_window", "Window size (m)",
                                           min = 3, max = 20, value = 5, step = 1)
                             ),
                             helpText("Xbar–S: multiple observations per subgroup; I–MR: one asset over time.")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12, title = "Statistical Process Control", status = "primary", solidHeader = TRUE,
                    plotOutput("spc_plot", height = 520))
              )
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session){
  
  r_data <- reactiveVal(load_initial())
  
  observeEvent(input$csv, {
    req(input$csv$datapath)
    r_data(read_csv(input$csv$datapath, show_col_types = FALSE))
  })
  
  base <- reactive({
    df <- r_data(); req(df)
    prep_base(df)
  })
  
  # Metric choices come from the thin schema (numeric only)
  output$metric_ui <- renderUI({
    df <- base(); req(df)
    numeric_cols <- intersect(c("rack_inlet_c","room_temp_c","server_temp_c","asset_temp_inlet_c"),
                              names(df))
    validate(need(length(numeric_cols) > 0, "No numeric metrics found."))
    selectInput("metric", "Metric", choices = numeric_cols, selected = numeric_cols[1])
  })
  
  # Dynamic picker (room/rack/server/asset) based on scope
  output$picker_ui <- renderUI({
    df <- base(); req(df)
    sc <- if (is.null(input$scope)) "Room (Xbar–S)" else input$scope
    
    if (sc == "Room (Xbar–S)") {
      rooms <- sort(unique(df$room_id))
      selectInput("room_id", "Room", choices = rooms, selected = rooms[1])
    } else if (sc == "Rack (Xbar–S)") {
      racks <- sort(unique(df$rack_id))
      selectInput("rack_id", "Rack", choices = racks, selected = racks[1])
    } else if (sc == "Server (Xbar–S)") {
      srvs <- sort(unique(df$server_id))
      selectInput("server_id", "Server", choices = srvs, selected = srvs[1])
    } else {
      assets <- sort(unique(df$asset_id))
      selectInput("asset_id", "Asset ID", choices = assets, selected = assets[1])
    }
  })
  
  output$spc_plot <- renderPlot({
    df <- base(); req(nrow(df) > 0)
    metric <- req(input$metric)
    
    # Route to SPC plot
    if (input$scope == "Room (Xbar–S)") {
      rid <- req(input$room_id)
      spc_room(df %>% filter(room_id == rid),
               room_id = rid, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      
    } else if (input$scope == "Rack (Xbar–S)") {
      rk <- req(input$rack_id)
      spc_rack(df %>% filter(rack_id == rk),
               rack_id = rk, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      
    } else if (input$scope == "Server (Xbar–S)") {
      sid <- req(input$server_id)
      spc_server(df %>% filter(server_id == sid),
                 server_id = sid, metric_col = metric,
                 show_labels = isTRUE(input$show_labels))
      
    } else {
      aid <- req(input$asset_id)
      method <- if (is.null(input$asset_method)) "imr" else input$asset_method
      if (method == "imr") {
        ggi_mr(df %>% filter(asset_id == aid) %>% arrange(t_idx),
               asset_id = aid, metric_col = metric,
               show_labels = isTRUE(input$show_labels))
      } else {
        m <- req(input$asset_window)
        spc_asset_xbar_s_window(df, asset_id = aid, metric_col = metric, m = m)
      }
    }
  })
}

shinyApp(ui, server)
