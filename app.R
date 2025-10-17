# app.R — Rack View (metric switcher) + separate Data Table tab
library(shiny)

ui <- navbarPage(
  "Data Center QC",
  tabPanel(
    "Rack View",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload dataset.csv", accept = ".csv"),
        hr(),
        h4("Totals"),
        verbatimTextOutput("totals_txt"),
        hr(),
        selectInput(
          "metric",
          "Metric to display on dot list:",
          choices = c(
            "Available GPUs (sum per rack)"     = "avail",
            "Avg GPU Temp (°C) (mean per rack)" = "gpu_temp",
            "CPU Load (%) (mean per rack)"      = "cpu",
            "Ambient Temp (°C) (mean per rack)" = "ambient",
            "Humidity (%) (mean per rack)"      = "humidity",
            "Status (OK/WARN/FAIL precedence)"  = "status"
          ),
          selected = "avail"
        ),
        sliderInput("green_min",  "🟢 Green if value ≥", min = 0, max = 100, value = 6),
        sliderInput("yellow_min", "🟡 Yellow if value ≥ (else 🔴)", min = 0, max = 100, value = 3),
        helpText("When 'Status' is selected, colors ignore thresholds: 🔴 if any FAIL in rack, 🟡 if any WARN, else 🟢.")
      ),
      mainPanel(
        h4(textOutput("list_title")),
        tableOutput("dot_list")
      )
    )
  ),
  tabPanel(
    "Data Table",
    fluidPage(
      h4("CSV Data (from dataset.csv or uploaded file)"),
      tableOutput("raw_tbl")
    )
  )
)

server <- function(input, output, session){
  
  # Load data: upload -> dataset.csv -> tiny fallback
  servers <- reactive({
    if (!is.null(input$file)) {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      attr(df, "source") <- "uploaded file"
    } else if (file.exists("dataset.csv")) {
      df <- read.csv("dataset.csv", stringsAsFactors = FALSE)
      attr(df, "source") <- "dataset.csv in project"
    } else {
      df <- data.frame(
        rack_id = c("1B","4E"),
        server_id = c("srv-1B-01","srv-4E-02"),
        gpus_capacity = c(8,8),
        gpus_in_use   = c(2,7),
        cpus_per_server = c(64,64),
        cpu_load_pct = c(38,74),
        avg_gpu_temp_C = c(68,84),
        ambient_temp_C = c(23,27),
        humidity_pct = c(42,53),
        power_W = c(850,1005),
        status = c("OK","FAIL"),
        stringsAsFactors = FALSE
      )
      attr(df, "source") <- "fallback sample (put dataset.csv in project)"
    }
    df$available_gpus <- df$gpus_capacity - df$gpus_in_use
    df
  })
  
  # Per-rack summary based on selected metric
  rack_metric_df <- reactive({
    df <- servers()
    
    sum_by  <- function(x) tapply(x, df$rack_id, sum,  na.rm = TRUE)
    mean_by <- function(x) tapply(x, df$rack_id, mean, na.rm = TRUE)
    
    avail    <- sum_by(df$available_gpus)
    gpu_temp <- mean_by(df$avg_gpu_temp_C)
    cpu      <- mean_by(df$cpu_load_pct)
    ambient  <- mean_by(df$ambient_temp_C)
    humid    <- mean_by(df$humidity_pct)
    
    status_rank <- tapply(df$status, df$rack_id, function(s) {
      s <- toupper(s)
      if (any(s == "FAIL")) "FAIL" else if (any(s == "WARN")) "WARN" else "OK"
    })
    
    out <- data.frame(
      rack_id        = names(avail),
      value_avail    = as.numeric(avail),
      value_gpu_temp = as.numeric(gpu_temp[names(avail)]),
      value_cpu      = as.numeric(cpu[names(avail)]),
      value_ambient  = as.numeric(ambient[names(avail)]),
      value_humidity = as.numeric(humid[names(avail)]),
      status         = as.character(status_rank[names(avail)]),
      stringsAsFactors = FALSE
    )
    
    out$display_value <- switch(
      input$metric,
      "avail"    = out$value_avail,
      "gpu_temp" = out$value_gpu_temp,
      "cpu"      = out$value_cpu,
      "ambient"  = out$value_ambient,
      "humidity" = out$value_humidity,
      "status"   = NA_real_
    )
    out
  })
  
  # Auto-adjust thresholds to selected metric range
  observeEvent(list(input$metric, servers()), {
    rd <- rack_metric_df()
    if (input$metric != "status") {
      v <- rd$display_value; v <- v[is.finite(v)]
      if (length(v)) {
        rng <- range(v, na.rm = TRUE); lo <- floor(rng[1]); hi <- ceiling(rng[2])
        if (lo == hi) { lo <- 0; hi <- hi + 1 }
        updateSliderInput(session, "green_min",  min = lo, max = hi, value = max(lo, floor(0.7*hi)))
        updateSliderInput(session, "yellow_min", min = lo, max = hi-1, value = max(lo, floor(0.4*hi)))
      }
    } else {
      updateSliderInput(session, "green_min",  min = 0, max = 10, value = 6)
      updateSliderInput(session, "yellow_min", min = 0, max = 9,  value = 3)
    }
  }, ignoreInit = TRUE)
  
  # Totals block
  output$totals_txt <- renderText({
    df <- servers()
    src        <- attr(df, "source")
    racks      <- length(unique(df$rack_id))
    servers_n  <- length(unique(df$server_id))
    total_cap  <- sum(df$gpus_capacity,   na.rm = TRUE)
    total_used <- sum(df$gpus_in_use,     na.rm = TRUE)
    total_free <- sum(df$available_gpus,  na.rm = TRUE)
    total_cpus <- sum(df$cpus_per_server, na.rm = TRUE)
    paste0(
      "Source: ", src, "\n\n",
      "Racks: ", racks, "\n",
      "Servers: ", servers_n, "\n",
      "GPU slots (total / in use / free): ",
      total_cap, " / ", total_used, " / ", total_free, "\n",
      "CPUs (sum across servers): ", total_cpus
    )
  })
  
  # Title above the list
  output$list_title <- renderText({
    switch(input$metric,
           "avail"    = "Rack  |  Available GPUs",
           "gpu_temp" = "Rack  |  Avg GPU Temp (°C)",
           "cpu"      = "Rack  |  CPU Load (%)",
           "ambient"  = "Rack  |  Ambient Temp (°C)",
           "humidity" = "Rack  |  Humidity (%)",
           "status"   = "Rack  |  Status (OK/WARN/FAIL)"
    )
  })
  
  # Dot list table (visual page)
  output$dot_list <- renderTable({
    rd <- rack_metric_df()
    
    if (input$metric == "status") {
      rd$dot   <- ifelse(rd$status == "FAIL","🔴", ifelse(rd$status == "WARN","🟡","🟢"))
      rd$shown <- rd$status
      order_key <- ifelse(rd$status=="FAIL",3, ifelse(rd$status=="WARN",2,1))
      rd <- rd[order(-order_key, rd$rack_id), ]
    } else {
      rd$dot <- ifelse(rd$display_value >= input$green_min, "🟢",
                       ifelse(rd$display_value >= input$yellow_min, "🟡", "🔴"))
      rd$shown <- round(rd$display_value, 1)
      rd <- rd[order(-rd$display_value, rd$rack_id), ]
    }
    
    data.frame(
      Rack = rd$rack_id,
      Value = paste(rd$dot, rd$shown),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, bordered = FALSE, striped = FALSE, spacing = "s", align = "l")
  
  # Raw CSV table (table page)
  output$raw_tbl <- renderTable({
    df <- servers()
    want <- c("rack_id","server_id","gpus_capacity","gpus_in_use","available_gpus",
              "cpus_per_server","cpu_load_pct","avg_gpu_temp_C",
              "ambient_temp_C","humidity_pct","power_W","status")
    keep <- intersect(want, names(df))
    df[, keep, drop = FALSE]
  })
}

shinyApp(ui, server)
