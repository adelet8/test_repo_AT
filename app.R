library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("GPU Quality Control (Starter)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload gpu_data.csv", accept = c(".csv")),
      selectInput("metric", "Metric to plot:",
                  choices = c("Temperature_C", "Power_W", "Utilization_Pct"),
                  selected = "Temperature_C"),
      sliderInput("temp_warn", "Temp warn threshold (°C):", min = 60, max = 100, value = 80)
    ),
    mainPanel(
      plotOutput("metricPlot", height = 350),
      hr(),
      h4("Flagged GPUs (Status != OK or Temp above threshold)"),
      tableOutput("flagTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Load data: either from upload or from file on disk
  gpu_data <- reactive({
    if (!is.null(input$file)) {
      read_csv(input$file$datapath, show_col_types = FALSE)
    } else {
      # fallback path if you saved it in the project folder:
      read_csv("gpu_data.csv", show_col_types = FALSE)
      # or use "data/gpu_data.csv" if you placed it in a data/ folder
    }
  })
  
  output$metricPlot <- renderPlot({
    df <- gpu_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = GPU_ID, y = .data[[input$metric]], fill = Status)) +
      geom_col() +
      labs(x = "GPU", y = input$metric, fill = "Status") +
      coord_flip() +                       # horizontal bars = easier to read
      theme_minimal(base_size = 12)
  })
  
  output$flagTable <- renderTable({
    df <- gpu_data()
    req(nrow(df) > 0)
    
    df %>%
      mutate(TempFlag = Temperature_C > input$temp_warn,
             FailFlag = Status != "OK") %>%
      filter(TempFlag | FailFlag) %>%
      arrange(desc(Status), desc(Temperature_C)) %>%
      select(GPU_ID, Status, Temperature_C, Power_W, Utilization_Pct,
             ECC_Corrected, ECC_Uncorrected, Fan_RPM)
  })
}

shinyApp(ui, server)
