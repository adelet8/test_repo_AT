```r
############ Dashboard Example ######################
# APP V2 - FINAL VERSION
# TEAM 7-11: ANALYSIS OF BOSTON'S BLUE BIKE DATA
# SYSEN 5460


global = function(){
  
  library(dplyr) # data wrangling
  library(readr) # reading data
  library(ggplot2) # data vizualization
  library(scales) # for rescaling values
  library(shiny) # main shiny app package
  library(RSQLite) #database connection
  library(stringr) #string manipulation
  library(lubridate) #date manipulation
  library(tidyr)
  library(shinydashboard) #styled boxes
  library(sf) # to read/work with sf data
  library(ggspatial) # for plotting maps
  library(cowplot)
  library(plotly)
  library(bslib)
  
  
  #load the data so it is available for ui and server
  
  rush_db = dbConnect(RSQLite::SQLite(), "data/bluebikes/bluebikes.sqlite")
  tally_rush_data = dbReadTable(rush_db, "tally_rush")
  dbDisconnect(rush_db)
  
  tally_rush_data$day = as.Date(tally_rush_data$day)  #convert "day" column into Date format
  tally_rush_data$year = lubridate::year(tally_rush_data$day)  #Extract year into a new "year" column
  tally_rush_data$month <- lubridate::month(tally_rush_data$day) # Extract year into a new "month" Column
  tally_rush_data$day_of_year = yday(tally_rush_data$day) #extract day for plotting
  
  # Load Data for the Maps
  sta_bg <- read_rds("data/stationbg_dataset.rds") %>%
    select(1:17)
  boston <- read_sf("data/bounds.geojson") %>%
    filter(name == "boston")
  blocks <- read_rds("data/bgdataset.rds") %>%
    select(1:16, pop_density_2020_smooth5) # Include pop_density_2020_smooth5
  neighborhood <- read_sf("data/boston_neighborhood_boundaries.json") %>%
    st_make_valid() %>%
    select(name, sqmiles, geometry)
  
  
  #store the data in global environment
  assign("tally_rush_data", tally_rush_data, envir = .GlobalEnv)
  assign("sta_bg", sta_bg, envir = .GlobalEnv)
  assign("boston", boston, envir = .GlobalEnv)
  assign("blocks", blocks, envir = .GlobalEnv)
  assign("neighborhood", neighborhood, envir = .GlobalEnv)
  
  
}

ui = function(){
  
  # Load up the databases
  tally_rush_data = get("tally_rush_data", envir = .GlobalEnv)
  sta_bg = get("sta_bg", envir = .GlobalEnv)
  boston = get("boston", envir = .GlobalEnv)
  blocks = get("blocks", envir = .GlobalEnv)
  neighborhood = get("neighborhood", envir = .GlobalEnv)
  
  available_years = sort(unique(tally_rush_data$year))
  
  navbarPage("Bluebikes Boston Rider Dashboard",
             
             # Page 1
             tabPanel("Page 1: Dashboard",
                      fluidPage(
                        titlePanel("Bluebikes Boston Rider Dashboard"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("selected_year", "Select Year:", choices = available_years, selected = available_years[11]),
                            radioButtons("rush_period", "Select Rush Period:", choices = c("AM" = "am", "PM" = "pm"), selected = "am"),
                            radioButtons("graph_choice", "Select Graph Choices:", choices = c("Daily Line Plot" = "Daily", "Monthly Bar Plot" = "Monthly"), selected = "Daily"),
                            br(),
                            h4("Average Monthly Ride Count Comparisons and Year-to-Year Trends"),
                            tableOutput("monthly_stats")
                          ),
                          mainPanel(
                            h3("Key Statistics"),
                            fluidRow(
                              column(6, uiOutput("vb1")),
                              column(6, uiOutput("vb2"))
                            ),
                            br(),
                            plotOutput("rider_count_plot"),
                            br(),
                            plotOutput("distribution_plot"),
                            br(),
                            h4("Median Ride Counts per Month"),
                            tableOutput("monthly_median")
                          )
                        )
                      )
             ),
             
             # Page 2
             tabPanel("Page 2: Density Map",
                      fluidPage(
                        div(
                          h1("Spatial Analysis of Boston Blue Bike Stations by Density", style = "text-align: center;")),
                        
                        sidebarLayout( # Use sidebar Layout for a common dashboard layout
                          sidebarPanel(
                            checkboxInput(inputId = "select_bikes",
                                          label = "Blue Bike Stations on Map",
                                          value = TRUE),
                            uiOutput("neighborhood_options")
                            
                          ),
                          
                          mainPanel(
                            fluidRow(
                              column(width = 12, 
                                 div(
                                   style = "text-align: center; background-color:  #FCB97D; border:1px solid; padding: 10px; border-radius:6px;",
                                   textOutput("neighborhood_stats")))), # Added text output for most recently selected neighborhood
                            plotOutput(outputId ="density_plot", height = "600px"),
                            br(),
                            h4("Quantities of Interest", style = "text-align: center; background-color:  #FCB97D; border:1px solid; padding: 10px; border-radius: 5px;"), # Added styling
                            fluidRow(
                              valueBoxOutput("dens_low", width = 6),
                              valueBoxOutput("dens_high", width = 6)
                            ),
                            br(),
                            plotlyOutput(outputId = "bar_chart")
                          ),
                          position = "left"
                          
                        )
                      )),
             
             # Page 3
             tabPanel("Page 3: Income Map",
                      fluidPage(
                        div(
                          h1("Spatial Analysis of Boston Blue Bike Stations by Annual Income", style = "text-align: center;")),
                        
                        sidebarLayout( # Use sidebar Layout for a common dashboard layout
                          sidebarPanel(
                            checkboxInput(inputId = "select_bikes_income", 
                                          label = "Blue Bike Stations on Map",
                                          value = TRUE),
                            checkboxGroupInput("sel_incomeBracket", "Select Income Bracket:",
                                               choices = c("$0 - $40k" = "pop_0_40000_2019",
                                                           "$40k - $60k" = "pop_40001_60000_2019",
                                                           "$60k - $100k" = "pop_60001_100000_2019",
                                                           "$100k+" = "pop_100000_plus_2019",
                                                           "Less than 50% in any income category" = "Below Threshold"),
                                               selected = c("pop_0_40000_2019",
                                                            "pop_40001_60000_2019",
                                                            "pop_100000_plus_2019",
                                                            "Below Threshold"))
                          ),
                          
                          mainPanel(
                            plotOutput(outputId ="income_plot", height = "600px"),
                            br(),
                            h4("Quantities of Interest", style = "text-align: center; background-color: #f0f8ff; border:1px solid; padding: 10px; border-radius: 5px;"), # Added styling
                            fluidRow( 
                              valueBoxOutput("income_low", width = 3), # Adjusted width
                              valueBoxOutput("income_high", width = 3), # Adjusted width
                              valueBoxOutput("pop_density_income", width = 3), # Added new value box
                              valueBoxOutput("total_boston_pop_density_box", width = 3) # Moved value box for total pop density
                            ),
                            br(),
                            fluidRow( 
                              column(12, textOutput("selected_income_percentage")) # Added text output for percentage
                            )
                          ),
                          position = "left"
                        )
                      )
             )
  )
}

server = function(input, output, session){
  
  # Access the data loaded in the global function
  tally_rush_data = get("tally_rush_data", envir = .GlobalEnv)
  sta_bg = get("sta_bg", envir = .GlobalEnv)
  boston = get("boston", envir = .GlobalEnv)
  blocks = get("blocks", envir = .GlobalEnv)
  neighborhood = get("neighborhood", envir = .GlobalEnv)
  
  
  filtered_data = reactive({
    tally_rush_data %>%
      filter(year == as.integer(input$selected_year), rush == input$rush_period) %>%
      mutate(month = month(day, label = TRUE, abbr = TRUE)) %>%
      arrange(day)
  })
  
  # Styled value boxes manually via renderUI
  output$vb1 <- renderUI({
    total <- sum(filtered_data()$count, na.rm = TRUE)
    div(style = "background-color:#e8f5e9; padding:20px; border-radius:6px; height:120px;",
        h3(style = "font-size:28px; font-weight:bold; margin:0;", format(total, big.mark = ",")),
        p(style = "margin:0;", paste("Total Riders in", input$selected_year, toupper(input$rush_period), "Rush Hour")),
        icon("bicycle", style = "font-size:30px; opacity:0.3; margin-top:5px;")
    )
  })
  
  output$vb2 <- renderUI({
    avg <- round(mean(filtered_data()$count, na.rm = TRUE), 1)
    div(style = "background-color:#e3f2fd; padding:20px; border-radius:6px; height:120px;",
        h3(style = "font-size:28px; font-weight:bold; margin:0;", avg),
        p(style = "margin:0;", paste("Average Riders per Day in", input$selected_year)),
        icon("chart-line", style = "font-size:30px; opacity:0.3; margin-top:5px;")
    )
  })
  
  stats = reactive({
    
    sel_year <- as.integer(input$selected_year)
    prev_year <- sel_year - 1
    
    data2 <- tally_rush_data %>%
      filter(year %in% c(prev_year, sel_year), rush == input$rush_period) %>%
      mutate(month = factor(month(day, label = TRUE, abbr = TRUE), levels = month.abb, ordered = FALSE)) %>%
      group_by(year, month) %>%
      summarize(avg_count = mean(count), .groups = "drop") %>%
      complete(year = c(sel_year, prev_year), month = factor(month.abb, levels = month.abb, ordered = FALSE),
               fill = list(avg_count = NA)) %>%
      pivot_wider(names_from = year, values_from = avg_count, names_prefix = "Avg_")
    
    colnames(data2) <- c("Month",
                         paste0("Avg ",toupper(input$rush_period), " Count ", prev_year),
                         paste0("Avg ",toupper(input$rush_period), " Count ", sel_year))
    
    col_order <- c("Month",
                   paste0("Avg ",toupper(input$rush_period), " Count ", sel_year),
                   paste0("Avg ",toupper(input$rush_period), " Count ", prev_year))
    
    data2 <- data2[, col_order, drop = FALSE]
    
    # Keep NA as is, just round numeric columns
    data2 %>%
      mutate(across(where(is.numeric), ~ round(., 1)))
  })
  
  
  month_med = reactive({
    data3 <- tally_rush_data %>%
      filter(year == input$selected_year, rush == input$rush_period) %>%
      mutate(month = factor(month(day, label = TRUE, abbr = TRUE), levels = month.abb)) %>%
      group_by(month) %>%
      summarize(med_count = median(count))
    
    wide_data <- data3 %>%
      pivot_wider(names_from = month, values_from = med_count)
    
    rbind(names(wide_data), as.character(unlist(wide_data))) %>%
      cbind(c(input$selected_year, "Median"), .) %>%
      as_tibble(.name_repair = "minimal")
  })
  
  output$rider_count_plot <- renderPlot({
    data_to_plot <- filtered_data()
    
    if (nrow(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available"))
    }
    graph_color <- if (input$rush_period == "am") "#2E8B57" else "darkblue"
    if (input$graph_choice == "Daily") {
      ggplot(data_to_plot, aes(x = day, y = count)) +
        geom_line(color = graph_color) +
        geom_point() +
        labs(title = paste("Daily Rider Count For", input$selected_year, toupper(input$rush_period), "Rush Hour"),
             x = "Date", y = "Total Riders") +
        theme_minimal() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
    } else {
      # Summarize the monthly totals
      data_to_plot2 <- data_to_plot %>%
        group_by(month) %>%
        summarize(total = sum(count), .groups = "drop")
      
      ggplot(data_to_plot2, aes(x = month, y = total)) +
        geom_col(fill = graph_color) +
        labs(title = paste("Monthly Rider Count For", input$selected_year, toupper(input$rush_period), "Rush Hour"),
             x = "Month", y = "Total Riders") +
        theme_minimal()
      
    }
  }) %>% bindEvent({ input$selected_year; input$rush_period; input$graph_choice })
  
  output$distribution_plot <- renderPlot({
    data_to_plot <- filtered_data()
    medians <- data_to_plot %>% group_by(month) %>% summarize(med = median(count))
    data_to_plot <- left_join(data_to_plot, medians, by = "month")
    
    if (nrow(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0, y = 0, label = "No data available"))
    }
    
    ggplot(data_to_plot, aes(x = month, y = count, fill = med)) +
      geom_boxplot(varwidth = TRUE) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = paste("Monthly Distribution of", toupper(input$rush_period), "Rush Hour Ride Counts in", input$selected_year),
           x = "Month", y = "Total Riders", fill = "Median") +
      theme_minimal()
  }) %>% bindEvent({ input$selected_year; input$rush_period })
  
  # Output table with median values
  output$monthly_median <- renderTable({ month_med() }, digits = 1)%>%
    bindEvent({input$selected_year; input$rush_period}) # Updates table
  # Output table for summary of monthly averages
  output$monthly_stats <- renderTable({ stats() }, digits = 1) %>%
    bindEvent({ input$selected_year; input$rush_period}) # Updates table
  
  ###_______________________ MAP SECTION _______________________________________
  # Initial Map Calculations:
  
  # For plot by density:
  sta_in <- sta_bg %>%
    select(geometry, geoid) %>%
    st_join(boston, left = FALSE)       # Filter to bike stations in Boston
  
  blocks_in <- blocks %>%
    select(geometry, geoid) %>%
    st_join(sta_in, left = FALSE) %>%   # Census blocks inside Boston w/Bike stations
    select(-geoid.y)
  
  blocks_bos <- blocks %>%
    st_join(boston, left = FALSE)       # All census blocks inside Boston
  
  # Convert areas in neighborhood data.frame to units of km^2
  area_km2 <- neighborhood %>%
    mutate(area = sqmiles*2.58999) %>%
    st_drop_geometry() %>%
    select(name, area)
  
  # Add area column to sta_neigh data.frame
  sta_neigh <- neighborhood %>%
    st_join(sta_in, left = FALSE, join = st_intersects) %>%
    select(name.x, geometry) %>%
    arrange(name.x) %>%
    add_count(name.x, name = 'n') %>%
    distinct(name.x, .keep_all = TRUE) %>%
    rename(name = name.x) %>%
    left_join(area_km2, by = "name")
  
  # Calculate the density of blue bike stations in each neighborhood, arrange highest to lowest
  dens_neigh <- sta_neigh %>%
    mutate(density = n / area) %>%
    arrange(desc(density))
  
  # WANT TO OUTPUT THESE MAX, MIN, MEDIAN VALUES
  # Find min, max densities
  min_dens <- dens_neigh %>% filter(density == min(density))    # Hyde Park, 0.253 stations/km^2
  
  max_dens <- dens_neigh %>% filter(density == max(density))    # Downtown, 11.8 stations/km^2
  
  # Reactive UI Checkboxes
  output$neighborhood_options <- renderUI({
    
    checkboxGroupInput("sel_neighborhood", "Select Neighborhoods:",
                       choices = c("None", sort(unique(dens_neigh$name))),
                       selected = "None")
  })
  
  
  
  
  # Income Calcs
  income_data <- blocks_bos %>%
    rowwise() %>% # Process row by row
    mutate(
      # Create a named vector of income values
      income_values = list(c(
        pop_0_40000_2019 = pop_0_40000_2019,
        pop_40001_60000_2019 = pop_40001_60000_2019,
        pop_60001_100000_2019 = pop_60001_100000_2019,
        pop_100000_plus_2019 = pop_100000_plus_2019)),
      # Find the maximum value among the income columns
      max_income_value = if (all(is.na(income_values))){NA_real_
      } else {
        max(unlist(income_values), na.rm = TRUE)},
      income_category = case_when(
        is.na(max_income_value) ~ "Unknown",
        max_income_value < 0.5 ~ "Below Threshold",
        TRUE ~ {
          iv <- income_values
          if(length(iv) == 0 || all(is.na(iv))){
            "Unknown"
          } else {
            names(iv)[which.max(iv)]
          }
        }
      ) ) %>%
    ungroup() %>% # Ungroup the data
    select(geoid, geometry, max_income_value, income_category, pop_density_2020_smooth5) # Include pop_density_2020_smooth5
  
  # WANT STATS FOR VALUE BOX OUTPUTS!
  # Calculate the percentage of low and high income families, and averages in each category
  income_count <- income_data %>%
    select(max_income_value) %>%
    st_drop_geometry %>%
    summarize( n = n()) %>% pull(n)
  
  low_income <- income_data %>%
    filter(max_income_value < 0.6) %>%
    summarize( n_low = n(),
               percentage = n_low/income_count,
               avg_low = mean(max_income_value, na.rm = TRUE)) # Added na.rm = TRUE
  
  high_income <- income_data %>%
    filter(max_income_value >0.6) %>%
    summarize( n_high = n(),
               percentage = n_high/income_count,
               avg_high = mean(max_income_value, na.rm = TRUE)) # Added na.rm = TRUE
  
  # Calculate total Boston population density
  total_boston_pop_density <- blocks_bos %>%
    st_drop_geometry() %>%
    summarize(total_density = sum(pop_density_2020_smooth5, na.rm = TRUE)) %>%
    pull(total_density)
  
  #_______________________________________________________________________________
  # Reactive variables:
  
  # Neighborhood for highlighting
  show_neigh = reactive({
    
    if(is.null(input$sel_neighborhood) || length(input$sel_neighborhood) == 0){
      return(neighborhood[0, ])
    }
    
    neighborhood %>%
      filter(name %in% input$sel_neighborhood)
    
  }) %>% bindEvent({ input$sel_neighborhood })  # Updates with map type selection
  
  # Income Levels
  income_bos = reactive({
    if(is.null(input$sel_incomeBracket) || length(input$sel_incomeBracket) == 0){
      return(income_data[0, ])
    }
    income_data %>%
      filter(income_category %in% input$sel_incomeBracket)
    
  }) %>% bindEvent({ input$sel_incomeBracket})  # Updates with map type selection
  
  # Calculate total population density for selected income brackets
  selected_pop_density_income = reactive({
    income_bos() %>%
      st_drop_geometry() %>%
      summarize(total_density = sum(pop_density_2020_smooth5, na.rm = TRUE)) %>%
      pull(total_density)
  }) %>% bindEvent({ input$sel_incomeBracket })
  
  
  # Calculate the percentage of total Boston population density for selected income brackets
  selected_income_percentage = reactive({
    selected_density <- selected_pop_density_income()
    # print(paste("Selected Density:", selected_density)) # Add this
    # print(paste("Total Density:", total_boston_pop_density)) # Add this
    
    if (total_boston_pop_density == 0) {
      return("N/A") # Avoid division by zero
    }
    percentage <- (selected_density / total_boston_pop_density) * 100
    # print(paste("Percentage:", percentage)) # Add this
    paste0(round(percentage, 2), "% of Boston Pop. Density")
  }) %>% bindEvent({ input$sel_incomeBracket })
  
  
  # Reactive text for density plot
  recent_selection <- reactiveVal()
  
  observeEvent(input$sel_neighborhood,{
    selected <- input$sel_neighborhood
    if(length(selected) > 1){
      recent_selection(tail(selected, 1))
    } else {
      recent_selection(selected)
    }
  })
  
  output$neighborhood_stats <- renderText({
    
    selected <- recent_selection()
    text_data <- dens_neigh %>% filter(name == selected)
    
    if(selected != "None"){
    paste0("Let's take a closer look at ", selected, "! \n It has an area of ", round(text_data$area, 2), " square kilometers and contains a total count of ", text_data$n, " Blue Bike Stations. \n Therefore, it has a density of ", round(text_data$density,2), " stations per km^2.") 
    } else {
      paste0("Please select a neighborhood using the checkboxes on the side. All selections will be highlighted on the map \n and data for your most recent selection will be shown here. Have fun!")
    }
    
  }) %>% bindEvent ({ input$sel_neighborhood })
  
  
  
  # Density Value Boxes
  output$dens_low <- renderValueBox({
    
    bslib::value_box(
      title = "Minimum Density per Neighborhood",
      value = paste0(round(min_dens$density, 2), " stations/km^2"),
      theme = "lightblue",
      class = "border")
    
  })
  
  output$dens_high <- renderValueBox({
    
    bslib::value_box(
      title = "Maximum Density per Neighborhood",
      value = paste0(round(max_dens$density, 2), " stations/km^2"),
      theme = "#A84268",
      class = "border")
    
  })
  
  # Income Value Boxes
  output$income_low <- renderValueBox({
    
    p <- round(pull(low_income,percentage), 2)
    avg <- round(pull(low_income, avg_low), 2)
    
    bslib::value_box(
      title = paste("The Average Annual Low-Income Earnings are: ", avg, " x $100,000"),
      value = paste0("Low Annual Income (< $60,000) Percentage: ", p, " of Boston's Census Blocks"),
      theme = bslib::value_box_theme(bg = "lightblue"),
      class = "border")
  })
  
  output$income_high <- renderValueBox({
    
    p <- round(pull(high_income,percentage), 2)
    avg <- round(pull(high_income, avg_high), 2)
    
    bslib::value_box(
      title = paste("The Average Annual High-Income Earnings are: ", avg, " x $100,000"),
      value = paste0("High Annual Income (> $60,000) Percentage: ", p, " of Boston's Census Blocks"),
      theme = bslib::value_box_theme(bg = "#A84268"),
      class = "border")
  })
  
  # New value box for total population density by income bracket
  output$pop_density_income <- renderValueBox({
    total_density <- selected_pop_density_income() # Use selected_pop_density_income()
    
    # Create a dynamic title based on selected income brackets
    selected_brackets <- input$sel_incomeBracket
    title_text <- if (is.null(selected_brackets) || length(selected_brackets) == 0) {
      "Total Population Density"
    } else if (length(selected_brackets) == length(unique(income_data$income_category))) {
      "Total Population Density (All Brackets)"
    } else {
      # Map selected bracket codes to human-readable labels
      bracket_labels <- c(
        "pop_0_40000_2019" = "$0 - $40k",
        "pop_40001_60000_2019" = "$40k - $60k",
        "pop_60001_100000_2019" = "$60k - $100k",
        "pop_100000_plus_2019" = "$100k+",
        "Below Threshold" = "Less than 50% in any income category"
      )
      selected_labels <- bracket_labels[selected_brackets]
      paste("Total Pop. Density (", paste(selected_labels, collapse = ", "), ")", sep = "")
    }
    
    
    bslib::value_box(
      title = title_text,
      value = paste0(round(total_density, 2), " people/km²"),
      theme = bslib::value_box_theme(bg = "lightgreen"),
      class = "border"
    )
  }) %>% bindEvent({ input$sel_incomeBracket })
  
  # Reactive text output for selected income percentage
  output$selected_income_percentage <- renderText({
    selected_income_percentage()
  }) %>% bindEvent({ input$sel_incomeBracket })
  
  # New value box for total Boston population density
  output$total_boston_pop_density_box <- renderValueBox({
    bslib::value_box(
      title = "Total Boston Pop. Density",
      value = paste0(round(total_boston_pop_density, 2), " people/km²"),
      theme = bslib::value_box_theme(bg = "orange"),
      class = "border"
    )
  })
  
  
  output$density_plot <- renderPlot({
    
    base_plot_d <-ggplot() +
      geom_sf(data = dens_neigh, aes(fill = density), color = "black", size = 0) +  #Plot Neighborhoods
      geom_sf(data = blocks_bos, fill = NA, color = "white", size = 0.4) +      # Plot all census blocks in Boston
      geom_sf(data = neighborhood, fill = NA, color = "black", size = 0.5) +   # Plot neighborhood boundaries
      scale_fill_gradientn(
        colors = c("lightblue", "#FCB97D", "#A84268"),
        labels = scales::label_number(suffix =" Stations/km^2"),
        name = "Density")
    
    # Only add highlight layer if geometry is present
    neigh_data <- show_neigh()
    if (!is.null(neigh_data) && nrow(neigh_data) > 0 && !all(st_is_empty(neigh_data))) {
      base_plot_d <- base_plot_d + geom_sf(data = neigh_data, fill = NA, color = "red", linewidth = 1.5, show.legend = FALSE)
    }
    
    
    if(input$select_bikes == TRUE){   ### input$select_bikes
      base_plot_d <- base_plot_d + geom_sf(data = sta_in, color = "#241571", size = 1.75, alpha = 0.5)} # Plot bike stations
    
    
    main_plot_d <- base_plot_d +
      coord_sf( xlim = c(-71.20, -70.90),
                ylim = c(42.20, 42.42)) +
      labs(title = "Location and Density of Blue Bike Stations in Boston Neighborhoods",
           subtitle = "Data Source: 'Station_bg.rdg' by T. Fraser, et. all.",
           x = "Longitude",
           y = "Latitude")+
      annotation_scale(location = "bl")+
      annotation_north_arrow(location = "bl", which_north = "true",
                             style = north_arrow_fancy_orienteering,
                             pad_x = unit(0.08, "npc"),
                             pad_y = unit(0.07, "npc")) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.justification = c(0, 1),
        legend.position = c(.6, 0.4),
        legend.background = element_rect(fill = NA, color = NA)
      )
    return(main_plot_d)
    
  }) %>% bindEvent({ input$select_bikes ; input$sel_neighborhood })
  
  
  
  output$income_plot <- renderPlot({          ### Income graph
    
    income_colors <- c(
      "pop_0_40000_2019" = "#DC267F",
      "pop_40001_60000_2019" = "purple",
      "pop_60001_100000_2019" = "green",
      "pop_100000_plus_2019" = "steelblue2",
      "Below Threshold" = "wheat")
    
    # Plot them
    base_plot_i <- ggplot() +
      geom_sf(data = income_bos(), aes(fill = income_category), color = NA, linewidth = 0.1, alpha = 0.5) +  #census group
      geom_sf(data = blocks_bos, fill = NA, color = "black", linewidth = 0.2) + # Outline of smaller areas
      geom_sf(data = boston, fill = NA, color = "black", alpha = 0.8, linewidth = 1)  # Suffolk outline
    
    if(input$select_bikes_income == TRUE){ # Changed ID
      base_plot_i <- base_plot_i + geom_sf(data = sta_in, color = "#241571", size = 1.75, alpha = 0.5)} # Plot bike stations
    
    main_plot_i <- base_plot_i +
      # Use scale_fill_manual for the income categories
      scale_fill_manual(values = income_colors,
                        name = "Dominant Income\nCategory (> 50%)",
                        # Add labels for the legend keys
                        labels = c(
                          "pop_0_40000_2019" = "$0 - $40k",
                          "pop_40001_60000_2019" = "$40k - $60k",
                          "pop_60001_100000_2019" = "$60k - $100k",
                          "pop_100000_plus_2019" = "$100k+",
                          "Below Threshold" = "Less than 50% in any income category"
                        ),
                        # Ensure all categories appear in the legend
                        breaks = names(income_colors)) +
      scale_color_manual(values = c("Bike Station" = "black",shape=2,size = 2 ), name = "Legend:",
                         guide = guide_legend(override.aes = list(linetype = c("blank"), shape = c(16)))) +
      # Manually set color and legend title for bike stations
      labs(
        title = "Boston Bike Sharing Map by Dominant Income Category",
        x = "Longitude",
        y = "Latitude"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 8), # Adjust size for x-axis text
        axis.text.y = element_text(size = 8), # Adjust size for y-axis text
        legend.box = "vertical", # Arrange multiple legends vertically
        legend.justification = c(0, 1),
        legend.position = c(.6, 0.35)
        
      ) +
      annotation_scale(location = "br", style = "tick", text_cex=0.7, height = unit(0.5, "cm")) # Adds a scale bar
    
    return(main_plot_i)
    
  }) %>% bindEvent({ input$sel_incomeBracket; input$select_bikes_income}) # Changed ID
  
  
  
  
  output$bar_chart <- renderPlotly({
    
    bar <- ggplot() +
      geom_col(data = dens_neigh, aes(x = reorder(name, density), y = density, fill = density)) +
      geom_text(data = dens_neigh, aes(x = reorder(name, density),y = density, label = round(density,2)),
                angle = 45, hjust = 0, size = 3.0)+
      scale_fill_gradientn(
        colors = c("lightblue", "#FCB97D", "#A84268"))+
      labs(title = "Neighborhood Ranking by Density",
           y = "Density (Stations/km^2)") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.background = element_blank(),
            panel.grid = element_blank())
    ggplotly(bar, tooltip = "label")
    
  })
  
  
}
# Run app
shiny::shinyApp(ui = ui, server = server, onStart = global)
```
