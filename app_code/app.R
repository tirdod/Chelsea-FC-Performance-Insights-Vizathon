Sys.setlocale("LC_ALL", "en_US.UTF-8")

library(shiny)
library(bs4Dash)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(hms)
library(stringr)
library(ggimage)
library(scales)
library(zoo)
library(ggradar)
library(patchwork) 

# Helper for parsing HR zones
parse_hr_zone <- function(x) {
  sapply(x, function(x_i) {
    x_i <- as.character(x_i)
    tryCatch({
      if (str_detect(x_i, ":")) {
        as.numeric(as_hms(x_i))
      } else {
        as.numeric(trimws(x_i))
      }
    }, error = function(e) NA_real_)
  })
}

dynamic_date_scale <- function(dates) {
  # Ensure dates are Date objects
  dates <- as.Date(dates)
  min_date <- min(dates, na.rm = TRUE)
  max_date <- max(dates, na.rm = TRUE)
  total_days <- as.numeric(max_date - min_date) + 1
  
  if(total_days <= 30) {
    # Use daily breaks if range is 30 days or less.
    scale_x_date(
      breaks = seq(min_date, max_date, by = "1 day"),
      date_labels = "%b\n%d"
    )
  } else if(total_days <= 90) {
    # Use weekly breaks for ranges up to ~3 months.
    scale_x_date(
      breaks = seq(min_date, max_date, by = "1 week"),
      date_labels = "%b\n%d"
    )
  } else {
    # For longer ranges, first try monthly ticks.
    breaks_seq <- seq(min_date, max_date, by = "1 month")
    if (length(breaks_seq) < 5) {
      # If fewer than 5 ticks result from monthly breaks, use weekly ticks.
      scale_x_date(
        breaks = seq(min_date, max_date, by = "1 week"),
        date_labels = "%b\n%d"
      )
    } else {
      scale_x_date(
        breaks = breaks_seq,
        date_labels = "%b %Y"
      )
    }
  }
}

player_info <- list(
  player_name = 'Mystery Player',
  birthdate = as.Date("1999-05-11"),
  height_cm = 187,
  weight_kg = 75,
  position = "Central Midfield (CM)",
  club = "Chelsea FC (2021/22 - Present)",
  club_history = "Chelsea FC Academy (2015/16 - 2021/22)",
  agent = 'Jorge Mendes',
  agent_phone = "+44 7911 123456",  # ← NEW FIELD
  weekly_salary = "£65,000 p/a",
  internal_model_market_value = "£100,000,000"
)

age_years <- as.numeric(difftime(Sys.Date(), player_info$birthdate, units = "days")) / 365.25
age_string <- floor(age_years * 10) / 10

clean_id <- function(x) {
  # Lowercase the string and replace one or more whitespace characters with an underscore
  tolower(gsub("[[:space:]]+", "_", x))
}

to_title_movement <- function(x) {
  tools::toTitleCase(gsub("_", " ", x))
}

ui <- bs4DashPage(
  title = "Chelsea Performance Dashboard",
  
  header = bs4DashNavbar(
    skin = "light",
    status = "primary",
    leftUi = tagList(),
    rightUi = tagList(),
    title = tags$div(
      class = "d-flex align-items-center",
      style = "gap: 20px; height: 80px;",   # Increase the height
      tags$img(
        src = "assets/img/logo/logo.svg",
        style = "height: 65px; width: auto;"  # Larger logo
      ),
      tags$span(
        "Chelsea Performance Dashboard",
        style = "font-size: 28px; font-weight: 800; color: white;"
      )
    )
  ),
  
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "assets/css/chelsea-font-face.css"),
      tags$link(rel = "stylesheet", href = "assets/css/LineIcons.2.0.css"),
      tags$style(HTML("
        /* Chelsea font override */
        body, h1, h2, h3, h4, h5, h6, .value-box, .card-title, .form-control {
          font-family: 'CFCSerif', sans-serif !important;
        }
      
        /* Reduce box height and center content */
        .small-box {
          height: 70px !important;
          padding: 8px 12px !important;
          display: flex !important;
          flex-direction: row !important;
          justify-content: space-between !important;
          align-items: center !important;
        }
      
        .small-box h3 {
          font-size: 20px !important;
          margin: 0 0 4px 0 !important;
        }
      
        .small-box p {
          font-size: 14px !important;
          margin: 0 !important;
        }
      
        /* ICON: clean vertical center */
        .small-box .icon {
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          height: 100% !important;
          font-size: 36px !important;
          opacity: 0.2 !important;
          margin: 0 !important;
          padding: 0 !important;
        }
      
        .small-box .icon > i {
          line-height: 1 !important;
          margin: 0 !important;
          padding: 0 !important;
        }
        
        /* Table row & cell styling */
        table.dataTable tbody td {
          vertical-align: middle;
        }
        table.dataTable {
          width: 100% !important;
          margin: 0 auto !important;
        }
      
        /* Badge styling for Tracking Status */
        .status-ontrack {
          background-color: #28a745 !important;  /* green */
          color: white !important;
          padding: 4px 8px;
          border-radius: 12px;
          font-weight: 500;
          display: inline-block;
        }
      
        .status-achieved {
          background-color: #007bff !important;  /* blue */
          color: white !important;
          padding: 4px 8px;
          border-radius: 12px;
          font-weight: 500;
          display: inline-block;
        }
      
        .status-at-risk {
          background-color: #ffc107 !important;  /* yellow */
          color: black !important;
          padding: 4px 8px;
          border-radius: 12px;
          font-weight: 500;
          display: inline-block;
        }
      
        .status-behind {
          background-color: #dc3545 !important;  /* red */
          color: white !important;
          padding: 4px 8px;
          border-radius: 12px;
          font-weight: 500;
          display: inline-block;
        }
        
          /* Smaller, centered bs4Card title headers */
          .card .card-header {
            height: 75px !important;
            display: flex !important;
            align-items: center !important;
            padding-left: 15px !important;
          }
          
          /* Slightly smaller and bold title text */
          .card-title {
            font-size: 20px !important;
            font-weight: 600 !important;
            margin: 0 !important;
            padding: 0 !important;
          }
      "))
    ),
    
    fluidRow(
      bs4Card(
        title = "Player Profile",
        width = 12,
        solidHeader = TRUE,
        collapsible = FALSE,  # ← remove the "-"
        status = "primary",
        fluidRow(
          column(
            width = 12,
            tags$div(
              class = "d-flex align-items-center",
              style = "gap: 20px;",
              tags$img(
                src = "assets/img/team/team-1/team-tbd.png",
                style = "border-radius: 50%; height: 100px; width: 100px; object-fit: cover;"
              ),
              tags$div(
                class = "d-flex",
                style = "gap: 40px;",
                
                # Column 1: Age, Birthdate, Height, Weight
                tags$div(
                  tags$p(HTML(paste0("<strong>Player Name:</strong> ", player_info$player_name)), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Age:</strong> ", age_string)), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Birthdate:</strong> ", format(player_info$birthdate, "%B %d, %Y"))), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Height:</strong> ", player_info$height_cm, " cm")), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Weight:</strong> ", player_info$weight_kg, " kg")), style = "margin: 0;")
                ),
                
                # Column 2: Position, Club
                tags$div(
                  tags$p(HTML(paste0("<strong>Position:</strong> ", player_info$position)), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Club:</strong> ", player_info$club)), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Club History:</strong> ", player_info$club_history)), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Agent:</strong> ", player_info$agent, " (", player_info$agent_phone, ")")), style = "margin: 0;"),
                  tags$p(HTML(paste0("<strong>Weekly Wage:</strong> ", player_info$weekly_salary)), style = "margin: 0;")
                )
              )
            )
          )
        )

      )
    ),
    
    fluidRow(
      bs4TabCard(
        id = "insight_tabs",
        title = "Performance Insights",
        side = "right",        # Puts tabs on the top-right
        type = "tabs",         # Style: tabs
        status = "primary",    # Header color
        collapsible = FALSE,  # ← remove the "-"
        width = 12,
        
        # --- Tab 1: Load & Movement ---
        tabPanel("Load & Movement", 
                 
                 fluidRow(
                   style = "margin-top: -10px; margin-bottom: 10px;",
                   column(width = 3, selectInput("season", "Season:", choices = NULL)),
                   column(width = 3, selectInput("time_range", "Time Range:", choices = NULL)),
                   column(width = 3, uiOutput("custom_date_ui")),
                   column(width = 3, selectInput("session_type", "Session Type:",
                                                 choices = c("All", "Match", "Training")))
                 ),
                 
                 fluidRow(
                   bs4ValueBoxOutput("total_distance"),
                   bs4ValueBoxOutput("total_day_duration"),
                   bs4ValueBoxOutput("peak_speed")
                 ),
                 fluidRow(
                   bs4ValueBoxOutput("high_speed_distance"),     # >21 km/h
                   bs4ValueBoxOutput("very_high_speed_distance"),# >24 km/h
                   bs4ValueBoxOutput("sprint_speed_distance")    # >27 km/h
                 ),
                 fluidRow(
                      column(width = 12, plotlyOutput("load_trend"))
                    ),
                 tags$div(style = "margin-top: 10px;"),  # ← Add whitespace above
                 fluidRow(
                      column(width = 6, plotlyOutput("accel_plot")),
                      column(width = 6, plotlyOutput("hr_zone_plot"))
                    )
        ),
        
        # --- Tab 2: Player Goals ---
        tabPanel("Player Priorities",
                 fluidRow(
                   style = "margin-top: -10px; margin-bottom: 10px;",
                   bs4Card(
                     title = "Player Priority Summary",
                     width = 12,
                     collapsible = FALSE,  # ← remove the "-"
                     status = "primary",
                     solidHeader = TRUE,
                     DT::dataTableOutput("priority_table")
                   )
                 ),
                 fluidRow(
                   bs4Card(
                     title = "Coach Reports",
                     width = 12,
                     collapsible = FALSE,  # ← remove the "-"
                     status = "primary",
                     solidHeader = TRUE,
                     uiOutput("coach_reports")
                   )
                 )
        ),
        
        # --- Tab 3: Physical Capability ---
        # Corrected Physical Capability tab UI without direct 'mvmt' references
        # --- Physical Capability Tab (UI) ---
        tabPanel("Physical Capability",
                 # Title row before the two-column row
                 fluidRow(
                   column(
                     width = 12,
                     tags$h3("Physical Capability Trends Over Past Three Months", style = "margin-bottom: 15px;")
                   )
                 ),
                 fluidRow(
                   # Right: Positive Trends
                   column(
                     width = 6,
                     tags$div(
                       style = "border: 1px solid #28a745; border-radius: 8px; padding: 15px; margin-bottom: 15px; background-color: #f3fdf5;",
                       tags$h4("Positive Trends", style = "color: #28a745; font-weight: 600; margin-bottom: 15px;"),
                       uiOutput("positive_trend_list")
                     )
                   ),
                   # Left: Negative Trends
                   column(
                     width = 6,
                     tags$div(
                       style = "border: 1px solid #dc3545; border-radius: 8px; padding: 15px; margin-bottom: 15px; background-color: #fef2f2;",
                       tags$h4("Negative Trends", style = "color: #dc3545; font-weight: 600; margin-bottom: 15px;"),
                       uiOutput("negative_trend_list")
                     )
                   )
              ),
                 # first we include the radar plot snapshot
                     fluidRow(
                       column(
                         width = 12,
                         tags$h3("Most Recent Physical Tests", style = "margin-bottom: 15px;")
                       )
                     ),
                     fluidRow(
                       column(width = 3,
                              tags$h3(align = "center", style = "margin-bottom: 10px;"),
                              plotlyOutput("radar_Agility", height = "400px")
                       )
                   ,
                   column(width = 3,
                          tags$h3(align = "center", style = "margin-bottom: 10px;"),
                          plotlyOutput("radar_Jump", height = "400px")
                   )
                   ,
                   column(width = 3,
                          tags$h3(align = "center", style = "margin-bottom: 10px;"),
                          plotlyOutput("radar_Sprint", height = "400px")
                   )
                   ,
                   column(width = 3,
                          tags$h3(align = "center", style = "margin-bottom: 10px;"),
                          plotlyOutput("radar_UpperBody", height = "400px")
                   )
                 )
                 ,
                 fluidRow(
                   column(
                     width = 12,
                     tags$h3("Physical Capability History", style = "margin-bottom: 15px;")
                   )
                 ),
                  # Date Range Input row (applies to all eight plots)
                  fluidRow(
                    column(
                      width = 3,
                      dateRangeInput(
                        "pcap_date_range",
                        "Select Date Range:",
                        start = Sys.Date() - 180,  # Temporary default; will be updated in server
                        end   = Sys.Date()
                      )
                    )
                  ),
                 # Then include your eight plot placeholders (for example):
                 fluidRow(
                   column(width = 6, plotlyOutput("trend_agility_isometric", height = "400px")),
                   column(width = 6, plotlyOutput("trend_agility_dynamic", height = "400px"))
                 ),
                 fluidRow(
                   column(width = 6, plotlyOutput("trend_jump_isometric", height = "400px")),
                   column(width = 6, plotlyOutput("trend_jump_dynamic", height = "400px"))
                 ),
                 fluidRow(
                   column(width = 6, plotlyOutput("trend_sprint_isometric", height = "400px")),
                   column(width = 6, plotlyOutput("trend_sprint_dynamic", height = "400px"))
                 ),
                 fluidRow(
                   column(width = 6, plotlyOutput("trend_upper_body_isometric", height = "400px")),
                   column(width = 6, plotlyOutput("trend_upper_body_dynamic", height = "400px"))
                 ))
                 
        # ,
        # 
        # # --- Tab 4: Recovery ---
        # tabPanel("Recovery",
        #             fluidRow(
        #               column(width = 12, plotlyOutput("recovery_plot")) # Placeholder for now
        #             )
        #     
        #   )
        )
      )  
    )
)

server <- function(input, output, session) {
  
  gps_data <- reactive({
    read_csv("DATA/CFC GPS Data.csv",
             locale = locale(encoding = "Latin1")) %>%
      mutate(date = lubridate::dmy(date)) %>%
      mutate(across(starts_with("hr_zone"), ~ parse_hr_zone(.))) %>%
      mutate(session_type = case_when(
        distance == 0 ~ "Recovery",
        !is.na(opposition_code) ~ "Match",
        TRUE ~ "Training"
      ))
  })
  
  individual_priority_data <- reactive({
    read_csv("DATA/CFC Individual Priority Areas.csv",
             locale = locale(encoding = "Latin1"))
  })
  
  physical_data <- reactive({
    read_csv("DATA//CFC Physical Capability Data_.csv", 
             locale = locale(encoding = "Latin1")) %>%
      rename(testDate = 1) %>%
      mutate(
        testDate = dmy(testDate),
        benchmarkPct = as.numeric(benchmarkPct),
        movement = str_replace_all(movement, " ", "_")  # Convert 'upper body' to 'upper_body'
      ) %>%
      filter(!is.na(benchmarkPct))
  })
  
  recovery_data <- reactive({
    read_csv("DATA/CFC Recovery status Data.csv",
             locale = locale(encoding = "Latin1"))
  })
  
  observe({
    data <- gps_data()
    seasons <- sort(unique(data$season), decreasing = TRUE)
    updateSelectInput(session, "season", choices = seasons, selected = seasons[1])
  })
  
  observeEvent(input$season, {
    req(input$season)
    data <- gps_data()
    season_data <- data %>% filter(season == input$season)
    if (nrow(season_data) == 0) return()
    
    latest_season <- max(data$season)
    
    choices <- if (input$season == latest_season) {
      c(
        "Full Season" = "full",
        "Last Session" = "1s",
        "Last 5 Sessions" = "5s",
        "Last 10 Sessions" = "10s",
        "Last 25 Sessions" = "25s",
        "Custom Session Range" = "custom_sessions",
        "Last 7 Days" = "7d",
        "Last 14 Days" = "14d",
        "Last 30 Days" = "30d",
        "Custom Date Range" = "custom"
      )
    } else {
      c("Full Season" = "full", "Custom Date Range" = "custom")
    }
    
    updateSelectInput(session, "time_range", choices = choices, selected = "full")
  }, ignoreNULL = FALSE)
  
  output$custom_date_ui <- renderUI({
    req(input$season)
    season_data <- gps_data() %>% filter(season == input$season)
    min_date <- min(season_data$date, na.rm = TRUE)
    max_date <- max(season_data$date, na.rm = TRUE)
    
    if (input$time_range == "custom") {
      dateRangeInput("custom_range", "Custom Date Range:", start = min_date, end = max_date, min = min_date, max = max_date)
    } else if (input$time_range == "custom_sessions") {
      numericInput("custom_session_n", "Number of Sessions:", min = 1, value = 5, step = 1)
    }
  })
  
  date_window <- reactive({
    req(input$season, input$time_range)
    
    season_data <- gps_data() %>%
      filter(season == input$season) %>%
      filter(!is.na(distance), distance > 0) %>%   # 🛠️ Exclude sessions with 0 or NA distance
      arrange(desc(date))
    
    if (input$session_type != "All") {
      season_data <- season_data %>% filter(session_type == input$session_type)
    }
    
    available_dates <- season_data$date
    
    n_sessions <- switch(
      input$time_range,
      "1s" = 1,
      "5s" = 5,
      "10s" = 10,
      "25s" = 25,
      "custom_sessions" = req(input$custom_session_n),
      NA
    )
    
    if (!is.na(n_sessions)) {
      last_n_dates <- head(available_dates, n_sessions)
      return(range(last_n_dates))
    }
    
    # Fallback for date-based options
    full_season_data <- gps_data() %>%
      filter(season == input$season)
    
    max_date <- max(full_season_data$date, na.rm = TRUE)
    min_date <- min(full_season_data$date, na.rm = TRUE)
    
    switch(input$time_range,
           "7d" = c(max_date - 6, max_date),
           "14d" = c(max_date - 13, max_date),
           "30d" = c(max_date - 29, max_date),
           "custom" = req(input$custom_range),
           c(min_date, max_date)
    )
  })
  
  filtered_data <- reactive({
    req(date_window())
    
    df <- gps_data() %>%
      filter(season == input$season) %>%
      filter(date >= date_window()[1], date <= date_window()[2])
    
    # Apply session type filter (match/training)
    if (input$session_type != "All") {
      df <- df %>% filter(session_type == input$session_type)
    }
    
    # Handle last N sessions logic — exclude 0-distance sessions BEFORE slicing
    if (input$time_range %in% c("1s", "5s", "10s", "25s", "custom_sessions")) {
      df <- df %>%
        filter(!is.na(distance), distance > 0) %>%
        arrange(desc(date))
      
      n_sessions <- switch(input$time_range,
                           "1s" = 1,
                           "5s" = 5,
                           "10s" = 10,
                           "25s" = 25,
                           "custom_sessions" = input$custom_session_n)
      
      df <- df %>% slice_head(n = n_sessions) %>% arrange(date)  # Re-sort chronologically
    }
    
    df
  })
  
  output$total_distance <- renderValueBox({
    df <- filtered_data()
    val <- sum(df$distance, na.rm = TRUE)
    formatted_val <- formatC(val, format = "d", big.mark = ",")
    valueBox(paste0(formatted_val, " meters"), "Total Distance", icon = icon("road"), color = "primary")
  })
  
  output$total_day_duration <- renderValueBox({
    df <- filtered_data()  # the filtered data for the selected date range and session type
    val <- sum(df$day_duration, na.rm = TRUE)
    
    # Format the duration as before
    duration_str <- paste0(
      formatC(floor(val), format = "d", big.mark = ","),
      " minutes"
    )
    
    # Count the number of Matches and Trainings in the filtered data
    n_matches   <- sum(df$session_type == "Match", na.rm = TRUE)
    n_trainings <- sum(df$session_type == "Training", na.rm = TRUE)
    
    # Create singular/plural labels
    match_label   <- if (n_matches == 1) "Match" else "Matches"
    training_label <- if (n_trainings == 1) "Training" else "Trainings"
    
    # Build a custom label based on the session_type filter
    label_str <- ""
    if (input$session_type == "All") {
      label_str <- paste0(
        "Total Session Duration (",
        n_matches, " ", match_label, ", ",
        n_trainings, " ", training_label, ")"
      )
    } else if (input$session_type == "Match") {
      label_str <- paste0(
        "Total Session Duration (",
        n_matches, " ", match_label, ")"
      )
    } else if (input$session_type == "Training") {
      label_str <- paste0(
        "Total Session Duration (",
        n_trainings, " ", training_label, ")"
      )
    } else {
      label_str <- "Total Session Duration"
    }
    
    valueBox(
      duration_str,
      label_str,
      icon  = icon("clock"),
      color = "purple"
    )
  })
  
  output$peak_speed <- renderValueBox({
    df <- filtered_data()
    val <- max(df$peak_speed, na.rm = TRUE)
    display <- ifelse(is.infinite(val), "No Data", paste0(round(val, 1), " km/h"))
    valueBox(display, "Peak Speed", icon = icon("bolt"), color = "warning")
  })
  
  output$high_speed_distance <- renderValueBox({
    df <- filtered_data()
    val <- sum(df$distance_over_21, na.rm = TRUE)
    formatted_val <- paste0(formatC(floor(val), format = "d", big.mark = ","), " meters")
    valueBox(formatted_val, "High Speed Distance (Speed faster than 21km/h)", icon = icon("tachometer-alt"), color = "info")
  })
  
  output$very_high_speed_distance <- renderValueBox({
    df <- filtered_data()
    val <- sum(df$distance_over_24, na.rm = TRUE)
    formatted_val <- paste0(formatC(floor(val), format = "d", big.mark = ","), " meters")
    valueBox(formatted_val, "Very High Speed Distance (Speed faster than 24km/h)", icon = icon("tachometer-alt"), color = "teal")
  })
  
  output$sprint_speed_distance <- renderValueBox({
    df <- filtered_data()
    val <- sum(df$distance_over_27, na.rm = TRUE)
    formatted_val <- paste0(formatC(floor(val), format = "d", big.mark = ","), " meters")
    valueBox(formatted_val, "Sprint Speed Distance (Speed faster than 27km/h)", icon = icon("tachometer-alt"), color = "success")
  })
  
  output$load_trend <- renderPlotly({
    df <- filtered_data() %>%
      arrange(date) %>%
      mutate(
        hover_text = case_when(
          session_type == "Match" ~ paste0(
            "Match: ", coalesce(opposition_full, "Unknown"),
            "<br>Date: ", as.character(date),
            "<br>Total Distance: ", formatC(distance, format = "d", big.mark = ","),
            " m<br>High Speed Distance: ", formatC(distance_over_21, format = "d", big.mark = ","),
            " m<br>Very High Speed Distance: ", formatC(distance_over_24, format = "d", big.mark = ","),
            " m<br>Sprint Speed Distance: ", formatC(distance_over_27, format = "d", big.mark = ","),
            " m"
          ),
          session_type == "Training" ~ paste0(
            "Training",
            "<br>Date: ", as.character(date),
            "<br>Total Distance: ", formatC(distance, format = "d", big.mark = ","),
            " m<br>High Speed Distance: ", formatC(distance_over_21, format = "d", big.mark = ","),
            " m<br>Very High Speed Distance: ", formatC(distance_over_24, format = "d", big.mark = ","),
            " m<br>Sprint Speed Distance: ", formatC(distance_over_27, format = "d", big.mark = ","),
            " m"
          ),
          TRUE ~ paste0(
            "Recovery",
            "<br>Date: ", as.character(date),
            "<br>Total Distance: ", formatC(distance, format = "d", big.mark = ","),
            " m<br>High Speed Distance: ", formatC(distance_over_21, format = "d", big.mark = ","),
            " m<br>Very High Speed Distance: ", formatC(distance_over_24, format = "d", big.mark = ","),
            " m<br>Sprint Speed Distance: ", formatC(distance_over_27, format = "d", big.mark = ","),
            " m"
          )
        )
      )
    
    req(nrow(df) > 0)
    
    # Generate the breaks for the x-axis
    date_range <- unique(format(df$date, "%Y-%m"))  # Get unique month-year combinations
    breaks <- seq(from = min(df$date), to = max(df$date), by = "month")  # Ensure monthly breaks
    
    p <- ggplot(df, aes(x = date, y = distance, group = 1, text = hover_text)) +
      geom_line(color = "#00225A", linewidth = 0.8) +
      geom_point(color = "#00225A", size = 2) +
      labs(
        title = "Daily Distance Covered",
        y = "Distance (m)",
        x = "Date"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      dynamic_date_scale(df$date) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),  # Center the labels
        axis.title.x = element_text(margin = margin(t = 10))  # Adjust space for x-axis title
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$accel_plot <- renderPlotly({
    df <- filtered_data() %>%
      arrange(date) %>%
      mutate(duration_minutes = if_else(day_duration > 0, day_duration, NA_real_)) %>%
      mutate(
        count_very_high = round(accel_decel_over_4_5 / duration_minutes, 1),
        count_high = round((accel_decel_over_3_5 - accel_decel_over_4_5) / duration_minutes, 1),
        count_moderate = round((accel_decel_over_2_5 - accel_decel_over_3_5) / duration_minutes, 1)
      ) %>%
      select(date, session_type, opposition_full, count_moderate, count_high, count_very_high) %>%
      pivot_longer(
        cols = starts_with("count_"),
        names_to = "intensity",
        values_to = "rate"
      ) %>%
      mutate(
        intensity_label = case_when(
          intensity == "count_moderate" ~ "2.5–3.4 m/s² (Moderate)",
          intensity == "count_high" ~ "3.5–4.4 m/s² (High)",
          intensity == "count_very_high" ~ ">4.5 m/s² (Very High)"
        ),
        intensity_label = factor(
          intensity_label,
          levels = c(
            "2.5–3.4 m/s² (Moderate)",
            "3.5–4.4 m/s² (High)",
            ">4.5 m/s² (Very High)"
          )
        ),
        hover_text = paste0(
          ifelse(session_type == "Match",
                 paste0("Match: ", coalesce(opposition_full, "Unknown")),
                 session_type),
          "<br>Date: ", as.character(date),
          "<br>Intensity: ", intensity_label,
          "<br>Rate: ", round(rate, 2), " events/min"
        )
      ) %>%
      # 🛠️ NEW: Fill in all combinations of dates and intensity labels
      complete(date = unique(filtered_data()$date),
               intensity_label,
               fill = list(rate = 0, hover_text = ""))
    
    if (nrow(df %>% filter(!is.na(rate))) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No acceleration/deceleration data available"))
    }
    
    p <- ggplot(df, aes(x = date, y = rate, fill = intensity_label, text = hover_text)) +
      geom_col(position = "dodge") +
      labs(
        title = "Acceleration/Deceleration Thresholds",
        subtitle = "Based on speed change thresholds (m/s²)",
        y = "Speed Changes per Minute",
        x = "Date",
        fill = "Threshold"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      dynamic_date_scale(df$date) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10))
      )
    
    ggplotly(p, tooltip = "text") %>% layout(yaxis = list(autorange = TRUE))
  })
  
  output$hr_zone_plot <- renderPlotly({
    df <- filtered_data() %>%
      arrange(date) %>%
      pivot_longer(
        cols = starts_with("hr_zone"),
        names_to = "zone",
        values_to = "seconds"
      ) %>%
      mutate(
        minutes = round(seconds / 60, 1),
        zone_label = case_when(
          zone == "hr_zone_1_hms" ~ "Zone 1 (Very Light)",
          zone == "hr_zone_2_hms" ~ "Zone 2 (Light)",
          zone == "hr_zone_3_hms" ~ "Zone 3 (Moderate)",
          zone == "hr_zone_4_hms" ~ "Zone 4 (Hard)",
          zone == "hr_zone_5_hms" ~ "Zone 5 (Maximum)"
        ),
        hover_text = paste0(
          ifelse(session_type == "Match",
                 paste0("Match: ", coalesce(opposition_full, "Unknown")),
                 session_type),
          "<br>Date: ", as.character(date),
          "<br>Heart Rate Zone: ", zone_label,
          "<br>Minutes: ", round(minutes, 2)
        )
      ) %>%
      complete(date, zone_label, fill = list(minutes = 0, seconds = 0, hover_text = "")) %>%
      mutate(
        zone_label = factor(
          zone_label,
          levels = c(
            "Zone 1 (Very Light)",
            "Zone 2 (Light)",
            "Zone 3 (Moderate)",
            "Zone 4 (Hard)",
            "Zone 5 (Maximum)"
          )
        ),
        minutes = ifelse(is.na(minutes), 0, minutes)
      )
    
    breaks <- seq(from = min(df$date), to = max(df$date), by = "1 day")
    
    p <- ggplot(df, aes(x = date, y = minutes, fill = zone_label, text = hover_text)) +
      geom_col(position = "dodge") +
      labs(
        title = "Heart Rate Zone Breakdown by Session",
        subtitle = "Time spent in each heart rate zone (minutes)",
        y = "Minutes Spent In Each Heart Rate Zone",
        x = "Date",
        fill = "Heart Rate Zone"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      dynamic_date_scale(df$date) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10))
      )
    
    ggplotly(p, tooltip = "text") %>% layout(yaxis = list(autorange = TRUE))
  })
  
  output$priority_table <- DT::renderDataTable({
    df <- individual_priority_data()
    
    # Rename columns
    colnames(df) <- c("Priority #", "Category", "Area", "Target", "Performance Type", "Target Set", "Review Date", "Tracking Status")
    
    # Add HTML badges for status
    df$`Tracking Status` <- case_when(
      tolower(df$`Tracking Status`) == "on track" ~ '<span class="status-ontrack">On Track</span>',
      tolower(df$`Tracking Status`) == "achieved" ~ '<span class="status-achieved">Achieved</span>',
      tolower(df$`Tracking Status`) == "at risk" ~ '<span class="status-at-risk">At Risk</span>',
      tolower(df$`Tracking Status`) == "behind" ~ '<span class="status-behind">Behind</span>',
      TRUE ~ df$`Tracking Status`
    )
    
    DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,  # Allow HTML
      options = list(
        dom = 't',
        ordering = FALSE,
        pageLength = nrow(df),
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact'
    )
    
  })
  
  output$coach_reports <- renderUI({
    fluidRow(
      lapply(list(
        list(
          name = "Enzo Maresca",
          title = "Head Coach",
          img = "assets/img/team/team-2/team-Enzo_Maresca_Head_Coach_1.png",
          text = "Mystery Player has been consistently delivering solid performances in midfield. His vision and positioning have noticeably improved over the past month."
        ),
        list(
          name = "Marcos Alvarez",
          title = "Fitness Coach",
          img = "assets/img/team/team-2/team-Marcos_Alvarez_Fitness_Coach_1.png",
          text = "We’ve seen a 6% increase in high-speed running volume over the last 4 weeks. His sprint deceleration control is trending in the right direction."
        ),
        list(
          name = "Chris Searle",
          title = "Lead 1st Team Sport Scientist",
          img = "assets/img/team/team-2/team-Chris_Searle_Lead_1st_Team_Sports_Scientist.png",
          text = "Average nightly sleep duration is 7.9 hours. We've introduced light-blocking strategies pre-bedtime, and quality has improved accordingly."
        ),
        list(
          name = "Nessan Costello",
          title = "1st Team Sports Nutritionist",
          img = "assets/img/team/team-2/team-Nessan_Costello_1st_Team_Sports_Nutritionist.png",
          text = "Carbohydrate timing and hydration protocols have been well maintained. Recovery nutrition compliance is 100% over the past 14 days."
        )
      ), function(report) {
        column(
          width = 6,
          bs4Card(
            title = tags$div(
              class = "d-flex align-items-center",
              tags$img(
                src = report$img,
                style = "border-radius: 50%; height: 70px; width: 70px; margin-right: 15px; object-fit: cover;"
              ),
              tags$p(
                HTML(paste0(
                  "<span style='font-weight:600;'>", report$title, ":</span> ",
                  "<span style='font-weight:normal;'>", report$name, "</span>"
                )),
                style = "margin: 0; font-size: 18px;"  # ← Increase font size here
              )
            ),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,  # ← remove the "-"
            width = 12,
            tags$p(report$text, style = "margin-top: 10px;")
          )
        )
      })
    )
  })
  
  observe({
    data <- physical_data()
    req(nrow(data) > 0)
    min_date <- min(data$testDate, na.rm = TRUE)
    max_date <- max(data$testDate, na.rm = TRUE)
    updateDateRangeInput(
      session,
      "pcap_date_range",
      start = min_date,
      end   = max_date,
      min   = min_date,
      max   = max_date
    )
  })
  
  pcap_filtered_data <- reactive({
    full_data <- physical_data()
    req(nrow(full_data) > 0, input$pcap_date_range)
    
    # Filter rows using the user-selected date range
    full_data %>%
      filter(testDate >= input$pcap_date_range[1],
             testDate <= input$pcap_date_range[2])
  })
  
  # # KPI Summary: Average benchmarks for Isometric and Dynamic
  # output$avg_isometric <- renderValueBox({
  #   df <- physical_data()
  #   iso_vals <- df %>% filter(tolower(expression) == "isometric") %>% pull(benchmarkPct)
  #   avg_iso <- mean(iso_vals, na.rm = TRUE)
  #   valueBox(
  #     paste0(round(avg_iso * 100, 1), "%"),
  #     "Avg Isometric Benchmark",
  #     icon = icon("heartbeat"),
  #     color = "primary"
  #   )
  # })
  # 
  # output$avg_dynamic <- renderValueBox({
  #   df <- physical_data()
  #   dyn_vals <- df %>% filter(tolower(expression) == "dynamic") %>% pull(benchmarkPct)
  #   avg_dyn <- mean(dyn_vals, na.rm = TRUE)
  #   valueBox(
  #     paste0(round(avg_dyn * 100, 1), "%"),
  #     "Avg Dynamic Benchmark",
  #     icon = icon("running"),
  #     color = "success"
  #   )
  # })
  
  # Compute key insights based on changes in performance over the last 30 days.
  # REVISED INSIGHTS CALCULATION (no fixed date splits)
  
  insights_data <- reactive({
    # Define our helper for title casing movement, expression, and quality if not already done.
    to_title_movement <- function(x) {
      tools::toTitleCase(gsub("_", " ", x))
    }
    
    # Use your filtered physical capability data (pcap_filtered_data)
    df <- pcap_filtered_data() %>%
      mutate(
        # Create a nicely formatted combo by title-casing the expression and quality separately,
        # and then joining them with a newline.
        combo = paste(to_title_movement(expression), to_title_movement(quality), sep = "\n")
      )
    
    # For each movement + combo, compute:
    # - overall average during the selected period ("overall_avg")
    # - the most recent value ("latest_value")
    df_summary <- df %>%
      group_by(movement, combo) %>%
      summarise(
        overall_avg = mean(benchmarkPct, na.rm = TRUE),
        latest_value = mean(benchmarkPct[testDate == max(testDate)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        diff = latest_value - overall_avg,
        Trend = case_when(
          diff > 0.05 ~ "Positive",   # Adjust the threshold as needed.
          diff < -0.05 ~ "Negative",
          TRUE ~ "Stable"
        ),
        Overall_Avg_Pct = scales::percent(overall_avg, accuracy = 1),
        Latest_Pct = scales::percent(latest_value, accuracy = 1),
        Diff_Pct = scales::percent(diff, accuracy = 1)
      )
    
    df_summary
  })
  
  output$negative_trend_list <- renderUI({
    # Subset and sort
    df_neg <- insights_data() %>% 
      filter(Trend == "Negative") %>%
      mutate(
        # Apply to_title_movement on movement (this ensures that e.g. "upper_body" becomes "Upper Body")
        movement = sapply(movement, to_title_movement),
        diff_value = as.numeric(str_remove(Diff_Pct, "[%+]"))
      ) %>%
      arrange(as.numeric(str_remove(Diff_Pct, "[%+]"))) 
    
    if(nrow(df_neg) == 0){
      return(tags$p("No negative trends found!", style = "color: #777;"))
    }
    
    tagList(
      lapply(seq_len(nrow(df_neg)), function(i){
        row <- df_neg[i,]
        tags$li(
          style = "margin-bottom: 5px;",
          HTML(sprintf(
            "<i class='lni lni-arrow-down' style='color: #dc3545;'></i> 
           <strong>%s – %s</strong>: %s → %s (%s)",
            row$movement,    # if needed, you can also apply to_title_movement() here
            row$combo,       # now nicely formatted with a newline between expression and quality
            row$Overall_Avg_Pct,
            row$Latest_Pct,
            row$Diff_Pct
          ))
        )
      })
    )
  })
  
  output$positive_trend_list <- renderUI({
    # Subset the insights data for positive trends
    df_pos <- insights_data() %>%
      filter(Trend == "Positive") %>%     
      mutate(
        # Apply to_title_movement on movement (this ensures that e.g. "upper_body" becomes "Upper Body")
        movement = sapply(movement, to_title_movement),
        diff_value = as.numeric(str_remove(Diff_Pct, "[%+]"))
      ) %>%
      # Sort by the positive difference (convert Diff_Pct to a numeric value without the % sign)
      arrange(desc(as.numeric(str_remove(Diff_Pct, "[%+]"))))
    
    # If no positive trends are found, display a message
    if(nrow(df_pos) == 0){
      return(tags$p("No positive trends found!", style = "color: #777;"))
    }
    
    # Build a bullet list with each positive trend as a list item
    tagList(
      lapply(seq_len(nrow(df_pos)), function(i) {
        row <- df_pos[i,]
        tags$li(
          style = "margin-bottom: 5px;",
          HTML(sprintf(
            "<i class='lni lni-arrow-up' style='color: #28a745;'></i> 
           <strong>%s – %s</strong>: %s → %s (+%s)",
            row$movement,        #/* Already nicely formatted, e.g., "Upper Body" */
              row$combo,           #/* e.g., "Isometric<br>Acceleration" if you inserted a newline in insights_data() */
              row$Overall_Avg_Pct, #/* Overall average in percent, like "102%" */
              row$Latest_Pct,      #/* Latest test percentage, like "108%" */
              row$Diff_Pct         #/* The difference, e.g., "6%" */
          ))
        )
      })
    )
  })
  
  # Dynamic generation of physical capability trend plots for each movement and expression
  observe({
    # Use filtered data if available, else use full physical_data()
    df_all <- pcap_filtered_data()  # or physical_data() if no filtering is implemented
    req(nrow(df_all) > 0)
    
    movements <- unique(df_all$movement)
    expressions <- c("isometric", "dynamic")
    
    for (mvmt in movements) {
      for (expr in expressions) {
        local({
          current_mv <- mvmt
          current_expr <- expr
          output_id <- paste0("trend_", clean_id(current_mv), "_", current_expr)
          
          output[[output_id]] <- renderPlotly({
            # 1. Filter & Prepare Data
            # Filter for the current movement/expression, then clean quality:
            df_sub <- df_all %>%
              filter(
                tolower(movement) == tolower(current_mv),
                tolower(expression) == tolower(current_expr)
              ) %>%
              mutate(quality = stringr::str_to_title(trimws(quality))) %>%
              # Merge "Land" and "Take Off" into a single quality "Take-Off / Land"
              mutate(quality = case_when(
                quality %in% c("Land", "Take Off") ~ "Take-Off / Land",
                TRUE ~ quality
              ))
            
            req(nrow(df_sub) > 0)
            
            # 2. Identify the most recent test result (raw) for each quality:
            df_most <- df_sub %>%
              group_by(quality) %>%
              filter(testDate == max(testDate, na.rm = TRUE)) %>%
              summarise(
                most_recent_benchmark = mean(benchmarkPct, na.rm = TRUE),
                .groups = "drop"
              )
            
            # 3. Compute aggregated daily average and rolling average using rollapply()
            df_trend <- df_sub %>%
              group_by(testDate, quality) %>%
              summarise(avg_benchmark = mean(benchmarkPct, na.rm = TRUE), .groups = "drop") %>%
              arrange(testDate) %>%
              group_by(quality) %>%
              mutate(
                rolling_avg = zoo::rollapply(
                  avg_benchmark,
                  width = 5,
                  FUN = mean,
                  align = "right",
                  fill = NA,
                  partial = TRUE
                )
              ) %>%
              ungroup()
            
            # 4. Join the most recent benchmark into the aggregated data for hover text
            df_trend <- df_trend %>%
              left_join(df_most, by = "quality") %>%
              mutate(
                hover_text = paste0(
                  "Quality: ", quality, "<br>",
                  "Date: ", format(testDate, "%Y-%m-%d"), "<br>",
                  "Rolling Avg: ", ifelse(is.na(rolling_avg),
                                          "NA",
                                          scales::percent(rolling_avg, accuracy = 1)
                  ), "<br>",
                  "Most Recent: ", scales::percent(most_recent_benchmark, accuracy = 1)
                )
              )
            
            # 5. Determine dynamic x-axis limits based on trend data
            min_d <- min(df_trend$testDate, na.rm = TRUE)
            max_d <- max(df_trend$testDate, na.rm = TRUE)
            
            # 6. Build the Plot without fixed y-axis limits
            p <- ggplot(df_trend, aes(x = testDate, group = quality, color = quality)) +
              geom_line(aes(y = rolling_avg, text = hover_text), size = 0.8) +
              geom_point(aes(y = avg_benchmark, text = hover_text), size = 2) +
              labs(
                title = paste("Benchmark Trend –",
                              tools::toTitleCase(current_expr),
                              tools::toTitleCase(gsub("_", " ", current_mv))
                              ),
                y = "Benchmark %",
                x = "Test Date",
                color = "Quality"
              ) +
              scale_x_date(
                date_labels = "%b\n%Y"
              ) +
              scale_y_continuous(
                labels = scales::percent_format(accuracy = 1)
              ) +
              theme_minimal() +
              theme(
                axis.text.x = element_text(angle = 0, hjust = 0.5),
                axis.title.x = element_text(margin = margin(t = 10))
              )
            
            # 7. Convert to Plotly and force autorange on x-axis and y-axis
            ggplotly(p, tooltip = "text") %>% layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
          })
        })
      }
    }
  })
  
  # Radar Chart: Aggregated summary per movement for Isometric and Dynamic benchmarks
  # Add this at the top with your other libraries:
  output$radar_Agility <- renderPlotly({
    req(nrow(pcap_filtered_data()) > 0)
    df <- pcap_filtered_data() %>%
      mutate(
        testDate   = as.Date(testDate),
        movement   = str_to_title(gsub("_", " ", movement)),
        expression = str_to_title(trimws(expression)),
        quality    = str_to_title(trimws(quality)),
        # Insert newline between expression and quality for better label formatting
        combo      = paste0(expression, "\n", quality)
      ) %>%
      filter(!is.na(benchmarkPct)) %>%
      filter(movement == "Agility")
    req(nrow(df) > 0)
    
    df_latest <- df %>%
      group_by(combo) %>%
      filter(testDate == max(testDate, na.rm = TRUE)) %>%
      summarise(latest_benchmark = mean(benchmarkPct, na.rm = TRUE),
                testDate = max(testDate, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(combo)
    req(nrow(df_latest) > 0)
    
    df_closed <- bind_rows(df_latest, df_latest[1,])
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = df_closed$latest_benchmark,
        theta = df_closed$combo,
        fill = 'toself',
        fillcolor = "rgba(0,60,143,0.3)",  # Chelsea blue, semi-transparent fill
        line = list(color = "#003C8F"),
        marker = list(color = "#003C8F"),
        text = paste0("Test Date: ", df_closed$testDate,
                      "<br>Benchmark: ", scales::percent(df_closed$latest_benchmark, accuracy = 1)),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "Agility", x = 0.5, font = list(size = 21)),
        margin = list(l = 40, r = 40, b = 40, t = 90),
        polar = list(
          radialaxis = list(
            tickvals = seq(0, 1, by = 0.2),
            ticktext = c("0%", "20%", "40%", "60%", "80%", "100%"),
            range = c(0, 1),
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            automargin = TRUE
          )
        )
      )
  })
  
  output$radar_Jump <- renderPlotly({
    req(nrow(pcap_filtered_data()) > 0)
    df <- pcap_filtered_data() %>%
      mutate(
        testDate   = as.Date(testDate),
        movement   = str_to_title(gsub("_", " ", movement)),
        expression = str_to_title(trimws(expression)),
        quality    = str_to_title(trimws(quality)),
        combo      = paste0(expression, "\n", quality)
      ) %>%
      filter(!is.na(benchmarkPct)) %>%
      filter(movement == "Jump")
    req(nrow(df) > 0)
    
    df_latest <- df %>%
      group_by(combo) %>%
      filter(testDate == max(testDate, na.rm = TRUE)) %>%
      summarise(latest_benchmark = mean(benchmarkPct, na.rm = TRUE),
                testDate = max(testDate, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(combo)
    req(nrow(df_latest) > 0)
    
    df_closed <- bind_rows(df_latest, df_latest[1,])
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = df_closed$latest_benchmark,
        theta = df_closed$combo,
        fill = 'toself',
        fillcolor = "rgba(0,60,143,0.3)",
        line = list(color = "#003C8F"),
        marker = list(color = "#003C8F"),
        text = paste0("Test Date: ", df_closed$testDate,
                      "<br>Benchmark: ", scales::percent(df_closed$latest_benchmark, accuracy = 1)),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "Jump", x = 0.5, font = list(size = 21)),
        margin = list(l = 40, r = 40, b = 40, t = 90),
        polar = list(
          radialaxis = list(
            tickvals = seq(0, 1, by = 0.2),
            ticktext = c("0%", "20%", "40%", "60%", "80%", "100%"),
            range = c(0, 1),
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            automargin = TRUE
          )
        )
      )
  })
  
  output$radar_Sprint <- renderPlotly({
    req(nrow(pcap_filtered_data()) > 0)
    df <- pcap_filtered_data() %>%
      mutate(
        testDate   = as.Date(testDate),
        movement   = str_to_title(gsub("_", " ", movement)),
        expression = str_to_title(trimws(expression)),
        quality    = str_to_title(trimws(quality)),
        combo      = paste0(expression, "\n", quality)
      ) %>%
      filter(!is.na(benchmarkPct)) %>%
      filter(movement == "Sprint")
    req(nrow(df) > 0)
    
    df_latest <- df %>%
      group_by(combo) %>%
      filter(testDate == max(testDate, na.rm = TRUE)) %>%
      summarise(latest_benchmark = mean(benchmarkPct, na.rm = TRUE),
                testDate = max(testDate, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(combo)
    req(nrow(df_latest) > 0)
    
    df_closed <- bind_rows(df_latest, df_latest[1,])
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = df_closed$latest_benchmark,
        theta = df_closed$combo,
        fill = 'toself',
        fillcolor = "rgba(0,60,143,0.3)",
        line = list(color = "#003C8F"),
        marker = list(color = "#003C8F"),
        text = paste0("Test Date: ", df_closed$testDate,
                      "<br>Benchmark: ", scales::percent(df_closed$latest_benchmark, accuracy = 1)),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "Sprint", x = 0.5, font = list(size = 21)),
        margin = list(l = 40, r = 40, b = 40, t = 90),
        polar = list(
          radialaxis = list(
            tickvals = seq(0, 1, by = 0.2),
            ticktext = c("0%", "20%", "40%", "60%", "80%", "100%"),
            range = c(0, 1),
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            automargin = TRUE
          )
        )
      )
  })
  
  output$radar_UpperBody <- renderPlotly({
    req(nrow(pcap_filtered_data()) > 0)
    df <- pcap_filtered_data() %>%
      mutate(
        testDate   = as.Date(testDate),
        movement   = str_to_title(gsub("_", " ", movement)),
        expression = str_to_title(trimws(expression)),
        quality    = str_to_title(trimws(quality)),
        combo      = paste0(expression, "\n", quality)
      ) %>%
      filter(!is.na(benchmarkPct)) %>%
      filter(movement == "Upper Body")
    req(nrow(df) > 0)
    
    df_latest <- df %>%
      group_by(combo) %>%
      filter(testDate == max(testDate, na.rm = TRUE)) %>%
      summarise(latest_benchmark = mean(benchmarkPct, na.rm = TRUE),
                testDate = max(testDate, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(combo)
    req(nrow(df_latest) > 0)
    
    df_closed <- bind_rows(df_latest, df_latest[1,])
    
    plot_ly(type = 'scatterpolar', mode = 'lines+markers') %>%
      add_trace(
        r = df_closed$latest_benchmark,
        theta = df_closed$combo,
        fill = 'toself',
        fillcolor = "rgba(0,60,143,0.3)",
        line = list(color = "#003C8F"),
        marker = list(color = "#003C8F"),
        text = paste0("Test Date: ", df_closed$testDate,
                      "<br>Benchmark: ", scales::percent(df_closed$latest_benchmark, accuracy = 1)),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "Upper Body", x = 0.5, font = list(size = 21)),
        margin = list(l = 40, r = 40, b = 40, t = 90),
        polar = list(
          radialaxis = list(
            tickvals = seq(0, 1, by = 0.2),
            ticktext = c("0%", "20%", "40%", "60%", "80%", "100%"),
            range = c(0, 1),
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            automargin = TRUE
          )
        )
      )
  })
  
}

shinyApp(ui, server)