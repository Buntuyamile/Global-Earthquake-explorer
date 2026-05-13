library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(ggplot2)
library(DT)

# DATA LOADING AND WRANGLING 
# Load raw datasets from the 'data' subdirectory

earthquakes_raw <- read.csv("Earthquakes 1965 - 2016.csv")
query_raw       <- read.csv("query.csv")

# Clean Earthquake dataset 
earthquake_clean <- earthquakes_raw |>
  mutate(
    # Combine separate Date and Time columns into one UTC datetime
    datetime = parse_date_time(paste(Date, Time), orders = "mdy HMS", tz = "UTC")
  ) |>
  rename(
    latitude         = Latitude,
    longitude        = Longitude,
    type             = Type,
    depth            = Depth,
    depth_error      = Depth.Error,
    magnitude        = Magnitude,
    magnitude_type   = Magnitude.Type,
    magnitude_error  = Magnitude.Error,
    azimuthal_gap    = Azimuthal.Gap,
    horizontal_error = Horizontal.Error,
    rms              = Root.Mean.Square,
    id               = ID,
    status           = Status,
    location_source  = Location.Source,
    magnitude_source = Magnitude.Source
  ) |>
  select(datetime, latitude, longitude, depth, depth_error,
         magnitude, magnitude_type, magnitude_error,
         azimuthal_gap, horizontal_error, rms,
         id, status, location_source, magnitude_source, type)

#  Clean query dataset 
query_clean <- query_raw |>
  mutate(datetime = ymd_hms(time, tz = "UTC")) |>
  rename(
    magnitude        = mag,
    magnitude_type   = magType,
    azimuthal_gap    = gap,
    horizontal_error = horizontalError,
    depth_error      = depthError,
    magnitude_error  = magError,
    status           = status,
    location_source  = locationSource,
    magnitude_source = magSource
  ) |>
  mutate(type = str_to_title(type)) |>
  select(datetime, latitude, longitude, depth, depth_error,
         magnitude, magnitude_type, magnitude_error,
         azimuthal_gap, horizontal_error, rms,
         id, status, location_source, magnitude_source, type)

# Merge datasets and filter to earthquakes only
earthquakes_df <- bind_rows(earthquake_clean, query_clean) |>
  arrange(datetime)

earthquakes <- earthquakes_df |>
  filter(type == "Earthquake")

# Add ordered Scale variable following the Richter magnitude scale 
scale_levels <- c("Micro", "Minor", "Slight", "Light", "Moderate",
                  "Strong", "Major", "Great", "Extreme")

earthquakes <- earthquakes |>
  mutate(
    Scale = case_when(
      magnitude >= 1.0 & magnitude < 2.0 ~ "Micro",
      magnitude >= 2.0 & magnitude < 3.0 ~ "Minor",
      magnitude >= 3.0 & magnitude < 4.0 ~ "Slight",
      magnitude >= 4.0 & magnitude < 5.0 ~ "Light",
      magnitude >= 5.0 & magnitude < 6.0 ~ "Moderate",
      magnitude >= 6.0 & magnitude < 7.0 ~ "Strong",
      magnitude >= 7.0 & magnitude < 8.0 ~ "Major",
      magnitude >= 8.0 & magnitude < 9.0 ~ "Great",
      magnitude >= 9.0                   ~ "Extreme",
      TRUE                               ~ NA_character_
    ),
    Scale = factor(Scale, levels = scale_levels, ordered = TRUE),
    year  = year(datetime),
    month = month(datetime, label = TRUE, abbr = TRUE)
  )

# colours
scale_colours <- c(
  "Micro"    = "lightblue",
  "Minor"    = "skyblue",
  "Slight"   = "steelblue",
  "Light"    = "royalblue",
  "Moderate" = "yellow",
  "Strong"   = "orange",
  "Major"    = "pink",
  "Great"    = "red",
  "Extreme"  = "darkred"
)

# World map base layer
world <- map_data("world")

# Year range used across all sliders
year_min <- min(earthquakes$year, na.rm = TRUE)
year_max <- max(earthquakes$year, na.rm = TRUE)

# USER INTERFACE

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: white; font-family: Arial, sans-serif; }
      h4 { color: darkgrey; }
      .well { background-color: lightgrey; border: none; }
    "))
  ),
  
  
  navbarPage(
    
    title = "Global Earthquake Explorer (1965-2023)",
    
    
    # TAB 1: Feature Distributions
    
    tabPanel("Feature Distributions",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filters"),
                 sliderInput("dist_years", "Year Range:",
                             min = year_min, max = year_max,
                             value = c(year_min, year_max), sep = ""),
                 sliderInput("dist_mag", "Magnitude Range:",
                             min = floor(min(earthquakes$magnitude, na.rm = TRUE)),
                             max = ceiling(max(earthquakes$magnitude, na.rm = TRUE)),
                             value = c(5, 10), step = 0.5),
                 checkboxGroupInput("dist_scale", "Scale Categories:",
                                    choices  = scale_levels,
                                    selected = c("Moderate", "Strong", "Major",
                                                 "Great", "Extreme")),
                 hr(),
                 h4("Plot Options"),
                 selectInput("dist_feature", "Feature to plot:",
                             choices = c("Magnitude" = "magnitude",
                                         "Depth (km)" = "depth")),
                 numericInput("dist_bins", "Number of bins:",
                              value = 60, min = 10, max = 200)
               ),
               
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(8, plotOutput("distPlot",     height = "350px")),
                   column(4, plotOutput("scalePiePlot", height = "350px"))
                 ),
                 hr(),
                 fluidRow(column(12, DTOutput("summaryTable")))
               )
             )
    ),
    
    
    # TAB 2: Temporal Trends
    
    tabPanel("Temporal Trends",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filters"),
                 sliderInput("temp_years", "Year Range:",
                             min = year_min, max = year_max,
                             value = c(year_min, year_max), sep = ""),
                 checkboxGroupInput("temp_scale", "Scale Categories:",
                                    choices  = scale_levels,
                                    selected = c("Moderate", "Strong", "Major",
                                                 "Great", "Extreme")),
                 hr(),
                 h4("M5+ Trend Options"),
                 sliderInput("temp_loess_span", "LOESS Smoothing Span:",
                             min = 0.1, max = 1.0, value = 0.3, step = 0.05)
               ),
               
               mainPanel(
                 width = 9,
                 h4("Annual Earthquake Counts by Scale"),
                 plotOutput("annualStackPlot", height = "350px"),
                 hr(),
                 h4("Magnitude 5.0+ Trend Over Time"),
                 plotOutput("m5TrendPlot", height = "350px")
               )
             )
    ),
    
    # TAB 3: Spatial Explorer
    
    tabPanel("Spatial Explorer",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Map Filters"),
                 sliderInput("map_mag", "Minimum Magnitude:",
                             min = 4, max = 9, value = 5, step = 0.5),
                 sliderInput("map_years", "Year Range:",
                             min = year_min, max = year_max,
                             value = c(year_min, year_max), sep = ""),
                 hr(),
                 h4("Region Zoom"),
                 selectInput("map_region", "Select Region:",
                             choices = c(
                               "Global"                         = "global",
                               "Western Pacific (Ring of Fire)" = "wpacific",
                               "Mediterranean Belt"             = "mediterranean",
                               "South American Subduction Zone" = "southamerica",
                               "Custom"                         = "custom"
                             )),
                 conditionalPanel(
                   condition = "input.map_region == 'custom'",
                   numericInput("cust_lon_min", "Min Longitude:", value = -180),
                   numericInput("cust_lon_max", "Max Longitude:", value =  180),
                   numericInput("cust_lat_min", "Min Latitude:",  value =  -90),
                   numericInput("cust_lat_max", "Max Latitude:",  value =   90)
                 ),
                 hr(),
                 h4("Plot Options"),
                 selectInput("map_colour", "Colour by:",
                             choices = c("Magnitude" = "magnitude",
                                         "Depth (km)" = "depth")),
                 sliderInput("map_alpha", "Point Transparency:",
                             min = 0.1, max = 1.0, value = 0.4, step = 0.05),
                 sliderInput("map_ptsize", "Max Point Size:",
                             min = 0.5, max = 5.0, value = 2.0, step = 0.5)
               ),
               
               mainPanel(
                 width = 9,
                 plotOutput("mapPlot", height = "550px"),
                 hr(),
                 h5("Summary statistics for filtered region:"),
                 tableOutput("mapStats")
               )
             )
    ),
    
    # TAB 4: Top Earthquakes
    
    tabPanel("Top Earthquakes",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filters"),
                 sliderInput("top_n", "Number of top earthquakes:",
                             min = 5, max = 50, value = 20, step = 5),
                 sliderInput("top_years", "Year Range:",
                             min = year_min, max = year_max,
                             value = c(year_min, year_max), sep = ""),
                 sliderInput("top_mag", "Minimum Magnitude:",
                             min = 4, max = 9, value = 7, step = 0.5)
               ),
               
               mainPanel(
                 width = 9,
                 DTOutput("topTable"),
                 hr(),
                 plotOutput("topMapPlot", height = "400px")
               )
             )
    ),
    
    # TAB 5: About
    
    tabPanel("About",
             fluidRow(
               column(8, offset = 2,
                      h3("Global Earthquake Explorer"),
                      p("Shiny app for STA5092Z Exploratory Data Analysis, Assignment 3."),
                      h4("Data Sources"),
                      tags$ul(
                        tags$li(strong("Earthquakes 1965-2016:"),
                                " Kaggle dataset, 2 Jan 1965 to 30 Dec 2016."),
                        tags$li(strong("query.csv:"),
                                " USGS website query, 1 Dec 2016 to 17 Mar 2023.")
                      ),
                      h4("Data Processing"),
                      p("Datasets merged after harmonising variable names and datetime formats.
            Only common variables retained. Filtered to Earthquake type events
            (34,168 records). Richter Scale category variable added."),
                      h4("References"),
                      tags$ol(
                        tags$li("Gutenberg & Richter (1956). BSSA, 46(2), 105-145."),
                        tags$li("Isacks, Oliver & Sykes (1968). JGR, 73(18), 5855-5899."),
                        tags$li("Jackson & McKenzie (1984). GJR Astron. Soc., 77(1), 185-264."),
                        tags$li("Lay & Wallace (1995). Modern Global Seismology. Academic Press.")
                      )
               )
             )
    )
    
  ) # end navbarPage
) # end fluidPage

# SERVER

server <- function(input, output, session) {
  # TAB 1: Feature Distributions
  dist_data <- reactive({
    earthquakes |>
      filter(
        year      >= input$dist_years[1],
        year      <= input$dist_years[2],
        magnitude >= input$dist_mag[1],
        magnitude <= input$dist_mag[2],
        Scale     %in% input$dist_scale
      )
  })
  
  # Histogram of selected feature coloured by Scale
  output$distPlot <- renderPlot({
    df   <- dist_data()
    feat <- input$dist_feature
    xlab <- ifelse(feat == "magnitude", "Magnitude", "Depth (km, log scale)")
    
    p <- ggplot(df, aes(x = .data[[feat]], fill = Scale)) +
      scale_fill_manual(values = scale_colours, drop = FALSE) +
      labs(x = xlab, y = "Count", fill = "Scale") +
      theme_minimal(base_size = 13)
    
    if (feat == "depth") {
      p <- p + geom_histogram(bins = input$dist_bins, colour = "white") +
        scale_x_log10()
    } else {
      p <- p + geom_histogram(bins = input$dist_bins, colour = "white")
    }
    p
  })
  
  # Pie chart: proportional scale composition
  output$scalePiePlot <- renderPlot({
    df <- dist_data() |>
      count(Scale, .drop = FALSE) |>
      filter(n > 0)
    
    ggplot(df, aes(x = "", y = n, fill = Scale)) +
      geom_col(colour = "white") +
      coord_polar("y") +
      scale_fill_manual(values = scale_colours, drop = FALSE) +
      labs(fill = "Scale", title = "Scale Composition") +
      theme_void(base_size = 12) +
      theme(legend.position = "right")
  })
  
  # Summary statistics table
  output$summaryTable <- renderDT({
    dist_data() |>
      summarise(
        N                 = n(),
        `Mag Min`         = round(min(magnitude,  na.rm = TRUE), 2),
        `Mag Mean`        = round(mean(magnitude, na.rm = TRUE), 2),
        `Mag Max`         = round(max(magnitude,  na.rm = TRUE), 2),
        `Depth Min (km)`  = round(min(depth,  na.rm = TRUE), 1),
        `Depth Mean (km)` = round(mean(depth, na.rm = TRUE), 1),
        `Depth Max (km)`  = round(max(depth,  na.rm = TRUE), 1)
      ) |>
      datatable(options = list(dom = "t"), rownames = FALSE)
  })
  
  # TAB 2: Temporal Trends
  
  temp_data <- reactive({
    earthquakes |>
      filter(
        year  >= input$temp_years[1],
        year  <= input$temp_years[2],
        Scale %in% input$temp_scale
      )
  })
  
  # Stacked bar chart: annual counts by Scale
  output$annualStackPlot <- renderPlot({
    df <- temp_data() |> count(year, Scale)
    
    ggplot(df, aes(x = year, y = n, fill = Scale)) +
      geom_col() +
      scale_fill_manual(values = scale_colours, drop = FALSE) +
      labs(x = "Year", y = "Count", fill = "Scale") +
      theme_minimal(base_size = 13)
  })
  
  # M5+ annual count with LOESS trend overlay
  output$m5TrendPlot <- renderPlot({
    df <- earthquakes |>
      filter(
        magnitude >= 5,
        year >= input$temp_years[1],
        year <= input$temp_years[2]
      ) |>
      count(year)
    
    ggplot(df, aes(x = year, y = n)) +
      geom_line(colour = "steelblue") +
      geom_smooth(method = "loess",
                  span   = input$temp_loess_span,
                  colour = "red",
                  fill   = "orange",
                  alpha  = 0.3) +
      labs(x = "Year", y = "Count",
           subtitle = "Red = LOESS trend; shaded = 95% confidence band") +
      theme_minimal(base_size = 13)
  })
  
  # TAB 3: Spatial Explorer
  
  # Return lon/lat limits for the selected region
  map_bbox <- reactive({
    switch(input$map_region,
           "global"        = list(lon = c(-180,  180), lat = c(-90, 90)),
           "wpacific"      = list(lon = c( 100,  180), lat = c(-60, 70)),
           "mediterranean" = list(lon = c( -15,   65), lat = c( 25, 50)),
           "southamerica"  = list(lon = c( -85,  -30), lat = c(-60, 15)),
           "custom"        = list(
             lon = c(input$cust_lon_min, input$cust_lon_max),
             lat = c(input$cust_lat_min, input$cust_lat_max)
           )
    )
  })
  
  # Filter earthquakes to bounding box and map filters
  map_data_filtered <- reactive({
    bb <- map_bbox()
    earthquakes |>
      filter(
        magnitude >= input$map_mag,
        year      >= input$map_years[1],
        year      <= input$map_years[2],
        longitude >= bb$lon[1], longitude <= bb$lon[2],
        latitude  >= bb$lat[1], latitude  <= bb$lat[2]
      )
  })
  
  # Map output
  output$mapPlot <- renderPlot({
    df         <- map_data_filtered()
    bb         <- map_bbox()
    colour_var <- input$map_colour
    colour_lab <- ifelse(colour_var == "magnitude", "Magnitude", "Depth (km)")
    
    ggplot() +
      geom_polygon(data = world,
                   aes(x = long, y = lat, group = group),
                   fill = "#2980b9", colour = "#8e44ad", linewidth = 0.2) +
      geom_point(data = df,
                 aes(x = longitude, y = latitude,
                     colour = .data[[colour_var]],
                     size   = magnitude),
                 alpha = input$map_alpha) +
      scale_colour_gradient(low = "orange", high = "darkred",
                            name = colour_lab) +
      scale_size_continuous(range = c(0.3, input$map_ptsize), guide = "none") +
      coord_fixed(1.3, xlim = bb$lon, ylim = bb$lat) +
      labs(x = "Longitude", y = "Latitude") +
      theme_minimal(base_size = 13)
  })
  
  # Summary statistics for the visible map area
  output$mapStats <- renderTable({
    df <- map_data_filtered()
    data.frame(
      "Events shown"   = nrow(df),
      "Min Magnitude"  = round(min(df$magnitude,  na.rm = TRUE), 2),
      "Mean Magnitude" = round(mean(df$magnitude, na.rm = TRUE), 2),
      "Max Magnitude"  = round(max(df$magnitude,  na.rm = TRUE), 2),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  # TAB 4: Top Earthquakes
  top_data <- reactive({
    earthquakes |>
      filter(
        year      >= input$top_years[1],
        year      <= input$top_years[2],
        magnitude >= input$top_mag
      ) |>
      slice_max(magnitude, n = input$top_n) |>
      arrange(desc(magnitude)) |>
      mutate(
        Date         = format(datetime, "%Y-%m-%d"),
        Latitude     = round(latitude,  2),
        Longitude    = round(longitude, 2),
        `Depth (km)` = round(depth,     1),
        Magnitude    = round(magnitude, 1)
      ) |>
      select(Date, Latitude, Longitude, `Depth (km)`, Magnitude, Scale)
  })
  
  # Searchable, sortable datatable
  output$topTable <- renderDT({
    datatable(top_data(),
              selection = "single",
              options   = list(pageLength = 10, scrollX = TRUE),
              rownames  = FALSE)
  })
  
  # World map with labelled top earthquakes
  output$topMapPlot <- renderPlot({
    df <- top_data()
    
    ggplot() +
      geom_polygon(data = world,
                   aes(x = long, y = lat, group = group),
                   fill = "#2980b9", colour = "#8e44ad", linewidth = 0.2) +
      geom_point(data = df,
                 aes(x = Longitude, y = Latitude,
                     colour = Magnitude, size = Magnitude),
                 alpha = 0.8) +
      geom_text(data = df,
                aes(x = Longitude, y = Latitude,
                    label = paste0("M", Magnitude)),
                size = 2.5, colour = "white",
                fontface = "bold", vjust = -0.8) +
      scale_colour_gradient(low = "orange", high = "darkred",
                            name = "Magnitude") +
      scale_size_continuous(range = c(3, 8), guide = "none") +
      coord_fixed(1.3) +
      labs(x = "Longitude", y = "Latitude") +
      theme_minimal(base_size = 12)
  })
  
} # end server

# RUN THE APP

shinyApp(ui = ui, server = server)


