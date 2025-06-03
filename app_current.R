library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(htmltools)

# Ensure data is loaded correctly - wrap in tryCatch to handle potential errors
tryCatch({
  nz_data <- read.csv("data/nz_stat_data_by_TA.csv", colClasses = c("AREA_POPES_SUB_001"="character"))
  names(nz_data) <- tolower(names(nz_data))

  by_area <- nz_data |>
    filter(area_popes_sub_001 < 10000) |>
    select(c("year_popes_sub_001",
             "year.at.30.june",
             "area_popes_sub_001",
             "area",
             "measure_popes_sub_001",
             "obs_value")) |>
    mutate(measure = case_match(measure_popes_sub_001,
                                "POP" ~ "Population",
                                "MEDAGE" ~ "Median age",
                                "NATINC" ~ "Natural increase",
                                "NETINTERNAL" ~ "Net internal migration",
                                "NETMIG" ~ "Net migration",
                                "NETINTERNATIONAL" ~ "Net international migration"))

  nz_shape <- read_sf("data/geodata/territorial-authority-local-board-2025.shp") |>
    st_transform(4326)
  names(nz_shape) <- tolower(names(nz_shape))

  joined_file <- inner_join(nz_shape, by_area, by = join_by(talb2025_v == area_popes_sub_001))

}, error = function(e) {
  message("Error loading data: ", e$message)
})

# Define UI with proper layout
ui <- fluidPage(
  theme = shinythemes::shinytheme('simplex'),
  titlePanel("New Zealand Territorial Authority Data"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Select year:",
                  min = 2018,
                  max = 2024,
                  value = 2018),
      selectInput('measure', 'Select a measure', unique(by_area$measure)),
      sliderInput("n_obs",
                  "Select how many observations:",
                  min = 5,
                  max = 30,
                  value = 10,
                  step = 5)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput('map', height = "600px")),
        tabPanel("Plot", plotOutput("plot", height = "600px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Fix reactive data filtering to ensure it works properly
  rval_map <- reactive({
    # Print for debugging
    print(paste("Filtering for year:", input$year, "and measure:", input$measure))

    filtered <- joined_file |>
      filter(year.at.30.june == input$year,  # Changed from year_popes_sub_001 to year.at.30.june
             measure == input$measure)

    # Print row count for debugging
    print(paste("Filtered data rows:", nrow(filtered)))
    return(filtered)
  })

  output$map <- renderLeaflet({
    req(input$year, input$measure)  # Ensure inputs are available

    filtered_data <- rval_map()

    # Check if data is empty
    if(nrow(filtered_data) == 0) {
      return(leaflet() |>
               addTiles() |>
               setView(174.25, -41.33, zoom = 5) |>
               addControl("No data available for selected criteria", position = "topright"))
    }

    value_min <- min(filtered_data$obs_value, na.rm = TRUE)
    value_max <- max(filtered_data$obs_value, na.rm = TRUE)
    nc_pal <- colorNumeric(palette = "Blues", domain = c(value_min, value_max))

    html_labels <- paste0("<b>", filtered_data$talb2025_1, "</b>",
                          "<br/>",
                          filtered_data$measure,
                          " : ",
                          filtered_data$obs_value)

    leaflet() |>
      addTiles() |>
      setView(174.25, -41.33, zoom = 5) |>
      addPolygons(data = filtered_data,
                  weight = 1,
                  fillOpacity = 0.7,  # Reduced opacity for better visibility
                  fillColor = ~nc_pal(obs_value),  # Changed to fillColor from color
                  color = "white",  # Added border color
                  label = lapply(html_labels, HTML),
                  highlight = highlightOptions(weight = 3,
                                               color = "red",
                                               bringToFront = TRUE)) |>
      addLegend(
        position = "bottomright",
        pal = nc_pal,
        values = filtered_data$obs_value,
        title = input$measure,
        opacity = 1
      )
  })

  output$plot <- renderPlot({
    req(input$year, input$measure, input$n_obs)  # Ensure inputs are available

    filtered_data <- rval_map()

    # Check if data is empty
    if(nrow(filtered_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected criteria") +
               theme_minimal())
    }

    filtered_data |>
      arrange(desc(obs_value)) |>
      head(input$n_obs) |>
      ggplot(aes(x = reorder(area, obs_value), y = obs_value)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Top", input$n_obs, "Territorial Authorities by",
                         tolower(input$measure), "for", input$year),
           x = "Territorial Authority",
           y = input$measure) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
