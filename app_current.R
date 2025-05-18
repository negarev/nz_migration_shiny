#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

library(sf)
library(leaflet)
library(htmltools)

nz_data <- read.csv("data/nz_stat_data_by_TA.csv", colClasses = c("AREA_POPES_SUB_001"="character"))

names(nz_data) <- tolower(names(nz_data))


by_area <- nz_data |>
  filter(area_popes_sub_001<10000) |>
  select(c("year_popes_sub_001",
                   "year.at.30.june",
                   "area_popes_sub_001",
                   "area",
                   "measure_popes_sub_001",
                   "obs_value")) |>
  mutate(measure=case_match(measure_popes_sub_001,
                            "POP" ~ "Population",
                            "MEDAGE" ~ "Median age",
                            "NATINC" ~ "Natural increase",
                            "NETINTERNAL" ~ "Net internal migration",
                            "NETMIG" ~ "Net migration",
                            "NETINTERNATIONAL" ~ "Net international migration"
                          ))

nz_shape<- read_sf("data/geodata/territorial-authority-local-board-2025.shp") |>
  st_transform(4326)

names(nz_shape) <- tolower(names(nz_shape))

joined_file <- inner_join(nz_shape, by_area, by= join_by(talb2025_v==area_popes_sub_001))

map_file <- filter(joined_file, year.at.30.june==2018 & measure_popes_sub_001=="POP")

# leaflet() |>
#   addTiles() |>
#   addPolygons(data = map_file, color="blue", stroke = 1, opacity=0.8)



# Define UI for application that draws a histogram

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput("year",
                            "Select year:",
                            min = 2018,
                            max = 2024,
                            value = 2018),
                selectInput('measure', 'Select a measure', unique(by_area$measure))
  ),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}
    #controls{background-color:white;padding:20px;}
  ")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  rval_map <- reactive({

    joined_file |>
      filter(year_popes_sub_001==input$year,
             measure==input$measure)
  })

  output$map <- leaflet::renderLeaflet({

    filtered_data <- rval_map()
    value_min <- min(filtered_data$obs_value, na.rm = TRUE)
    value_max <- max(filtered_data$obs_value, na.rm = TRUE)
    nc_pal <- colorNumeric(palette = "Blues", domain = c(value_min, value_max))

    html_labels <- paste0("<b>", filtered_data$talb2025_1, "</b>", # Bold text for the name
    "<br/>", # Line break
    filtered_data$measure,
    " : ",
    filtered_data$obs_value) # Additional information
    leaflet()  |>
      addTiles() |>
      setView( 174.25, -41.33, zoom = 5)  |>
      addTiles() |>
      addPolygons(data=filtered_data, weight =1, fillOpacity =1,
                  color =~nc_pal(obs_value),
                  label =lapply(html_labels, htmltools::HTML),
                  highlight = highlightOptions(weight =3,
                                               color ="red",
                                               bringToFront =TRUE)) |>
    addLegend(
      position = "bottomright",
      pal = nc_pal,
      values = filtered_data$obs_value,
      title = input$measure,
      opacity = 1
    )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
