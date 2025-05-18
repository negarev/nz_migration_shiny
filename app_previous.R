#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)

library(sf)
library(leaflet)

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




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NZ Statistical Data Explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Select year:",
                        min = 2018,
                        max = 2024,
                        value = 2018),
            sliderInput("n_obs",
                        "Select how many observations:",
                        min = 5,
                        max = 30,
                        value = 10,
                        step = 5),
            selectInput('measure', 'Select a measure', unique(by_area$measure))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {



        # draw the histogram with the specified number of bins
      output$plot1 <- renderPlot(
        by_area |>
        filter(measure==input$measure, year.at.30.june==input$year) |>
        arrange(desc(obs_value)) |>
        head(input$n_obs) |>
        ggplot(aes(x=reorder(area, obs_value), y=obs_value))+
        geom_col()+
        coord_flip()+
          labs(title=paste("Top ",input$n_obs," Territorial Authorities by ",tolower(input$measure)," for ",input$year, sep=""))
    )}


# Run the application
shinyApp(ui = ui, server = server)
