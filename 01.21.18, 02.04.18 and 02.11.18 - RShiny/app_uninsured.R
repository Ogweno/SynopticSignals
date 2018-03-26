# Load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(choroplethrMaps)
library(choroplethr)
library(rsconnect)


# Load data

acadat <- read.csv(file="states2.csv", header = TRUE, sep = ",", strip.white = TRUE )

head(acadat)

# rename columns and convert state names to lowercase
acadat$region  = tolower(acadat$State)


# get us state map data and merge with insurance data
us_state_map <- map_data('state');
map_data = merge(acadat, us_state_map, by = 'region') 

# keep data sorted by polygon order
map_data = arrange(map_data, order)

# User interface
ui <- fluidPage(
  titlePanel("Uninsured Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create Uninsured Rate maps with informaction from state.gov"),
      
      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Uninsured Rate 2010", "Uninsured Rate 2015", "Change in Uninsured Rate 2010-2015"),
                  selected = "Change in Uninsured Rate 2010-2015")),
      
      mainPanel(plotOutput("map"))
    )
  )

# Server Logic

server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Uninsured Rate 2010" = map_data$Uninsured.Rate..2010.,
                   "Uninsured Rate 2015" = map_data$Uninsured.Rate..2015.,
                   "Change in Uninsured Rate 2010-2015" = map_data$Uninsured.Rate.Change..2010.2015.)
    
    title <- switch(input$var,
                    "Uninsured Rate 2010" = "Uninsured Rate 2010",
                    "Uninsured Rate 2015" = "Uninsured Rate 2015",
                    "Change in Uninsured Rate 2010-2015" = "Change in Uninsured Rate 2010-2015")
    
    # Uninsured Plot
  ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(data, 6))) +
  geom_path(colour = 'gray') +  labs(title = title) +
  scale_fill_brewer('Uninsured Percent, 2010') + coord_map()
    
  })
}

# Run app 
shinyApp(ui, server)

