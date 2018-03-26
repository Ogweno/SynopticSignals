# Load packages
library(dplyr)
library(ggplot2)
library(reshape2)

# Load dataset
nat_exp <- read.csv(file="nat_hlth_exp.csv", header = TRUE, sep = ",", strip.white = TRUE )

# Transform dataset for graphing purposes
m_nat_exp <- melt(nat_exp, id = c("First.Level", "Second.Level", "Third.Level", "Fourth.Level", "Fifth.Level"))

# Change year variable into numeric so stacked bar displays correctly
m_nat_exp$variable <- substring(m_nat_exp$variable, 2)

m_nat_exp$variable <- as.numeric(m_nat_exp$variable)


# User interface
ui <- fluidPage(
  titlePanel("National Health Expenditures"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize National Health Expenditures with information from https://www.cms.gov"),
      
      selectInput("var",
                  label = "Choose a graph to display",
                  choices = c("By Service or Product", "By Source of Funding"),
                  selected = "By Service or Product"),
      
      selectInput("var2",
                  label = "Select a Level of Disaggregation",
                  choices = c("Second Level", "Third Level", "Fourth Level", "Fifth Level"),
                  selected = "Fourth Level"),
    
     # Add a slider selector for years to filter
      sliderInput("years", "Years",
                  min(m_nat_exp$variable), max(m_nat_exp$variable),
                  value = c(1960, 2016), sep = "")),
     
      mainPanel(plotOutput("graph"))
    )
  )

# Server Logic

server <- function(input, output) {
  output$graph <- renderPlot({
    data <- switch(input$var,
                   "By Service or Product" = "Type of Service or Product",
                   "By Source of Funding" = "Source of Funds")
    
    level <- switch(input$var2,
                    "Second Level" = "Second.Level",
                    "Third Level" = "Third.Level",
                    "Fourth Level" = "Fourth.Level",
                    "Fifth Level" = "Fifth.Level")
    
   m_nat_exp2 <- subset(m_nat_exp, variable >= input$years[1] & variable <= input$years[2] )
    
  # Uninsured Plot
  m_nat_exp2 %>%
  filter(First.Level == data) %>%
  group_by_(.dots = c("variable", level)) %>%
  summarize(Total = sum(value)) %>%
  ggplot(., aes_string(x = "variable", y = "Total", fill = level)) + geom_area(position = "stack") + labs(title = "National Health Expenditures", caption = "(data from https://www.cms.gov)", y = "Total (Millions USD)", x = "Year")
  

    
  })
}

# Run app 
shinyApp(ui, server)

