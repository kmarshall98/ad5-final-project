# AD5 Final Project 
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library(shiny)


# UI
ad5_ui <- fluidPage(
  # app title
  titlePanel("Analysis Report on the relationship between Education and Success"),
  p("AD5 Final Project by Kendall Marshall, James Hovet, Thomas Luk, Dan Palanchuck"),
  p("3/11/2019")
)

# Server
ad5_server <- function(input, output){
  
}

# Shiny App 
shinyApp(ui = ad5_ui, server = ad5_server)