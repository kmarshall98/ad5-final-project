# AD5 Final Project 
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library(shiny)
library(dplyr)

# data sets
census_data <- read.csv("./data/census_county_data.csv")
education_data <- read.csv("./data/education_by_county.csv")


# UI
ad5_ui <- navbarPage("AD5 Final",
     tabPanel("Introduction"),
     tabPanel("Question One"),
     tabPanel("Question Two"),
     tabPanel("Question Three"),
     tabPanel("Question Four"),
     tabPanel("References")
)

# Server
ad5_server <- function(input, output){
  
}

# Shiny App 
shinyApp(ui = ad5_ui, server = ad5_server)