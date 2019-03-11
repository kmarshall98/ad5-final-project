# AD5 Final Project 
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library(shiny)
library(dplyr)

# data sets
census_data <- read.csv("./data/census_county_data.csv")
education_data <- read.csv("./data/education_by_county.csv")


# UI
ad5_ui <- fluidPage(
  # app title
  titlePanel("Analysis Report on the relationship between Education and Success"),
  p("AD5 Final Project by Kendall Marshall, James Hovet, Thomas Luk, Dan Palanchuck"),
  p("3/11/2019"),
  h4("Introduction"),
  p("INTRODUCTION OF WHAT THE DATA REPORT IS ANALYZING.......afjsdkfjFHJGSDAKJDFHDSFJOekw
    qehefjkffjaskfsakfnsakjfsjnafndsajfnsdjkfnsdkjfn"),
  sidebarLayout(
    
    sidebarPanel(
       
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("question_one", plotOutput("person_one_plot"), textOutput(outputId = "question_one_info")),
        tabPanel("question_two", plotOutput("person_two_plot"), textOutput(outputId = "question_two_info")),
        tabPanel("question_three", plotOutput("person_three_plot"), textOutput(outputId = "question_three_info")),
        tabPanel("question_four", plotOutput("person_four_plot"), textOutput(outputId = "question_four_info"))
        )
        
    )
  ),
  h4("References:")
)

# Server
ad5_server <- function(input, output){
  
}

# Shiny App 
shinyApp(ui = ad5_ui, server = ad5_server)