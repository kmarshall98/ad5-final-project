# AD5 Final Project
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("rsconnect")
library("tibble")

education_by_county <- read.csv("./data/education_by_county.csv", stringsAsFactors = FALSE)
colnames(education_by_county) <- c("FIPS.Code", "state", "area_name", "less_than_high", "high_only", "some_college", "bachelors", "per_less_than_high", "per_high_only", "per_some_college", "per_bachelors")
census_county_data <- read.csv("./data/census_county_data.csv", stringsAsFactors = FALSE)
joined <- inner_join(education_by_county, census_county_data, by = "FIPS.Code")
joined <- mutate(joined, Non_White = 100 - White)

# data sets
census_data <- read.csv("./data/census_county_data.csv")
education_data <- read.csv("./data/education_by_county.csv")
joined_national <- add_row(joined, State = "National")
state_list <- joined_national$State
education_level_list <- c("Less than High School", "High School Only", "Some College", "Bachelors")
education_level <- list("Less than High School" = "per_less_than_high", "High School Only" = "per_high_only", "Some College" = "per_some_college", "Bachelors" = "per_bachelors")


# UI
ad5_ui <- navbarPage(
  "AD5 Final",
  tabPanel("Introduction"),
  tabPanel(
    "Education and Income",
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        titlePanel("Select Data"),
        selectInput(inputId = "educ_level", label = "Select Level of Education", choices = education_level_list, selected = "per_bachelors"),
        selectInput(inputId = "state", label = "Select State", choices = state_list, selected = "National")
      ),     
      mainPanel(
        titlePanel("Do Counties with a Higher Education Level Correlate to Having Higher Income per Capita?"),
        p("hello"), 
        p("hm"),
          plotOutput("question_one_plot"),
          tableOutput("question_one_table")
          
      )
    )
  ),
  tabPanel("Question Two"),
  tabPanel("Question Three"),
  tabPanel("Question Four"),
  tabPanel("References")
)

# Server
ad5_server <- function(input, output) {
  output$question_one_plot <- renderPlot({
    if(input$state != "National") {
      joined <- joined %>% 
        filter(State == input$state)
    }
    
    education_income <- ggplot(data = joined) +
      geom_hex(mapping = aes_string(x = education_level[[input$educ_level]], y = "IncomePerCap")) +
      labs(title = paste("Prominence of", input$educ_level ,"and Income per Capita"), x = paste("% Population with", input$educ_level), y = "Income Per Capita")
    education_income
  })
  output$question_one_table <- renderTable({
    if(input$state != "National") {
      joined <- joined %>%
        filter(State == input$state)
    }
    income_table <- joined %>% 
      select(State, County,IncomePerCap, education_level[[input$educ_level]])
    income_table
  })
}

# Shiny App
shinyApp(ui = ad5_ui, server = ad5_server)
