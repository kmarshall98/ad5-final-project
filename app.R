# AD5 Final Project
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("rsconnect")

education_by_county <- read.csv("./data/education_by_county.csv", stringsAsFactors = FALSE)
colnames(education_by_county) <- c("FIPS.Code", "state", "area_name", "less_than_high", "high_only", "some_college", "bachelors", "per_less_than_high", "per_high_only", "per_some_college", "per_bachelors")
census_county_data <- read.csv("./data/census_county_data.csv", stringsAsFactors = FALSE)
joined <- inner_join(education_by_county, census_county_data, by = "FIPS.Code")
joined <- mutate(joined, Non_White = 100 - White)

# data sets
census_data <- read.csv("./data/census_county_data.csv")
education_data <- read.csv("./data/education_by_county.csv")


# UI
ad5_ui <- navbarPage(
  "AD5 Final",
  tabPanel("Introduction"),
  tabPanel(
    "Question One",
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        titlePanel("Select Data")
      ),     
      mainPanel(
        titlePanel("Prominence of Bachelors Degrees and Income per Capita"),
          plotOutput("question_one"),
          p("Hello world!")
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
  output$question_one <- renderPlot({
    bachelors_income <- ggplot(data = joined) +
      geom_hex(mapping = aes(x = per_bachelors, y = IncomePerCap)) +
      labs(title = "Prominence of Bachelors Degrees and Income per Capita", x = "% Population with Bachelors Degree or Higher", y = "Income Per Capita")
    bachelors_income
  })
}

# Shiny App
shinyApp(ui = ad5_ui, server = ad5_server)
