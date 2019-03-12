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


joined_national <- add_row(joined, State = "National")
state_list <- joined_national$State

income_range <- range(joined$Income, na.rm = TRUE)



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
  
  tabPanel("Commute Times vs. Education",
    sidebarLayout(
      sidebarPanel(
        titlePanel("Select the State:"),
        selectInput("state_choice", 
                    label = "Select:", 
                    choices = state_list,
                    selected = "National"
        ),
        sliderInput("income_slider", 
                    label = "Income Range",
                    min = income_range[1],
                    max = income_range[2],
                    value = income_range
        )
      ),
      mainPanel(
        titlePanel("How does level of education impact commute times?"),
        plotOutput("question_two")
      )
    )
  ),
  tabPanel("Question Three"),
  tabPanel("Question Four"),
  tabPanel("References")
)

# Server
ad5_server <- function(input, output) {
  #QUESTION ONE 
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
  
  # QUESTION TWO
  output$question_two <- renderPlot({
    if(input$state_choice == "National") {
      state_choice_df <- joined
    } else {
      state_choice_df <- filter(joined, State == input$state_choice)  
    }
    state_choice_df <- filter(state_choice_df, Income > input$income_slider[1] & Income < input$income_slider[2])
    commute_plot <- ggplot(data = state_choice_df) +
      geom_point(mapping = aes(x = per_bachelors, y = MeanCommute, color = Income, size = TotalPop)) +
      labs(title = "Plot of Commute times based on Education", x = "% Population with a Bachelors Degree or Higher", y = "Mean Commute Time") 
      
    commute_plot
  })
}

# Shiny App
shinyApp(ui = ad5_ui, server = ad5_server)
