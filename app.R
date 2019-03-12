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
  tabPanel("Introduction",
           titlePanel("Analyzing Education's Impact on Success"),
           imageOutput("cover_image"),
           br(),
           br(),
           br(),
           br(),
           br(),
           br(),
           h3(em("Why does this matter?")),
           textOutput(outputId = "intro_msg")),
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
        plotOutput("question_two"),
        textOutput(outputId = "question_two_msg")
      )
    )
  ),
  tabPanel("Race and College Access",
           sidebarLayout(
             sidebarPanel(
               titlePanel("Select the State:"),
               selectInput("state_q3",
                           label = "Select:",
                           choices = state_list,
                           selected = "National"
               ),
               selectInput(inputId = "educ_level_q3",
                           label = "Select Level of Education",
                           choices = education_level_list,
                           selected = "per_bachelors")

             ),
             mainPanel(
               titlePanel("How does race makeup coorelate with education levels?"),
               plotOutput("question_three_plot"),
               p("Below are the education levels for the mean county in the selected area"),
               tableOutput("question_three_table"),
               p("According to our data, nationally at least, a high school education at least is a near-guarantee for those counties that are almost exclusivly white, where that is nowhere near the case for those counties with less primarily-white makeups. However, interestingly enough, the counties with the highest percentage of people with bachelors degrees or higher were in those counties with closer to 75-80% white populations, possibly suggesting that those places with higher levels of higher education are cities with more cosmopolitan makeups."),
               p("Perhaps not suprisingly, but certianly unfortunatly, those places with the highest levels of high-school dropouts are the most non-white counties, although it is important to not that high levels of high-school dropouts are to be found in predominantly white counties as well.")
             )
           )
  ),
  tabPanel("Question Four"),
  tabPanel("References")
)

# Server
ad5_server <- function(input, output) {
  #INTRODUCTION PAGE
  output$cover_image <- renderImage({
    return(list(src = "image/suzzallo.jpg", filetype = "image/jpeg", alt = "Suzzallo Library"))
  }, deleteFile = FALSE)

  output$intro_msg <- renderText({
    background_msg <- "In this report we are looking at data that gives information from different
                      cities in the United States on useful employment statistics, these include average
                      income, demographics, level of education rates, and more. The main purpose of this report
                      is to analyze trends of level of education and its relationship to income/how sucessful one is.
                      It is usually assumed that college is the gateway to the middle class, but for many people,
                      the cost of attending university is a significant restriction. This domain is worth analyzing because
                      almost every prospective college student has to weigh in on whether the increased income is worth the time
                      and monetary investment of getting a college education. Through our ideas of what factors makes one sucessful through
                      information like commute times, type of job, income, race we investigate whether education correlates to our
                      beliefs of sucess. In this data report we plan to demonstrate that education does correlate to sucess through
                      analyzing the United States Census Data and data on education from the counties across the nation by evaluating
                      data visulizations on our factors of sucess"
  })



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

  output$question_two_msg <- renderText({
  q2_msg <- "The above scatter plot plots the county's data on the percent of the population with
            bachelors degrees or higher on the x-axis and the average commute time for the county's.
            The visualization defaults to display all of the counties but it can also plot specific states.
            The reason why this question is asked is because often times a measure of one's success is
            how close they are to their work. For example major cities like New York, Seattle, Los Angelos,
            and San Francisco are so expensive to live in these desired locations that employees of companies
            within these cities have long commutes. Those with higher incomes can choose to live close
            to their places of work and can afford to have these short commute times if they choose.Despite my prediction
            that the higher degree means higher sucess and therefore shorter commute times, the data somewhat opposes this theory.
            Looking at the default graph that plots every county, there doesnt seem to be a slight positive correlation between
            education level percent and average commute times, meaning that counties with higher degrees have longer commute times.
            The reason why there isnt a strong correlation in one way, is most liekly due to the fact that county's
            often have large amounts of diversity. There are such strong differences within each county with nice parts and not
            so nice parts that its hard to make any conclusions. The major flaw of this data visualization is the lack of a
            larger sampling pool. If the data was from each district and part of a city, I believe that there would be a stronger
            correlation, however there are some signicant circumstances that contradict my belief of greater sucess means greater
            income shown through outliers. Some counties may be great neighbor hood suburbs with highly sucessful people but choose
            not to live near their work. Looking at the county's across the nation, when just looking at the county's that on
            average has incomes over $100,000, most of these cities have average commute times of over or near 30 minutes. This
             is most likely countys that are nice subarbs somewhat near large cities. Looking at the lowest of incomes of the nation
            there actually seems to be a slight correlation between having more of the population with a degree and shorter
            commute averages. The purpose of this graph was to demonstrate that the greater education means greater sucess in terms
            of commute times, however from the graph it is clear that commute times do not correlate between sucess. This data
            visualization though still provides valuable information. The color of each point is associated with income. The lighter
            dots are higher incomes where as the darker dots are lower incomed county's. From the data visualization there is a
            significant correlation between higher percentage of bachelors degrees with higher incomes like the previous plot."
  })

  #QUESTION THREE

  output$question_three_plot <- renderPlot({
    if(input$state_q3 != "National") {
      joined <- joined %>%
        filter(State == input$state_q3)
    }

    race_plot <- ggplot(data = joined) +
      geom_hex(mapping = aes_string(x = "White", y = education_level[[input$educ_level_q3]])) +
      labs(title = "Levels of Education vs. Percentage of County That is White", x = "Percentage of County that is White", y = paste("Percentage of County where Highest \nLevel of Education is", input$educ_level_q3))

    race_plot
  })

  output$question_three_table <- renderTable({
    if(input$state_q3 != "National") {
      joined <- joined %>%
        filter(State == input$state_q3)
    }

    output <- summarise(joined,
                        mean_less_than_high = mean(per_less_than_high),
                        mean_high_only = mean(per_high_only),
                        mean_some_college = mean(per_some_college),
                        mean_bachelors = mean(per_bachelors))


    colnames(output) <- education_level_list <- c("Less than High School", "High School Only", "Some College", "Bachelors")

    output
  })


}

# Shiny App
shinyApp(ui = ad5_ui, server = ad5_server)
