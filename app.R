# AD5 Final Project
# Kendall Marshall, James Hovet, Thomas Luk, and Dan Palanchuk

# Necessary Packages
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("rsconnect")
library("tibble")
library("hexbin")


# Data wrangling for entire report
education_by_county <- read.csv("./data/education_by_county.csv", stringsAsFactors = FALSE) #dataframe containing education data
census_county_data <- read.csv("./data/census_county_data.csv", stringsAsFactors = FALSE) #dataframe containing demographics
colnames(education_by_county) <- c("FIPS.Code", "state", "area_name", "less_than_high", "high_only", "some_college", "bachelors", "per_less_than_high", "per_high_only", "per_some_college", "per_bachelors")
joined <- inner_join(education_by_county, census_county_data, by = "FIPS.Code")
joined <- mutate(joined, Non_White = 100 - White)
joined_national <- add_row(joined, State = "National")
state_list <- joined_national$State
income_range <- range(joined$Income, na.rm = TRUE)
education_level_list <- c("Less than High School", "High School Only", "Some College", "Bachelors")
education_level <- list("Less than High School" = "per_less_than_high", "High School Only" = "per_high_only", "Some College" = "per_some_college", "Bachelors" = "per_bachelors")
industries <- c("Professional", "Service", "Office", "Construction", "Production")


# UI
ad5_ui <- navbarPage(
  "AD5 Final",
  # Tab Panel for Introduction
  tabPanel("Introduction",
    titlePanel("Analyzing Education's Impact on Success"),
    imageOutput("cover_image"),
    # line breaks so text doesn't cover image
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    h3(em("Why does this matter?")),
    textOutput(outputId = "intro_msg"),
    h4("References"),
    p("1).  https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/"),
    br(),
    p("2).  https://www.kaggle.com/muonneutrino/us-census-demographic-data#acs2015_county_data.csv")
  ),
  # Tab panel for question 1
  tabPanel(
    "Education and Income",
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        titlePanel("Select Data"),
        selectInput(
          inputId = "educ_level",
          label = "Select Level of Education",
          choices = education_level_list,
          selected = "per_bachelors"
        ),
        selectInput(
          inputId = "state",
          label = "Select State",
          choices = state_list,
          selected = "National"
        )
      ),
      mainPanel(
        titlePanel("Do Counties with a Higher Education Level Correlate to Having Higher Income per Capita?"),
        plotOutput("question_one_plot"),
        textOutput(outputId = "question_one_msg"),
        p(""),
        p("There is a strong positive correlation of 0.71 between the percent of the population with a bachelor’s degree and the income per capita for counties within the United States. In contrast, there is a strong negative correlation of -0.67 between percent of population with less than a high school education and income per capita. From these observations, it can be inferred that more educated counties tend to have a higher income per capita than the less educated counties and vice versa. "),
        p("However, despite the strong correlation, there are several outliers that have to be mentioned. In the graph of “% Population with Bachelors” against “Income Per Capita”, there is a county where only 14.20% of the population has a bachelors degree, which is 0.35% lower than the national average of 14.55%. However, this county’s income per capita is about $51000, which exceeds the national average of $24000 by $27000. It turns out that this county is North Slope Borough of Alaska, home of the Prudhoe Bay Oil Field. Oil rigs workers make nearly $100,000 a year and many jobs do not require a college degree (Hargreaves). "),
        p("According to the data, unless you are planning to work in an oil rig, having a higher level of education does in fact lead to a higher income. "),
        p(""),
        p("Hargreaves, Steve. “Oil Rig Workers Make Nearly $100,000 a Year.” CNNMoney, Cable News Network, 10 May 2012, money.cnn.com/2012/05/10/news/economy/oil_workers/index.htm.")
        # tableOutput("question_one_table")
      )
    )
  ),
  # Tab Panel for Question 2
  tabPanel(
    "Commute Times vs. Education",
    sidebarLayout(
      sidebarPanel(
        # allows the user to narrow the data to a certain state
        selectInput("state_choice",
          label = "Select State:",
          choices = state_list,
          selected = "National"
        ),
        # allows the user to only have the graph display a certain range of incomes
        sliderInput("income_slider",
          label = "Income Range",
          min = income_range[1],
          max = income_range[2],
          value = income_range
        )
      ),
      # displays a plot with an analysis
      mainPanel(
        titlePanel("How does level of education impact commute times?"),
        plotOutput("question_two", click = "plot_click"),
        verbatimTextOutput("info"),
        h3("Analysis of Scatter Plot"),
        textOutput(outputId = "question_two_msg")
      )
    )
  ),
  # Tab Panel for Question 3
  tabPanel(
    "Race and College Access",
    sidebarLayout(
      sidebarPanel(
        selectInput("state_q3",
                    label = "Select State:",
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
       p("According to our data, nationally at least, a high school education at least is a near-guarantee for 
        those counties that are almost exclusivly white, where that is nowhere near the case for those counties with 
        less primarily-white makeups. However, interestingly enough, the counties with the highest percentage of 
        people with bachelors degrees or higher were in those counties with closer to 75-80% white populations, possibly 
        suggesting that those places with higher levels of higher education are cities with more cosmopolitan makeups."),
       p("Perhaps not suprisingly, but certianly unfortunatly, those places with the highest levels of high-school dropouts 
        are the most non-white counties, although it is important to not that high levels of high-school dropouts are to be 
        found in predominantly white counties as well.")
       )
     )
  ),
  # Tab Panel for Question 4
  tabPanel(
    "Education Level vs. Industry Makeup",
     sidebarLayout(
      sidebarPanel(
        selectInput("state_q4",
                    label = "Select State:",
                    choices = state_list,
                    selected = "National"
        ),
        selectInput(inputId = "industry_q4",
                    label = "Select Industry",
                    choices = industries,
                    selected = "Professional"
        ),
        selectInput(inputId = "educ_level_q4",
                    label = "Select Level of Education",
                    choices = education_level_list,
                    selected = "per_bachelors"
        )
     ),
     mainPanel(
      titlePanel("How does level of education coorelate with industry makeup?"),
      plotOutput("question_four_plot"),
      p("Below is the industry makeup for the mean county in the selected area"),
      tableOutput("question_four_table"),
       p("Nationwide, those counties with higher education levels have a higher representation of professionals in their county makeup.
         Unsuprisingly, counties with a high percentage of college dropouts have much less representaion of professionals and higher 
         representation of Service and construction."),
       br(),
       p("From these numbers, we can conclude that those places where a high percentage of residents work in higher-paying industries have
         higher rates of college education, suggesting that there is a correlation between high-paying industries and levels of education.")
      )
    )
  )
)


# Server
ad5_server <- function(input, output) {
  # INTRODUCTION
  # renders an image of Suzzallo Library of the University of Washington in introduction panel
  output$cover_image <- renderImage({
    return(list(src = "image/suzzallo.jpg", filetype = "image/jpeg", alt = "Suzzallo Library"))
  }, deleteFile = FALSE)
  # Puts an introductory paragraph in the introduction panel
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
    if (input$state != "National") {
      joined <- joined %>%
        filter(State == input$state)
    }
    education_income <- ggplot(data = joined) +
      geom_hex(mapping = aes_string(x = education_level[[input$educ_level]], y = "IncomePerCap")) +
      geom_smooth(mapping = aes_string(x = education_level[[input$educ_level]], y = "IncomePerCap")) +
      labs(title = paste("Prominence of", input$educ_level, "and Income per Capita"), x = paste("% Population with", input$educ_level),
            y = "Income Per Capita")
    education_income
  })
  output$question_one_table <- renderTable({
    if (input$state != "National") {
      joined <- joined %>%
        filter(State == input$state)
    }
    income_table <- joined %>%
      select(State, County, IncomePerCap, education_level[[input$educ_level]])
    income_table
  })
  output$question_one_msg <- renderText({
    if (input$state != "National") {
      joined <- joined %>%
        filter(State == input$state)
    }
    joined <- joined %>%
      select(State, County, IncomePerCap, education_level[[input$educ_level]])
    q1_msg <- paste0(
      "The correlation of this data is ", round(cor(joined[[education_level[[input$educ_level]]]], joined$IncomePerCap), digits = 2),
      ", the average % ", input$educ_level, " is ", round(mean(joined[[education_level[[input$educ_level]]]]), digits = 2),
      " and the average Income Per Capita is ", round(mean(joined$IncomePerCap), digits = 2), ". "
    )
    q1_msg
  })

  # QUESTION 2
  # renders a plot from the user input, a tool box that gives info to the user, and an analysis of the data for panel of Education vs Commute
  output$question_two <- renderPlot({
    if (input$state_choice == "National") {
      state_choice_df <- joined
    } else {
      state_choice_df <- filter(joined, State == input$state_choice)
    }
    state_choice_df <- filter(state_choice_df, Income > input$income_slider[1] & Income < input$income_slider[2])
    # scatter plot of commute times based on percent bachelors degree for Education vs commute panel
    commute_plot <- ggplot(data = state_choice_df) +
      geom_point(mapping = aes(x = per_bachelors, y = MeanCommute, color = Income, size = TotalPop)) +
      labs(title = "Plot of Commute times based on Education", x = "% Population with a Bachelors Degree or Higher", y = "Mean Commute Time") +
      scale_color_gradientn(colours = rainbow(5))
    commute_plot
  })
  # Plot helper for user for Education vs commute panel
  output$info <- renderText({
    paste0("Data Point Information\nPercent of Bachelors Degree(%): ", input$plot_click$x, "\nMean Commute Time(minutes): ", input$plot_click$y)
  })
  # puts a analysis paragraph in Education vs commute panel
  output$question_two_msg <- renderText({
    q3_msg <- "The above scatter plot plots the county's data on the percent of the population with
            bachelors degrees or higher on the x-axis and the average commute time for the county's.
            The visualization defaults to display all of the counties but it can also plot specific states.
            The reason why this question is asked is because often times a measure of one's success is
            how close they are to their work. For example major cities like New York, Seattle, Los Angelos,
            and San Francisco are so expensive to live in these desired locations that employees of companies
            within these cities have long commutes. Those with higher incomes can choose to live close
            to their places of work and can afford to have these short commute times if they choose. Despite my prediction
            that the higher degree means higher sucess and therefore shorter commute times, the data somewhat opposes this theory.
            Looking at the default graph that plots every county, there does seem to be a slight positive correlation between
            education level percent and average commute times, meaning that counties with higher degrees have longer commute times.
            (Note this is the slightest correlation with a very insignificant correlation coefiicient of around +0.1)
            The reason why there isnt a strong correlation in any way, is most liekly due to the fact that county's
            often have large amounts of diversity. There are such strong differences within each county with nice parts and not
            so nice parts that its hard to make any conclusions. The major flaw of this data visualization is the lack of a
            larger sampling pool. If the data was from each district and part of a city, I believe that there would be a stronger
            correlation, however there are some signicant circumstances that contradict my belief of greater sucess means greater
            income shown through outliers. Some counties may be great neighbor hood suburbs with highly sucessful people but choose
            not to live near their work. Looking at the county's across the nation, when just looking at the county's that on
            average has incomes over $100,000, most of these cities have average commute times of over or near 30 minutes. This
            is most likely county's that are nice subarbs somewhat near large cities(20-40 minutes away). Looking at the lowest of incomes of the nation
            there actually seems to be a slight correlation between having more of the population with a degree and shorter
            commute averages. The purpose of this graph was to demonstrate that the greater education means greater sucess in terms
            of commute times, however from the graph it is clear that commute times do not correlate between sucess. This data
            visualization though still provides valuable information. The color of each point is associated with income(red being the lowest and
            blue being the highest income for the county averages). What is interesting when just looking at the blue points from the entire nation
            is that from the highest incomed counties show that the more bachelor's degree percentage, their commute seems to decrease. This is
            most obvious shown in the highest income range, but when looking at the other income groups there seems to be the same trend.
            From the data visualization it is quite possible that within income ranges, commute times actually do decrease with more education, this however
            is just a possible theory and when changing the ranges there are still a lot of outliers. Although this plot does not confirm that
            more education correlates to less commute time (a factor of sucess), the graph does demonstrate
            significant correlation between higher percentage of bachelors degrees with higher incomes to re-confirm the previous plot."
  q3_msg #returns analysis paragraph
  })

  #QUESTION THREE
  output$question_three_plot <- renderPlot({
    if (input$state_q3 != "National") {
      joined <- joined %>%
        filter(State == input$state_q3)
    }
    race_plot <- ggplot(data = joined) +
      geom_hex(mapping = aes_string(x = "White", y = education_level[[input$educ_level_q3]])) +
      labs(title = "Levels of Education vs. Percentage of County That is White", x = "Percentage of County that is White", y = paste("Percentage of County where Highest \nLevel of Education is", input$educ_level_q3))
    race_plot
  })
  output$question_three_table <- renderTable({
    if (input$state_q3 != "National") {
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
  
  # QUESTION FOUR
  output$question_four_plot <- renderPlot({
    if(input$state_q4 != "National") {
      joined <- filter(joined, State == input$state_q4)
    }
    industry_plot <- ggplot(data = joined) +
      geom_point(mapping = aes_string(x = education_level[[input$educ_level_q4]], y = input$industry_q4), alpha = 0.3) +
      labs(title = "Levels of Education vs. industry makeup", y = paste("Percentage of population who work in the", input$industry_q4, "industry"), x = paste("Percentage of County where Highest \nLevel of Education is", input$educ_level_q4))    
    industry_plot
  })
  output$question_four_table <- renderTable({
    if(input$state_q4 != "National") {
      joined <- filter(joined, State == input$state_q4)
    }
    output <- summarise(joined,
                        mean_professional = mean(Professional),
                        mean_service = mean(Service),
                        mean_office = mean(Office),
                        mean_construction = mean(Construction),
                        mean_production = mean(Production)
                        )
    colnames(output) <- industries
    output
  })
}


# Shiny App
shinyApp(ui = ad5_ui, server = ad5_server)
