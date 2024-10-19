
library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  

  fluidRow(
    column(
      width = 4,
      selectizeInput(
        inputId = 'multi',
        options = list(maxItems = 5),
        label = tags$span(
          "Characteristics to Visualize"
        ),
        choices = c("Independent", 
                    "Republican",
                    "Democrat", 
                    "Conservative",
                    "Liberal",
                    "Moderate",
                    "Latino",
                    "18-29 years",
                    "30-45 years",
                    "45-65 years",
                    "65+ years",
                    "Male", "Female",
                    "Non Hispanic White", 
                    "Other Race",
                    "Authoritarian",
                    "Non-Authoritarian",
                    "Low Racial Resentment",
                    "High Racial Resentment",
                    "College Degree",
                    "Less than College Degree",
                    "Christian",
                    "Non Christian",
                    "Kids in Home",
                    "No Kids in Home",
                    "More than $80k",
                    "Less than $80k",
                    paste0("CD", c(1:9))
        ), 
        multiple = TRUE
      )
    ),
    column(
      width = 6,
      selectInput(
        inputId = "surveyweight",
        choices = c("No Survey Weights", "Survey Weights"),
        selected = "survey_weight",
        label = tags$span(
          "Apply Survey Weights"
        )
      )
    ),
  ),
  br(),br(),
  fluidRow(
    column(
      width = 6,
      tags$h4("Arizona, like other southwestern states, is facing cuts to its water supply. To what extent do you believe that the water supply in the state is a problem? ", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_supply")
    ),
    column(
      width = 6,
      tags$h4("To what extent do you support mandatory water conservation strategies, such as taxing  excessive water usage?", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      
      plotlyOutput(outputId = "hist_tax")
    ),
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$h4("To what extent do you support mandatory water conservation strategies, such as limiting the amount of water usage per household?", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      
      plotlyOutput(outputId = "hist_limit")
    )
  ),
  # todo (weber): These are referenced here but not defined in the server.R file. 
  # fluidRow(
  #   column(
  #     width = 6,
  #     plotlyOutput(outputId = "hist_separate")
  #   ),
  #   column(
  #     width = 6,
  #     plotlyOutput(outputId = "hist_citizen")
  #   )
  # ),
)