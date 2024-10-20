
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
      width = 4,
      selectInput(
        inputId = "surveyweight",
        choices = c("No Survey Weights", "Survey Weights"),
        selected = "survey_weight",
        label = tags$span(
          "Apply Survey Weights"
        )
      )
    )
  ),
  
  br(),br(),

  fluidRow(
    column(
      width = 6,
      tags$h3("Support ballot recounts", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_recount")
    ),
    column(
      width = 6,
      tags$h3("Contest the outcome in the courts ", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_court")
    )
  ),
  fluidRow(
    column(
      width = 6,
      tags$h3("Publicly criticize the integrity or fairness of the election", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_criticize")
    ),
    column(
      width = 6,
      tags$h3("Attend a march or demonstration", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_march")
    )
  ),
  fluidRow(
    column(
      tags$h3("State legislators refusing to certify the election results", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      width = 6,
      plotlyOutput(outputId = "hist_cert")
    ),
    column(
      width = 6,
      tags$h3("Burn the American Flag", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_burn")
    )
  ),
)