
library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  
  
  tags$head(
    tags$style(HTML("
      .vertical-line {
        border-left: 2px solid #000;
        height: 100%;
      }
    "))
  ),
  
  tags$div(
    tags$h2("Immigration", style = "color: black; font-weight: bold;")
  ),
  
  tags$hr(),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      class = "vertical-line"
    ),
    column(
      width = 6,
      selectizeInput(
        inputId = 'multi',
        options = list(maxItems = 5),
        label = tags$span(
          "Choose up to Five Group Characteristics to Display", style = "font-size: 1.5em; font-style: bold;"
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
          "Apply Survey Weights", style = "font-size: 1.8rem; font-style: normal;"
        )
      )
    )
  ),
   fluidRow(
    column(
      width = 6,
      tags$div(
        tags$b("Explore characteristics of the Arizona Voter Survey, a representative sample of Arizona voters. 
                Each graph includes interactive features. You may zoom in and out, rescale axes, and hover over data points to show their values.", 
               style = "font-size: 2rem; font-weight: normal;")
      )
    ),
    column(
      width = 6,
      plotlyOutput(outputId = "hist_immigration")
    )
  ),
  

  br(),
  
  fluidRow(
    column(
      width = 6,
      plotlyOutput(outputId = "hist_separate")
    ),
    column(
      width = 6,
      plotlyOutput(outputId = "hist_citizen")
    )
  ),
  
  br(),
  
  tags$div(
    tags$h3("Notes:"),
    tags$b("Visualize similarities and differences between groups in the 
            Arizona Voter Survey."),
    tags$b("Use the dropdown menus to visualize different groups and choose to apply survey weights. Each graph has interactive features, 
            where axes can be rescaled, the user can zoom in and out, and hover over data points to see the exact values. The predictions 
            for each group were generated from a statistical model called the ordered logistic regression model. 
            The models were estimated in the programming language R, using rstan. The predictions are the predicted 
            probability of choosing a given category for a particular group.")
  )
)