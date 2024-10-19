
library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  
  
  fluidRow(
    column(
      width = 6,
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
  )
  ),
  br(),br(),
  fluidRow(
    column(
      width = 6,
      tags$h3("Number of immigrants who are allowed into the United States ", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_immigration")),
    column(
      width = 6,
      tags$h3("The United States should end the policy of granting citizenship to children of  foreign born in the U.S.", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_citizen"))
  ),
  fluidRow(
    column(
      width = 6,
      tags$h3("End the separation of parents and children at the border", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_separate")),
    
    column(
      width = 6,
      tags$h3("Increase spending on smart technology", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_smart")),
  ),
  fluidRow(
    column(
      width = 6,
      tags$h3("Californians moving to the state is adversely affecting Arizona", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_cali")),
    column(
      width = 6,
      tags$h3("International migration to the state is adversely affecting Arizona", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_az_imm")
    )
  ),
)