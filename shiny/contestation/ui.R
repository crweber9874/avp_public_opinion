
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
        choices = list( `Party Identification` = list("Democrat", "Independent", "Republican"), 
                        `Age` = list("18-29 years", "30-44 years", "45-64 years", "65+ years"),
                        `Psychological Variables` = list("High Social Conformity", "Low Social Conformity",
                                                         "High Racial Resentment", "Low Racial Resentment"),
                        `Education` = list("Less than College Degree", "College Degree"),
                        `Gender` = list("Female", "Male"),
                        `Household Income` = list("Less than $80k", "More than $80k"),
                        `Ideology` = list("Liberal", "Moderate", "Conservative"),
                        `Kids in Home` = list("No Kids in Home", "Kids in Home"),
                        `Race/Ethnicity` = list("Latino", "Non Hispanic White", "Other Race or Ethnic Group"),
                        `Religious Denomination` = list("Christian", "Non Christian"),
                        `Congressional District (CD)` = list("CD1", "CD2", "CD3", "CD4", "CD5", "CD6", "CD7", "CD8", "CD9")
                        
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