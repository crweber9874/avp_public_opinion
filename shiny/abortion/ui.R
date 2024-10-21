
library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  
  
  fluidRow(
    column(
      width = 12,
      class = "vertical-line"
    ),
    column(
      width = 4,
      selectizeInput(
        inputId = 'multi',
        options = list(maxItems = 5),
        label = tags$span(
          "Characteristics to Visualize",
        ),
        choices = list(`Age` = list("18-29 years", "30-44 years", "45-64 years", "65+ years"),
                       `Child-Rearing` = list("Authoritarian", "Non-Authoritarian"),
                       `Congressional District (CD)` = list("CD1", "CD2", "CD3", "CD4", "CD5", "CD6", "CD7", "CD8", "CD9"),
                       `Education` = list("Less than College Degree", "College Degree"),
                       `Gender` = list("Female", "Male"),
                       `Household Income` = list("Less than $80k", "More than $80k"),
                       `Ideology` = list("Liberal", "Moderate", "Conservative"),
                       `Kids in Home` = list("No Kids in Home", "Kids in Home"),
                       `Party Identification` = list("Democrat", "Independent", "Republican"), 
                       `Race/Ethnicity` = list("Latino", "Non Hispanic White", "Other Race"),
                       `Racial Resentment` = list("Low Racial Resentment", "High Racial Resentment"),
                       `Religious Denomination` = list("Christian", "Non Christian")
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
      tags$h3("Abortion Legalization", style = "width: 100%; text-align: center;"),
      plotlyOutput(outputId = "hist_legal")
      
    ),
    column(
      width = 6,
      tags$h3("Support or Oppose Mandatory 2-5 Year Prison Sentence for Abortion Providers", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_jail")
    )
  ),
 )
