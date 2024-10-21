
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
                        `Race/Ethnicity` = list("Latino", "Non Hispanic White", "Person of Color"),
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
      tags$h4("Number of immigrants who are allowed into the United States ", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_immigration")),
    column(
      width = 6,
      tags$h4("The United States should end the policy of granting citizenship to children of  foreign born in the U.S.", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_citizen"))
  ),
  fluidRow(
    column(
      width = 6,
      tags$h4("End the separation of parents and children at the border", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_separate")),
    
    column(
      width = 6,
      tags$h4("Increase spending on smart technology", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_smart")),
  ),
  fluidRow(
    column(
      width = 6,
      tags$h4("Californians moving to the state is adversely affecting Arizona", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_cali")),
    column(
      width = 6,
      tags$h4("International migration to the state is adversely affecting Arizona", style = "width: 100%; text-align: center; padding: 0px 75px 0px 75px;"),
      plotlyOutput(outputId = "hist_az_imm")
    )
  ),
)