library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  
  
  
  tags$head(
    tags$style(HTML("
      .navbar-default {
        background-color: #0C234B;  /* Change this to your desired color */
        border-color: #0C234B;      /* Change this to your desired color */
      }
      .navbar-default .navbar-brand {
        color: white;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
    "))
  ),
  
  
  navbarPage(
    title = "Democracy",
    tabPanel(
      "Electoral Contestation",
      tags$div(
        tags$h1("Beliefs about Elections and Democracy", style = "color: black;"),
        tags$h2("How much do you support or oppose each of the following behaviors when people are unhappy with the outcome of an election?", style = "color: black;")
      ),
      
      tags$hr(),
      
      br(),
      
      tags$head(
        tags$style(HTML("
          .vertical-line {
            border-left: 2px solid #000;
            height: 100%;
          }
        "))
      ),
      
      fluidRow(
        column(
          width = 12,
          class = "vertical-line"
        ),
        column(
          width = 4,
          height = 4,
          selectizeInput(
            inputId = 'multi2',
            options = list(maxItems = 5),
            label = tags$span(
              "Choose up to Five Group Characteristics to Visualize", style = "font-size: 1.5em; font-style: bold;"
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
                        ####
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
          width = 8,
          tags$div(
            tags$b("This application allows you to visualize similarities and differences between groups in the Arizona Voter Survey. Plot estimates
           for various groups and choose to apply survey weights. Each graph has interactive features, where axes can be rescaled, the user can zoom in and out,
            and hover over data points to see the predicted values. ", 
                   style = "font-size: 2rem; font-weight: normal;"),
            
          )
        )
        
      ),
      fluidRow(  
        column(
          width = 4,
          selectInput(
            inputId = "surveyweight",
            choices = c("No Survey Weights", "Survey Weights"),
            selected = "survey_weight",
            label = tags$span(
              "Apply Survey Weights", style = "font-size: 1.8rem; font-style: normal;"
            )
          ))),
      

      br(),
      
      fluidRow(
        column(
          width = 6,
          plotlyOutput(outputId = "hist_newelection")
        ),
        column(
          width = 6,
          plotlyOutput(outputId = "hist_certify")
        )
      ),
      
      br(),
      
      tags$div(
        tags$h3("Notes:"),
        tags$b("This application allows you to visualize similarities and differences between groups in the 
        Arizona Voter Survey."),
        tags$b("Use the dropdown menus to visualize different groups and choose to apply survey weights. Each graph has interactive features, 
               where axes can be rescaled, the user can zoom in and out, and hover over data points to see the exact values. The predictions 
               for each group were generated from a statistical model, known as an ordered logistic regression model. 
               The models were estimated using the brms package in R. The predictions are the predicted probability of 
               each group, with a margin of error; in statistics, this is called a 'credibility interval.'")
      )
    ),
    # Main tab
    tabPanel(
      "Electoral Contestation, Continued",
      
      tags$div(
        tags$h1("Beliefs about Elections and Democracy", style = "color: black;"),
        tags$h2("How much do you support or oppose each of the following behaviors when people are unhappy with the outcome of an election?", style = "color: black;")
      ),
      
      tags$hr(),
      
      br(),
      
      tags$head(
        tags$style(HTML("
          .vertical-line {
            border-left: 2px solid #000;
            height: 100%;
          }
        "))
      ),
      
      fluidRow(
        column(
          width = 12,
          class = "vertical-line"
        ),
        column(
          width = 4,
          height = 4,
          selectizeInput(
            inputId = 'multi',
            options = list(maxItems = 5),
       label = tags$span(
              "Choose up to Five Group Characteristics to Visualize", style = "font-size: 1.5em; font-style: bold;"
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
                        ####
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
         width = 8,
         tags$div(
           tags$b("This application allows you to visualize similarities and differences between groups in the Arizona Voter Survey. Plot estimates
           for various groups and choose to apply survey weights. Each graph has interactive features, where axes can be rescaled, the user can zoom in and out,
            and hover over data points to see the predicted values. ", 
                  style = "font-size: 2rem; font-weight: normal;"),
          
         )
       )
       
       ),
      fluidRow(  
      column(
          width = 4,
          selectInput(
            inputId = "surveyweight",
            choices = c("No Survey Weights", "Survey Weights"),
            selected = "survey_weight",
            label = tags$span(
              "Apply Survey Weights", style = "font-size: 1.8rem; font-style: normal;"
            )
          ))),
        

      br(),
      
      fluidRow(
        column(
          width = 6,
          plotlyOutput(outputId = "hist_recount")
        ),
        column(
          width = 6,
          plotlyOutput(outputId = "hist_criticize")
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          plotlyOutput(outputId = "hist_court")
        ),
        column(
          width = 6,
          plotlyOutput(outputId = "hist_burn")
        )
      ),
      
      br(),
      
      tags$div(
        tags$h3("Notes:"),
        tags$b("This application allows you to visualize similarities and differences between groups in the 
        Arizona Voter Survey."),
        tags$b("Use the dropdown menus to visualize different groups and choose to apply survey weights. Each graph has interactive features, 
               where axes can be rescaled, the user can zoom in and out, and hover over data points to see the exact values. The predictions 
               for each group were generated from a statistical model, known as an ordered logistic regression model. 
               The models were estimated using the brms package in R. The predictions are the predicted probability of 
               each group, with a margin of error; in statistics, this is called a 'credibility interval.'")
      )
    ),
    
    # New tab
    tabPanel(
      "The 2020 Election",
      
      tabPanel(
        "The 2020 Election",
        tags$div(
          tags$h1("Beliefs about Elections and Democracy", style = "color: black;"),
          tags$h2("The 2020 Election", style = "color: black;")
        ),
        
        tags$hr(),
        
        br(),
        
        tags$head(
          tags$style(HTML("
          .vertical-line {
            border-left: 2px solid #000;
            height: 100%;
          }
        "))
        ),
        
        fluidRow(
          column(
            width = 12,
            class = "vertical-line"
          ),
          column(
            width = 4,
            height = 4,
            selectizeInput(
              inputId = 'multi3',
              options = list(maxItems = 5),
              label = tags$span(
                "Choose up to Five Group Characteristics to Visualize", style = "font-size: 1.5em; font-style: bold;"
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
                          ####
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
            width = 8,
            tags$div(
              tags$b("This application allows you to visualize similarities and differences between groups in the Arizona Voter Survey. Plot estimates
           for various groups and choose to apply survey weights. Each graph has interactive features, where axes can be rescaled, the user can zoom in and out,
            and hover over data points to see the predicted values. ", 
                     style = "font-size: 2rem; font-weight: normal;"),
              
            )
          )
          
        ),
        fluidRow(  
          column(
            width = 4,
            selectInput(
              inputId = "surveyweight",
              choices = c("No Survey Weights", "Survey Weights"),
              selected = "survey_weight",
              label = tags$span(
                "Apply Survey Weights", style = "font-size: 1.8rem; font-style: normal;"
              )
            ))),
        
        # Add a colum of instruction
        
        br(),
        
        fluidRow(
          column(
            width = 6,
            offset = 5,
            plotlyOutput(outputId = "stolen_2020")
          )
        ),
        
        br(),
        
        tags$div(
          tags$h3("Notes:"),
          tags$b("This application allows you to visualize similarities and differences between groups in the 
        Arizona Voter Survey."),
          tags$b("Use the dropdown menus to visualize different groups and choose to apply survey weights. Each graph has interactive features, 
               where axes can be rescaled, the user can zoom in and out, and hover over data points to see the exact values. The predictions 
               for each group were generated from a statistical model, known as an ordered logistic regression model. 
               The models were estimated using the brms package in R. The predictions are the predicted probability of 
               each group, with a margin of error; in statistics, this is called a 'credibility interval.'")
        )
      )
    )
  )

  
)