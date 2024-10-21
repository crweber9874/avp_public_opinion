
library(htmltools)
library(shinythemes)
library(plotly)
library(shiny)

ui <- fluidPage(
  theme = shinytheme("cosmo"),  

  fluidRow(
    column(
      width = 7,
      height = 7,
      selectizeInput(
        inputId = 'group',
        selected = "party_identification3",
        options = list(maxItems = 1),
        label = tags$span(
          "Group Characteristics to Display"
        ),
        choices = c("Party Identification" = "party_identification3",
                    "Political Ideology" = "conservative3",
                    "Education" = "college",
                    "Racial Resentment" = "rr",
                    "White" = "white",
                    "Social Conformity" = "authoritarianism",
                    "Gender" = "female",
                    "Kids in Home" = "kids_in_home",
                    "Family Income" = "faminc",
                    "Latino" = "latino",
                    "Religion" = "christian"),
              
        multiple = FALSE,
    )
    ),
  ),
  br(),
  fluidRow(
    column(
      offset = 1,
      width = 5,
      height = 6,
      plotlyOutput("feelings", height = 600, width = 600),
    )
  ),
  fluidRow(
    column(
      width = 10,
      offset = 1,
      plotlyOutput("radar", height = "400px", width = "90%")  # Use percentage for width
    )
  )
  


  # br(),
  # 
  # tags$div(
  #   tags$h3("Notes:"),
  #   tags$b("Visualize similarities and differences between groups in the 
  #           Arizona Voter Survey."),
  #   tags$b("Use the dropdown menus to visualize different groups and choose to apply survey weights. Each graph has interactive features, 
  #           where axes can be rescaled, the user can zoom in and out, and hover over data points to see the exact values. The predictions 
  #           for each group were generated from a statistical model called the ordered logistic regression model. 
  #           The models were estimated in the programming language R, using rstan. The predictions are the predicted 
  #           probability of choosing a given category for a particular group.")
  # )
)