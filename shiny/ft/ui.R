
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
        choices = list(
          `Politics` = list("Party Identification" = "party_identification3",
                            "Political Ideology" = "conservative3"),
          `Psychology` = list("Social Conformity" = "authoritarianism",
                              "Racial Resentment" = "rr"),
          `Demographics` = list("Education" = "college",
                              "Race" = "white",
                              "Gender" = "female",
                              "Kids in Home" = "kids_in_home",
                              "Family Income" = "faminc",
                              "Latino" = "latino",
                              "Religion" = "christian")),
        multiple = FALSE,
    )
    ),
  ),
  br(),
  fluidRow(
    column(
      width = 5,
      plotlyOutput("feelings", height = 600),
    ),
    column(
      width = 7,
      plotlyOutput("radar")
    )
  )
)