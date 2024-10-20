# Immigration
source("functions/functions.R")
source("global.R")


server <- function(input, output) {
  
  create_plot_multi <- function(outcome_name, plot_title, recode_scheme = "oppose5r") {
    
    build_data_outcome <- reactive({
      build_data(item = outcome_name, 
                 recode_scheme = recode_scheme, 
                 prediction_data = dat_immigration,
                 ungrouped_data = full_dat)
    })
    
    df_outcome <- reactive({
      if(input$surveyweight == "No Survey Weights") {
        build_data_outcome()[[2]]
      } else {
        build_data_outcome()[[3]]
      }
    })
    
    model_df_outcome <- reactive({
      build_data_outcome()[[1]]
    })
    
    dat_reactive_outcome <- reactive(NULL)
    
    if (recode_scheme == "agree5r") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category,
                                    `5` = "Strongly Agree", 
                                    `4` = "Agree", 
                                    `3` = "Neither/Not Sure",
                                    `2` = "Disagree", 
                                    `1` = "Strongly Disagree"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category, 
                                    levels = c("Strongly Agree", "Agree", "Neither/Not Sure",
                                               "Disagree", "Strongly Disagree")))
      })
    }
    
    if (recode_scheme == "agree4") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category,
                                    `1` = "Strongly Agree", 
                                    `2` = "Agree", 
                                    `3` = "Disagree", 
                                    `4` = "Strongly Disagree"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category, 
                                    levels = c("Strongly Agree", "Agree", 
                                               "Disagree", "Strongly Disagree")))
      })
    }
    if (recode_scheme == "agree") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category,
                                    `1` = "Strongly Agree", 
                                    `2` = "Agree", 
                                    `3` = "Neither/Not Sure", 
                                    `4` = "Disagree", 
                                    `5` = "Strongly Disagree"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category, 
                                    levels = c("Strongly Agree", "Agree", "Neither/Not Sure",
                                               "Disagree", "Strongly Disagree")))
      })
    }
    if (recode_scheme == "oppose5r") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category, 
                                    `1` = "Strongly Oppose", 
                                    `2` = "Oppose", 
                                    `3` = "Not Sure", 
                                    `4` = "Support", 
                                    `5` = "Strongly Support"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category, 
                                    levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                               "Support", "Strongly Support")))
      })
    }
    if (recode_scheme == "abortion") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category, 
                                    `4` = "Abortion Not Legal",
                                    `3` = "First Trimester",
                                    `2` = "First and Second Trimester",
                                    `1` = "Completely Legal at Any Point"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category, 
                                    levels = c("Abortion Not Legal", 
                                               "First Trimester", 
                                               "First and Second Trimester", 
                                               "Completely Legal at Any Point")))
      })
    }
    if (recode_scheme == "water") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category,
                                    `5` = "Extremely Serious",
                                    `4` = "Very Serious",
                                    `3` = "Not Sure",
                                    `2` = "Somewhat Serious",
                                    `1` = "Not Serious"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category,
                                    levels = c("Extremely Serious",
                                               "Very Serious",
                                               "Not Sure",
                                               "Somewhat Serious",
                                               "Not Serious")))
      })
    }
    if (recode_scheme == "decrease") {
      dat_reactive_outcome <- reactive({
        model_df_outcome() %>%
          mutate(.category = recode(.category,
                                    `5` = "Decrease a lot", 
                                    `4` = "Decrease", 
                                    `3` = "Stay the same", 
                                    `2` = "Increase", 
                                    `1` = "Increase a lot"),
                 .value = as.numeric(.value)) %>%
          mutate(.category = factor(.category,
                                    levels = c("Decrease a lot",
                                               "Decrease",
                                               "Stay the same",
                                               "Increase",
                                               "Increase a lot")))
      })
    }
    add_trace_if_selected <- function(base, condition, data, name, color, symbol = 'circle') {
      if(condition %in% input$multi) {
        base %>%
          add_trace(data = data, x = ~.category, y = ~.value, 
                    showlegend = TRUE,
                    name = name,
                    opacity = 0.5,
                    type = 'scatter',
                    mode = 'markers',
                    marker = list(size = 15, color = color, symbol = symbol),
                    error_y = ~list(array = .upper - .value, 
                                    arrayminus = .value - .lower, 
                                    color = 'grey'),
                    text = ~paste0("<b>", name, "</b>","<br>",
                                   "<b>", "Category: ", "</b>", .category, "<br>",
                                   "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                   "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                   round(.upper, 2),  "]<br>"),
                    hoverinfo = 'text')
      } else {
        base
      }
    }
    
    
    renderPlotly({
      
      df <- df_outcome()
      dat <- dat_reactive_outcome()
      
      base <- plot_ly(df, x = ~.category, 
                      y = ~.value, type = 'bar', 
                      name = "All Participants",
                      text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                     "<b>","Probability: ","</b>", round(.value, 2)),
                      marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                      textposition = "none",
                      hoverinfo =  "text",
                      opacity = 0.3) %>% 
        layout(title = plot_title,
               showlegend = TRUE,
               xaxis = list(title = "", tickangle = -45),
               yaxis = list(title = "Probability", range = c(0, 1)),
               hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', 
                                 font = list(color = 'darkgrey')), 
               hovermode = 'closest',
               autosize = TRUE)
      
      base <- add_trace_if_selected(base, "Democrat", filter(dat, party_identification3 == 1), "Democrat", az_color('pid3color1'), 'diamond')
      base <- add_trace_if_selected(base, "Independent", filter(dat, party_identification3 == 2), "Independent", az_color('pid3color2'), 'diamond')
      base <- add_trace_if_selected(base, "Republican", filter(dat, party_identification3 == 3), "Republican", az_color('pid3color3'), 'diamond')
      
      base <- add_trace_if_selected(base, "Non Hispanic White", filter(dat, white == 1), "White", az_color("racecolor3"), 'circle')
      base <- add_trace_if_selected(base, "Latino", filter(dat, latino == 1), "Latino", az_color("racecolor2"), 'circle')
      base <- add_trace_if_selected(base, "Other Race", filter(dat, white == 0), "Other Race", az_color("racecolor1"), 'circle')
      
      base <- add_trace_if_selected(base, "Liberal", filter(dat, conservative3 == 1), "Liberal", az_color("ideo3color1"), 'square')
      base <- add_trace_if_selected(base, "Moderate", filter(dat, conservative3 == 2), "Moderate", az_color("ideo3color2"), 'square')
      base <- add_trace_if_selected(base, "Conservative", filter(dat, conservative3 == 3), "Conservative", az_color("ideo3color3"), 'square')
      
      base <- add_trace_if_selected(base, "18-29 years", filter(dat, age_cohort == "18-29"), "18-29", az_color("agecolor1"), 'triangle-up')
      base <- add_trace_if_selected(base, "30-45 years", filter(dat, age_cohort == "30-44"), "30-44", az_color("agecolor2"), 'triangle-up')
      base <- add_trace_if_selected(base, "45-65 years", filter(dat, age_cohort == "45-64"), "45-64", az_color("agecolor3"), 'triangle-up')
      base <- add_trace_if_selected(base, "65+ years", filter(dat, age_cohort == "65+"), "65+", az_color("agecolor4"), 'triangle-up')
      
      base <- add_trace_if_selected(base, "Male", filter(dat, female == 0), "Male", az_color("gendercolor1"), 'triangle-down')
      base <- add_trace_if_selected(base, "Female", filter(dat, female == 1), "Female", az_color("gendercolor2"), 'triangle-down')
      
      base <- add_trace_if_selected(base, "Authoritarian", filter(dat, authoritarianism == 1), "Authoritarian", az_color("authcolor1"), 'star')
      base <- add_trace_if_selected(base, "Non-Authoritarian", filter(dat, authoritarianism == 0), "Non-Authoritarian", az_color("authcolor2"), 'star')
      
      
      base <- add_trace_if_selected(base, "Christian", filter(dat, christian == 1), "Christian", az_color("christiancolor1"), 'pentagon')
      base <- add_trace_if_selected(base, "Non Christian", filter(dat, christian == 0), "Non Christian", az_color("christiancolor2"), 'pentagon') 
      
      base <- add_trace_if_selected(base, "High Racial Resentment", filter(dat, rr == 1), "High Racial Resentment", az_color("rrcolor1"), 'hexagon')
      base <- add_trace_if_selected(base, "Low Racial Resentment", filter(dat, rr == 0), "Low Racial Resentment", az_color("rrcolor2"), 'hexagon') 
      
      base <- add_trace_if_selected(base, "College Degree", filter(dat, college == 1), "College Degree", az_color("collegecolor1"), 'hourglass')
      base <- add_trace_if_selected(base, "Less than College Degree", filter(dat, college == 0), "Less than College Degree", az_color("collegecolor2"), 'hourglass') 
      
      base <- add_trace_if_selected(base, "Kids in Home", filter(dat, kids_in_home == 1), "Kids in Home", az_color("kidscolor1"), 'cross')
      base <- add_trace_if_selected(base, "No Kids in Home", filter(dat, kids_in_home == 0), "No Kids in Home", az_color("kidscolor2"), 'cross') 
      
      base <- add_trace_if_selected(base, "More than $80k", filter(dat, faminc == 1), "More than $80k", az_color("hinccolor1"), 'x')
      base <- add_trace_if_selected(base, "Less than $80k", filter(dat, faminc == 0), "Less than $80k", az_color("hinccolor2"), 'x') 
      
      base <- add_trace_if_selected(base, "CD1", filter(dat, CD == 1), "CD1", az_color("cdcolor1"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD2", filter(dat, CD == 2), "CD2", az_color("cdcolor2"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD3", filter(dat, CD == 3), "CD3", az_color("cdcolor3"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD4", filter(dat, CD == 4), "CD4", az_color("cdcolor4"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD5", filter(dat, CD == 5), "CD5", az_color("cdcolor5"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD6", filter(dat, CD == 6), "CD6", az_color("cdcolor6"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD7", filter(dat, CD == 7), "CD7", az_color("cdcolor7"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD8", filter(dat, CD == 8), "CD8", az_color("cdcolor8"), 'star-diamond')
      base <- add_trace_if_selected(base, "CD9", filter(dat, CD == 9), "CD9", az_color("cdcolor9"), 'star-diamond')
      
      base %>% layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)',
                                        font = list(color = 'black', size = 16)))
    })
  }  

  output$hist_immigration <- create_plot_multi(
    outcome_name = "immigration_rate_long",
    plot_title = "",
    recode_scheme = "decrease"
  ) 
  output$hist_cali <- create_plot_multi(
    outcome_name = "cali_migration",
    plot_title = "",
    recode_scheme = "agree"
  ) 
  
  output$hist_separate <- create_plot_multi(
    outcome_name = "separate_parents",
    plot_title = "",
    recode_scheme = "agree"
  ) 
  
  output$hist_citizen <- create_plot_multi(
    outcome_name = "citizen",
    plot_title = "",
    recode_scheme = "agree4"
  ) 
  output$hist_smart <- create_plot_multi(
    outcome_name = "smart_border",
    plot_title = "",
    recode_scheme = "agree4"
  ) 
  
  output$hist_immigration <- create_plot_multi(
    outcome_name = "immigration_rate_long",
    plot_title = "",
    recode_scheme = "decrease"
  ) 
  output$hist_az_imm <- create_plot_multi(
    outcome_name = "immigration_to_az",
    plot_title = "",
    recode_scheme = "agree5r"
  ) 
  
  
  
  
  observe({
    print(input$multi)
    # if(input[["partyChoice"]][[1]]) {
    #   party_choice("Republican")
    # } else {
    #   party_choice(FALSE)
    # }
    # print(party_choice())
  })
  
  
  
}
                             
    