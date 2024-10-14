## Load globals and functions
source("functions/functions.R")
source("global.R")


server <- function(input, output) {
  party_choice_r = reactiveVal(NULL)
  party_choice_d = reactiveVal(NULL)
  party_choice_i = reactiveVal(NULL)
  

output$hist_burn <- renderPlotly({
    # create a histogram of the data
    # Create the base histogram
    if(input$surveyweight == "No Survey Weights"){
      df  =  build_data(item = "burn_flag")[[2]]
          }
    else{
      df  =  build_data(item = "burn_flag")[[3]]
        }
    
     model_df = build_data(item = "burn_flag")[[1]]

    dat = model_df %>% 
      mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                                `2` = "Oppose", 
                                `3` = "Neutral", 
                                `4` = "Support", 
                                `5` = "Strongly Support"),
             .value = as.numeric(.value)) %>%
      mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                      "Support", "Strongly Support")))
    
   base =  plot_ly(df, x = ~.category, 
            y = ~.value, type = 'bar', 
            name = "All Participants",
            text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                           "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
            marker = list(color = 'rgba(128, 128, 128, 0.9)'),
            textposition = "none",
            hoverinfo =  "text",
            opacity = 0.3) %>% 
      layout(title = "'Burn the American Flag'",
             showlegend = TRUE,
             xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
             yaxis = list(title = "Probability ", range = c(0, 1)),
             hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'darkgrey')), 
             hovermode = 'closest',
             autosize = TRUE)
    
    if("Democrat" %in% input$multi) {
      #Filter cases
      dat_democrat = dat %>% 
        filter(party_identification3 == 1)
      
 
      base <- base %>%
        add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                  showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
    }

    if("Republican" %in% input$multi) {
     #Filter cases
     dat_republican = dat %>% 
       filter(party_identification3 == 3)
     
     
     base <- base %>%
       add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Republican Participants",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Republican","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16
                                            ))) 
   }
   
    if("Independent" %in% input$multi) {
     #Filter cases
     dat_independent = dat %>% 
       filter(party_identification3 == 2)
     
     
     base <- base %>%
       add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Independent Participants",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "purple", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Independent","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    if("Non Hispanic White" %in% input$multi) {
     #Filter cases
     dat_white= dat %>% 
       filter(white == 1)
     
     base <- base %>%
       add_trace(data = dat_white, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Non Hispanic White",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    if("Latino" %in% input$multi) {
     #Filter cases
     dat_latino= dat %>% 
       filter(latino == 1)
     
     base <- base %>%
       add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Non Hispanic White",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Latino","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
 
    if("Conservative" %in% input$multi) {
     #Filter cases
     dat_conservative= dat %>% 
       filter(conservative3 == 3)
     
     base <- base %>%
       add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Conservative",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Conservative","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    if("Liberal" %in% input$multi) {
     #Filter cases
     dat_lib= dat %>% 
       filter(conservative3 == 1)
     
     base <- base %>%
       add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Liberal",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Liberal","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    if("Moderate" %in% input$multi) {
     #Filter cases
     dat_mod= dat %>% 
       filter(conservative3 == 2)
     
     base <- base %>%
       add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Moderate",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Moderate","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    if("18-29 years" %in% input$multi) {
     #Filter cases
     dat1= dat %>% 
       filter(age_cohort == "18-29")
     
     base <- base %>%
       add_trace(data = dat1, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "18-29 years",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "18-29 years","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("30-45 years" %in% input$multi) {
     #Filter cases
     dat2= dat %>% 
       filter(age_cohort == "30-45")
     
     base <- base %>%
       add_trace(data = dat2, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "30-45 years",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "18-29 years","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   
   if("45-65 years" %in% input$multi) {
     #Filter cases
     dat3= dat %>% 
       filter(age_cohort == "45-65")
     
     base <- base %>%
       add_trace(data = dat3, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "45-65 years",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "orange", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "45-65 years","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("65+ years" %in% input$multi) {
     #Filter cases
     dat4= dat %>% 
       filter(age_cohort == "65+")
     
     base <- base %>%
       add_trace(data = dat4, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "65+ years",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "orange", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "65+ years","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
 
   if("Male" %in% input$multi) {
     #Filter cases
     dat5= dat %>% 
       filter(female == "0")
     
     base <- base %>%
       add_trace(data = dat5, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Male",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "pink", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Male","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Female" %in% input$multi) {
     #Filter cases
     dat6= dat %>% 
       filter(female == "1")
     
     base <- base %>%
       add_trace(data = dat6, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Female",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Female","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Other Race" %in% input$multi) {
     #Filter cases
     dat7= dat %>% 
       filter(white == "0")
     
     base <- base %>%
       add_trace(data = dat7, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Other Race or Ethnic Group",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Authoritarian" %in% input$multi) {
     #Filter cases
     dat_auth= dat %>% 
       filter(authoritarianism == "1")
     
     base <- base %>%
       add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Authoritarian Voter",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Non-Authoritarian" %in% input$multi) {
     #Filter cases
     dat_lauth= dat %>% 
       filter(authoritarianism == "0")
     
     base <- base %>%
       add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Non-Authoritarian Voter",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Christian" %in% input$multi) {
     #Filter cases
     dat_christian= dat %>% 
       filter(christian == 1)
     
     base <- base %>%
       add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Christian Voter",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Christian","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Non Christian" %in% input$multi) {
     #Filter cases
     dat_nchristian= dat %>% 
       filter(christian == 0)
     
     base <- base %>%
       add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Non Christian Voter",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Non Christian","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("High Racial Resentment" %in% input$multi) {
     #Filter cases
     dat_rr= dat %>% 
       filter(rr == 1)
     
     base <- base %>%
       add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "High Racial Resentment",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Low Racial Resentment" %in% input$multi) {
     #Filter cases
     dat_lrr= dat %>% 
       filter(rr == 0)
     
     base <- base %>%
       add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Low Racial Resentment",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#433878", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("College Degree" %in% input$multi) {
     #Filter cases
     dat_college= dat %>% 
       filter(college == 1)
     
     base <- base %>%
       add_trace(data = dat_college, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "College Degree",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }

   if("Less than College Degree" %in% input$multi) {
     #Filter cases
     dat_ncollege= dat %>% 
       filter(college == 0)
     
     base <- base %>%
       add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Less than College Degree",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Kids in Home" %in% input$multi) {
     #Filter cases
     dat_kids= dat %>% 
       filter(kids_in_home == 1)
     
     base <- base %>%
       add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Kids in Home",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#347928", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Kids in home","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("No Kids in Home" %in% input$multi) {
     #Filter cases
     dat_nkids= dat %>% 
       filter(kids_in_home == 0)
     
     base <- base %>%
       add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "No Kids in Home",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("More than $80k" %in% input$multi) {
     #Filter cases
     dat_inc= dat %>% 
       filter(faminc == 1)
     
     base <- base %>%
       add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Family Income $80k or greater",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#795757", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "More than $80k","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("Less than $80k" %in% input$multi) {
     #Filter cases
     dat_ninc= dat %>% 
       filter(faminc == 0)
     
     base <- base %>%
       add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Family Income $80k or less",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#507687", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD1" %in% input$multi) {
     #Filter cases
     datc1= dat %>% 
       filter(CD == 1)
     
     base <- base %>%
       add_trace(data = datc1, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 1",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD1","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD2" %in% input$multi) {
     #Filter cases
     datc2= dat %>% 
       filter(CD == 2)
     
     base <- base %>%
       add_trace(data = datc2, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 2",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD2","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD3" %in% input$multi) {
     #Filter cases
     datc3= dat %>% 
       filter(CD == 3)
     
     base <- base %>%
       add_trace(data = datc3, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 3",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD3","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }

   if("CD4" %in% input$multi) {
     #Filter cases
     datc4= dat %>% 
       filter(CD == 4)
     
     base <- base %>%
       add_trace(data = datc4, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 4",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD4","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }

   if("CD5" %in% input$multi) {
     #Filter cases
     datc5= dat %>% 
       filter(CD == 5)
     
     base <- base %>%
       add_trace(data = datc5, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 5",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD5","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD6" %in% input$multi) {
     #Filter cases
     datc6= dat %>% 
       filter(CD == 6)
     
     base <- base %>%
       add_trace(data = datc6, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 6",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD6","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD7" %in% input$multi) {
     #Filter cases
     datc7= dat %>% 
       filter(CD == 7)
     
     base <- base %>%
       add_trace(data = datc7, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 7",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD7","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD8" %in% input$multi) {
     #Filter cases
     datc8= dat %>% 
       filter(CD == 8)
     
     base <- base %>%
       add_trace(data = datc8, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 8",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#257180", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD8","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
   if("CD9" %in% input$multi) {
     #Filter cases
     datc9= dat %>% 
       filter(CD == 9)
     
     base <- base %>%
       add_trace(data = datc9, x = ~.category, y = ~.value, 
                 showlegend = TRUE,
                 name = "Congressional District 9",
                 opacity = 0.5,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                 error_y = ~list(array = .upper - .value, 
                                 arrayminus = .value - .lower, 
                                 color = 'grey'),
                 text = ~paste0("<b>", "CD8","</b>","<br>",
                                "<b>", "Category: ", "</b>", .category, "<br>",
                                "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                                "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                                round(.upper, 2),  "]<br>"),
                 hoverinfo = 'text') %>%
       layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                                font = list(color = 'lightgrey', 
                                            size = 16))) 
   }
   
    base <- base %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                               font = list(color = 'black', size = 16))) # Black text color and larger font size
    
})
output$hist_court <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data(item = "court")[[2]]
  }
  else{
    df  =  build_data(item = "court")[[3]]
  }
  
  model_df = build_data(item = "court")[[1]]
  
  dat = model_df %>% 
    mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                    "Support", "Strongly Support")))
  
  base =  plot_ly(df, x = ~.category, 
                  y = ~.value, type = 'bar', 
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>% 
    layout(title = "'Contest the Outcome in Court'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')), 
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi) {
    #Filter cases
    dat_democrat = dat %>% 
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Republican" %in% input$multi) {
    #Filter cases
    dat_republican = dat %>% 
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16
                               ))) 
  }
  
  if("Independent" %in% input$multi) {
    #Filter cases
    dat_independent = dat %>% 
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Hispanic White" %in% input$multi) {
    #Filter cases
    dat_white= dat %>% 
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Latino" %in% input$multi) {
    #Filter cases
    dat_latino= dat %>% 
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Conservative" %in% input$multi) {
    #Filter cases
    dat_conservative= dat %>% 
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Liberal" %in% input$multi) {
    #Filter cases
    dat_lib= dat %>% 
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Moderate" %in% input$multi) {
    #Filter cases
    dat_mod= dat %>% 
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("18-29 years" %in% input$multi) {
    #Filter cases
    dat1= dat %>% 
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("30-45 years" %in% input$multi) {
    #Filter cases
    dat2= dat %>% 
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("45-65 years" %in% input$multi) {
    #Filter cases
    dat3= dat %>% 
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("65+ years" %in% input$multi) {
    #Filter cases
    dat4= dat %>% 
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("Male" %in% input$multi) {
    #Filter cases
    dat5= dat %>% 
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Female" %in% input$multi) {
    #Filter cases
    dat6= dat %>% 
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Other Race" %in% input$multi) {
    #Filter cases
    dat7= dat %>% 
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Authoritarian" %in% input$multi) {
    #Filter cases
    dat_auth= dat %>% 
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non-Authoritarian" %in% input$multi) {
    #Filter cases
    dat_lauth= dat %>% 
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Christian" %in% input$multi) {
    #Filter cases
    dat_christian= dat %>% 
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Christian" %in% input$multi) {
    #Filter cases
    dat_nchristian= dat %>% 
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("High Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_rr= dat %>% 
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Low Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_lrr= dat %>% 
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("College Degree" %in% input$multi) {
    #Filter cases
    dat_college= dat %>% 
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than College Degree" %in% input$multi) {
    #Filter cases
    dat_ncollege= dat %>% 
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Kids in Home" %in% input$multi) {
    #Filter cases
    dat_kids= dat %>% 
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("No Kids in Home" %in% input$multi) {
    #Filter cases
    dat_nkids= dat %>% 
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("More than $80k" %in% input$multi) {
    #Filter cases
    dat_inc= dat %>% 
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than $80k" %in% input$multi) {
    #Filter cases
    dat_ninc= dat %>% 
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD1" %in% input$multi) {
    #Filter cases
    datc1= dat %>% 
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD2" %in% input$multi) {
    #Filter cases
    datc2= dat %>% 
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD3" %in% input$multi) {
    #Filter cases
    datc3= dat %>% 
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD4" %in% input$multi) {
    #Filter cases
    datc4= dat %>% 
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD5" %in% input$multi) {
    #Filter cases
    datc5= dat %>% 
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD6" %in% input$multi) {
    #Filter cases
    datc6= dat %>% 
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD7" %in% input$multi) {
    #Filter cases
    datc7= dat %>% 
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD8" %in% input$multi) {
    #Filter cases
    datc8= dat %>% 
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD9" %in% input$multi) {
    #Filter cases
    datc9= dat %>% 
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})
output$hist_recount <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data(item = "recount")[[2]]
  }
  else{
    df  =  build_data(item = "recount")[[3]]
  }
  
  model_df = build_data(item = "recount")[[1]]
  
  dat = model_df %>% 
    mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                    "Support", "Strongly Support")))
  
  base =  plot_ly(df, x = ~.category, 
                  y = ~.value, type = 'bar', 
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>% 
    layout(title = "'Support Ballot Recounts'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')), 
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi) {
    #Filter cases
    dat_democrat = dat %>% 
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Republican" %in% input$multi) {
    #Filter cases
    dat_republican = dat %>% 
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16
                               ))) 
  }
  
  if("Independent" %in% input$multi) {
    #Filter cases
    dat_independent = dat %>% 
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Hispanic White" %in% input$multi) {
    #Filter cases
    dat_white= dat %>% 
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Latino" %in% input$multi) {
    #Filter cases
    dat_latino= dat %>% 
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Conservative" %in% input$multi) {
    #Filter cases
    dat_conservative= dat %>% 
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Liberal" %in% input$multi) {
    #Filter cases
    dat_lib= dat %>% 
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Moderate" %in% input$multi) {
    #Filter cases
    dat_mod= dat %>% 
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("18-29 years" %in% input$multi) {
    #Filter cases
    dat1= dat %>% 
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("30-45 years" %in% input$multi) {
    #Filter cases
    dat2= dat %>% 
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("45-65 years" %in% input$multi) {
    #Filter cases
    dat3= dat %>% 
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("65+ years" %in% input$multi) {
    #Filter cases
    dat4= dat %>% 
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("Male" %in% input$multi) {
    #Filter cases
    dat5= dat %>% 
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Female" %in% input$multi) {
    #Filter cases
    dat6= dat %>% 
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Other Race" %in% input$multi) {
    #Filter cases
    dat7= dat %>% 
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Authoritarian" %in% input$multi) {
    #Filter cases
    dat_auth= dat %>% 
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non-Authoritarian" %in% input$multi) {
    #Filter cases
    dat_lauth= dat %>% 
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Christian" %in% input$multi) {
    #Filter cases
    dat_christian= dat %>% 
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Christian" %in% input$multi) {
    #Filter cases
    dat_nchristian= dat %>% 
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("High Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_rr= dat %>% 
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Low Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_lrr= dat %>% 
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("College Degree" %in% input$multi) {
    #Filter cases
    dat_college= dat %>% 
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than College Degree" %in% input$multi) {
    #Filter cases
    dat_ncollege= dat %>% 
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Kids in Home" %in% input$multi) {
    #Filter cases
    dat_kids= dat %>% 
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("No Kids in Home" %in% input$multi) {
    #Filter cases
    dat_nkids= dat %>% 
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("More than $80k" %in% input$multi) {
    #Filter cases
    dat_inc= dat %>% 
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than $80k" %in% input$multi) {
    #Filter cases
    dat_ninc= dat %>% 
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD1" %in% input$multi) {
    #Filter cases
    datc1= dat %>% 
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD2" %in% input$multi) {
    #Filter cases
    datc2= dat %>% 
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD3" %in% input$multi) {
    #Filter cases
    datc3= dat %>% 
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD4" %in% input$multi) {
    #Filter cases
    datc4= dat %>% 
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD5" %in% input$multi) {
    #Filter cases
    datc5= dat %>% 
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD6" %in% input$multi) {
    #Filter cases
    datc6= dat %>% 
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD7" %in% input$multi) {
    #Filter cases
    datc7= dat %>% 
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD8" %in% input$multi) {
    #Filter cases
    datc8= dat %>% 
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD9" %in% input$multi) {
    #Filter cases
    datc9= dat %>% 
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})
output$hist_criticize <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data(item = "criticize_election")[[2]]
  }
  else{
    df  =  build_data(item = "criticize_election")[[3]]
  }
  
  model_df = build_data(item = "criticize_election")[[1]]
  
  dat = model_df %>% 
    mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                    "Support", "Strongly Support")))
  
  base =  plot_ly(df, x = ~.category, 
                  y = ~.value, type = 'bar', 
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>% 
    layout(title = "'Publicly criticize the integrity or fairness of the election'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')), 
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi) {
    #Filter cases
    dat_democrat = dat %>% 
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Republican" %in% input$multi) {
    #Filter cases
    dat_republican = dat %>% 
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16
                               ))) 
  }
  
  if("Independent" %in% input$multi) {
    #Filter cases
    dat_independent = dat %>% 
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Hispanic White" %in% input$multi) {
    #Filter cases
    dat_white= dat %>% 
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Latino" %in% input$multi) {
    #Filter cases
    dat_latino= dat %>% 
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Conservative" %in% input$multi) {
    #Filter cases
    dat_conservative= dat %>% 
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Liberal" %in% input$multi) {
    #Filter cases
    dat_lib= dat %>% 
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Moderate" %in% input$multi) {
    #Filter cases
    dat_mod= dat %>% 
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("18-29 years" %in% input$multi) {
    #Filter cases
    dat1= dat %>% 
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("30-45 years" %in% input$multi) {
    #Filter cases
    dat2= dat %>% 
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("45-65 years" %in% input$multi) {
    #Filter cases
    dat3= dat %>% 
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("65+ years" %in% input$multi) {
    #Filter cases
    dat4= dat %>% 
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("Male" %in% input$multi) {
    #Filter cases
    dat5= dat %>% 
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Female" %in% input$multi) {
    #Filter cases
    dat6= dat %>% 
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Other Race" %in% input$multi) {
    #Filter cases
    dat7= dat %>% 
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Authoritarian" %in% input$multi) {
    #Filter cases
    dat_auth= dat %>% 
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non-Authoritarian" %in% input$multi) {
    #Filter cases
    dat_lauth= dat %>% 
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Christian" %in% input$multi) {
    #Filter cases
    dat_christian= dat %>% 
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Christian" %in% input$multi) {
    #Filter cases
    dat_nchristian= dat %>% 
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("High Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_rr= dat %>% 
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Low Racial Resentment" %in% input$multi) {
    #Filter cases
    dat_lrr= dat %>% 
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("College Degree" %in% input$multi) {
    #Filter cases
    dat_college= dat %>% 
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than College Degree" %in% input$multi) {
    #Filter cases
    dat_ncollege= dat %>% 
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Kids in Home" %in% input$multi) {
    #Filter cases
    dat_kids= dat %>% 
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("No Kids in Home" %in% input$multi) {
    #Filter cases
    dat_nkids= dat %>% 
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("More than $80k" %in% input$multi) {
    #Filter cases
    dat_inc= dat %>% 
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than $80k" %in% input$multi) {
    #Filter cases
    dat_ninc= dat %>% 
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD1" %in% input$multi) {
    #Filter cases
    datc1= dat %>% 
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD2" %in% input$multi) {
    #Filter cases
    datc2= dat %>% 
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD3" %in% input$multi) {
    #Filter cases
    datc3= dat %>% 
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD4" %in% input$multi) {
    #Filter cases
    datc4= dat %>% 
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD5" %in% input$multi) {
    #Filter cases
    datc5= dat %>% 
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD6" %in% input$multi) {
    #Filter cases
    datc6= dat %>% 
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD7" %in% input$multi) {
    #Filter cases
    datc7= dat %>% 
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD8" %in% input$multi) {
    #Filter cases
    datc8= dat %>% 
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD9" %in% input$multi) {
    #Filter cases
    datc9= dat %>% 
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})
output$hist_certify <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data(item = "state_certify")[[2]]
  }
  else{
    df  =  build_data(item = "state_certify")[[3]]
  }
  
  model_df = build_data(item = "state_certify")[[1]]
  
  dat = model_df %>% 
    mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                    "Support", "Strongly Support")))
  
  base =  plot_ly(df, x = ~.category, 
                  y = ~.value, type = 'bar', 
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>% 
    layout(title = "'State legislators refusing to certify the election results'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')), 
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi2) {
    #Filter cases
    dat_democrat = dat %>% 
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Republican" %in% input$multi2) {
    #Filter cases
    dat_republican = dat %>% 
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16
                               ))) 
  }
  
  if("Independent" %in% input$multi2) {
    #Filter cases
    dat_independent = dat %>% 
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Hispanic White" %in% input$multi2) {
    #Filter cases
    dat_white= dat %>% 
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Latino" %in% input$multi2) {
    #Filter cases
    dat_latino= dat %>% 
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Conservative" %in% input$multi2) {
    #Filter cases
    dat_conservative= dat %>% 
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Liberal" %in% input$multi2) {
    #Filter cases
    dat_lib= dat %>% 
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Moderate" %in% input$multi2) {
    #Filter cases
    dat_mod= dat %>% 
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("18-29 years" %in% input$multi2) {
    #Filter cases
    dat1= dat %>% 
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("30-45 years" %in% input$multi2) {
    #Filter cases
    dat2= dat %>% 
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("45-65 years" %in% input$multi2) {
    #Filter cases
    dat3= dat %>% 
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("65+ years" %in% input$multi2) {
    #Filter cases
    dat4= dat %>% 
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("Male" %in% input$multi2) {
    #Filter cases
    dat5= dat %>% 
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Female" %in% input$multi2) {
    #Filter cases
    dat6= dat %>% 
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Other Race" %in% input$multi2) {
    #Filter cases
    dat7= dat %>% 
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Authoritarian" %in% input$multi2) {
    #Filter cases
    dat_auth= dat %>% 
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non-Authoritarian" %in% input$multi2) {
    #Filter cases
    dat_lauth= dat %>% 
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Christian" %in% input$multi2) {
    #Filter cases
    dat_christian= dat %>% 
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Christian" %in% input$multi2) {
    #Filter cases
    dat_nchristian= dat %>% 
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("High Racial Resentment" %in% input$multi2) {
    #Filter cases
    dat_rr= dat %>% 
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Low Racial Resentment" %in% input$multi2) {
    #Filter cases
    dat_lrr= dat %>% 
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("College Degree" %in% input$multi2) {
    #Filter cases
    dat_college= dat %>% 
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than College Degree" %in% input$multi2) {
    #Filter cases
    dat_ncollege= dat %>% 
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Kids in Home" %in% input$multi2) {
    #Filter cases
    dat_kids= dat %>% 
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("No Kids in Home" %in% input$multi2) {
    #Filter cases
    dat_nkids= dat %>% 
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("More than $80k" %in% input$multi2) {
    #Filter cases
    dat_inc= dat %>% 
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than $80k" %in% input$multi2) {
    #Filter cases
    dat_ninc= dat %>% 
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD1" %in% input$multi2) {
    #Filter cases
    datc1= dat %>% 
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD2" %in% input$multi2) {
    #Filter cases
    datc2= dat %>% 
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD3" %in% input$multi2) {
    #Filter cases
    datc3= dat %>% 
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD4" %in% input$multi2) {
    #Filter cases
    datc4= dat %>% 
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD5" %in% input$multi2) {
    #Filter cases
    datc5= dat %>% 
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD6" %in% input$multi2) {
    #Filter cases
    datc6= dat %>% 
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD7" %in% input$multi2) {
    #Filter cases
    datc7= dat %>% 
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD8" %in% input$multi2) {
    #Filter cases
    datc8= dat %>% 
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD9" %in% input$multi2) {
    #Filter cases
    datc9= dat %>% 
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})
output$hist_newelection <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data(item = "new_election")[[2]]
  }
  else{
    df  =  build_data(item = "new_election")[[3]]
  }
  
  model_df = build_data(item = "new_election")[[1]]
  
  dat = model_df %>% 
    mutate(.category = recode(.category, `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Oppose", "Oppose", "Neutral",
                                                    "Support", "Strongly Support")))
  
  base =  plot_ly(df, x = ~.category, 
                  y = ~.value, type = 'bar', 
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>", 
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>% 
    layout(title = "'Call a new election'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')), 
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi2) {
    #Filter cases
    dat_democrat = dat %>% 
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Republican" %in% input$multi2) {
    #Filter cases
    dat_republican = dat %>% 
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16
                               ))) 
  }
  
  if("Independent" %in% input$multi2) {
    #Filter cases
    dat_independent = dat %>% 
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Hispanic White" %in% input$multi2) {
    #Filter cases
    dat_white= dat %>% 
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Latino" %in% input$multi2) {
    #Filter cases
    dat_latino= dat %>% 
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Conservative" %in% input$multi2) {
    #Filter cases
    dat_conservative= dat %>% 
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Liberal" %in% input$multi2) {
    #Filter cases
    dat_lib= dat %>% 
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Moderate" %in% input$multi2) {
    #Filter cases
    dat_mod= dat %>% 
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("18-29 years" %in% input$multi2) {
    #Filter cases
    dat1= dat %>% 
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("30-45 years" %in% input$multi2) {
    #Filter cases
    dat2= dat %>% 
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("45-65 years" %in% input$multi2) {
    #Filter cases
    dat3= dat %>% 
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("65+ years" %in% input$multi2) {
    #Filter cases
    dat4= dat %>% 
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  
  if("Male" %in% input$multi2) {
    #Filter cases
    dat5= dat %>% 
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Female" %in% input$multi2) {
    #Filter cases
    dat6= dat %>% 
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Other Race" %in% input$multi2) {
    #Filter cases
    dat7= dat %>% 
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Authoritarian" %in% input$multi2) {
    #Filter cases
    dat_auth= dat %>% 
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non-Authoritarian" %in% input$multi2) {
    #Filter cases
    dat_lauth= dat %>% 
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Christian" %in% input$multi2) {
    #Filter cases
    dat_christian= dat %>% 
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Non Christian" %in% input$multi2) {
    #Filter cases
    dat_nchristian= dat %>% 
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("High Racial Resentment" %in% input$multi2) {
    #Filter cases
    dat_rr= dat %>% 
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Low Racial Resentment" %in% input$multi2) {
    #Filter cases
    dat_lrr= dat %>% 
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("College Degree" %in% input$multi2) {
    #Filter cases
    dat_college= dat %>% 
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than College Degree" %in% input$multi2) {
    #Filter cases
    dat_ncollege= dat %>% 
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Kids in Home" %in% input$multi2) {
    #Filter cases
    dat_kids= dat %>% 
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("No Kids in Home" %in% input$multi2) {
    #Filter cases
    dat_nkids= dat %>% 
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("More than $80k" %in% input$multi2) {
    #Filter cases
    dat_inc= dat %>% 
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("Less than $80k" %in% input$multi2) {
    #Filter cases
    dat_ninc= dat %>% 
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD1" %in% input$multi2) {
    #Filter cases
    datc1= dat %>% 
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD2" %in% input$multi2) {
    #Filter cases
    datc2= dat %>% 
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD3" %in% input$multi2) {
    #Filter cases
    datc3= dat %>% 
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD4" %in% input$multi2) {
    #Filter cases
    datc4= dat %>% 
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD5" %in% input$multi2) {
    #Filter cases
    datc5= dat %>% 
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD6" %in% input$multi2) {
    #Filter cases
    datc6= dat %>% 
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD7" %in% input$multi2) {
    #Filter cases
    datc7= dat %>% 
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD8" %in% input$multi2) {
    #Filter cases
    datc8= dat %>% 
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  if("CD9" %in% input$multi2) {
    #Filter cases
    datc9= dat %>% 
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value, 
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value, 
                                arrayminus = .value - .lower, 
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",", 
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey', 
                                           size = 16))) 
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})
output$stolen_2020 <- renderPlotly({
  # create a histogram of the data
  # Create the base histogram
  if(input$surveyweight == "No Survey Weights"){
    df  =  build_data_agree(item = "stolen_2020")[[2]]
  }
  else{
    df  =  build_data_agree(item = "stolen_2020")[[3]]
  }
  
  model_df = build_data_agree(item = "stolen_2020")[[1]]
  
  dat = model_df %>%
    mutate(.category = recode(.category, `1` = "Strongly Disagree",
                              `2` = "Somewhat Disagree",
                              `3` = "Neutral",
                              `4` = "Somewhat Agree",
                              `5` = "Strongly Agree"),
           .value = as.numeric(.value)) %>%
    mutate(.category = factor(.category, levels = c("Strongly Disagree", "Somewhat Disagree", "Neutral",
                                                    "Somewhat Agree", "Strongly Agree")))
  
  base =  plot_ly(df, x = ~.category,
                  y = ~.value, type = 'bar',
                  name = "All Participants",
                  text = ~paste0("<b>", "Category: ", "</b>", .category, "<br>",
                                 "<b>","Probability: ","</b>", round(.value, 2)), # Tooltip text
                  marker = list(color = 'rgba(128, 128, 128, 0.9)'),
                  textposition = "none",
                  hoverinfo =  "text",
                  opacity = 0.3) %>%
    layout(title = "'The 2020 Presidential election was\nstolen by the Democratic Party'",
           showlegend = TRUE,
           xaxis = list(title = "Category ", tickangle = -45), # Rotate x-axis labels by 45 degrees
           yaxis = list(title = "Probability ", range = c(0, 1)),
           hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                             font = list(color = 'darkgrey')),
           hovermode = 'closest',
           autosize = TRUE)
  
  if("Democrat" %in% input$multi3) {
    #Filter cases
    dat_democrat = dat %>%
      filter(party_identification3 == 1)
    
    
    base <- base %>%
      add_trace(data = dat_democrat, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Democratic Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azblue'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Democrat","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Republican" %in% input$multi3) {
    #Filter cases
    dat_republican = dat %>%
      filter(party_identification3 == 3)
    
    
    base <- base %>%
      add_trace(data = dat_republican, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Republican Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color('azred'), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Republican","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16
                               )))
  }
  
  if("Independent" %in% input$multi3) {
    #Filter cases
    dat_independent = dat %>%
      filter(party_identification3 == 2)
    
    
    base <- base %>%
      add_trace(data = dat_independent, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Independent Participants",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "purple", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Independent","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Non Hispanic White" %in% input$multi3) {
    #Filter cases
    dat_white= dat %>%
      filter(white == 1)
    
    base <- base %>%
      add_trace(data = dat_white, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("azurite"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Non Hispanic White","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Latino" %in% input$multi3) {
    #Filter cases
    dat_latino= dat %>%
      filter(latino == 1)
    
    base <- base %>%
      add_trace(data = dat_latino, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Non Hispanic White",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Latino","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Conservative" %in% input$multi3) {
    #Filter cases
    dat_conservative= dat %>%
      filter(conservative3 == 3)
    
    base <- base %>%
      add_trace(data = dat_conservative, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Conservative",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("chili"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Conservative","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Liberal" %in% input$multi3) {
    #Filter cases
    dat_lib= dat %>%
      filter(conservative3 == 1)
    
    base <- base %>%
      add_trace(data = dat_lib, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Liberal",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Liberal","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Moderate" %in% input$multi3) {
    #Filter cases
    dat_mod= dat %>%
      filter(conservative3 == 2)
    
    base <- base %>%
      add_trace(data = dat_mod, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Moderate",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("midnight"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Moderate","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("18-29 years" %in% input$multi3) {
    #Filter cases
    dat1= dat %>%
      filter(age_cohort == "18-29")
    
    base <- base %>%
      add_trace(data = dat1, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "18-29 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("30-45 years" %in% input$multi3) {
    #Filter cases
    dat2= dat %>%
      filter(age_cohort == "30-45")
    
    base <- base %>%
      add_trace(data = dat2, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "30-45 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "darkblue", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "18-29 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("45-65 years" %in% input$multi3) {
    #Filter cases
    dat3= dat %>%
      filter(age_cohort == "45-65")
    
    base <- base %>%
      add_trace(data = dat3, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "45-65 years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "45-65 years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("65+ years" %in% input$multi3) {
    #Filter cases
    dat4= dat %>%
      filter(age_cohort == "65+")
    
    base <- base %>%
      add_trace(data = dat4, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "65+ years",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "orange", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "65+ years","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Male" %in% input$multi3) {
    #Filter cases
    dat5= dat %>%
      filter(female == "0")
    
    base <- base %>%
      add_trace(data = dat5, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Male",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "pink", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Male","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Female" %in% input$multi3) {
    #Filter cases
    dat6= dat %>%
      filter(female == "1")
    
    base <- base %>%
      add_trace(data = dat6, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Female",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "lightgreen", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Female","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Other Race" %in% input$multi3) {
    #Filter cases
    dat7= dat %>%
      filter(white == "0")
    
    base <- base %>%
      add_trace(data = dat7, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Other Race or Ethnic Group",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = az_color("oasis"), symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Other Race or Ethnic Group","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Authoritarian" %in% input$multi3) {
    #Filter cases
    dat_auth= dat %>%
      filter(authoritarianism == "1")
    
    base <- base %>%
      add_trace(data = dat_auth, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#72BF78", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Non-Authoritarian" %in% input$multi3) {
    #Filter cases
    dat_lauth= dat %>%
      filter(authoritarianism == "0")
    
    base <- base %>%
      add_trace(data = dat_lauth, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Non-Authoritarian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FEFF9F", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Non-Authoritarian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Christian" %in% input$multi3) {
    #Filter cases
    dat_christian= dat %>%
      filter(christian == 1)
    
    base <- base %>%
      add_trace(data = dat_christian, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF6500", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Non Christian" %in% input$multi3) {
    #Filter cases
    dat_nchristian= dat %>%
      filter(christian == 0)
    
    base <- base %>%
      add_trace(data = dat_nchristian, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Non Christian Voter",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#0B192C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Non Christian","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("High Racial Resentment" %in% input$multi3) {
    #Filter cases
    dat_rr= dat %>%
      filter(rr == 1)
    
    base <- base %>%
      add_trace(data = dat_rr, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "High Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4B1F0", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "High Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Low Racial Resentment" %in% input$multi3) {
    #Filter cases
    dat_lrr= dat %>%
      filter(rr == 0)
    
    base <- base %>%
      add_trace(data = dat_lrr, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Low Racial Resentment",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#433878", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Low Racial Resentment","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("College Degree" %in% input$multi3) {
    #Filter cases
    dat_college= dat %>%
      filter(college == 1)
    
    base <- base %>%
      add_trace(data = dat_college, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#B7E0FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "College Degree or Greater","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Less than College Degree" %in% input$multi3) {
    #Filter cases
    dat_ncollege= dat %>%
      filter(college == 0)
    
    base <- base %>%
      add_trace(data = dat_ncollege, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Less than College Degree",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FFD7C4", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Less than College Degree","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Kids in Home" %in% input$multi3) {
    #Filter cases
    dat_kids= dat %>%
      filter(kids_in_home == 1)
    
    base <- base %>%
      add_trace(data = dat_kids, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#347928", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("No Kids in Home" %in% input$multi3) {
    #Filter cases
    dat_nkids= dat %>%
      filter(kids_in_home == 0)
    
    base <- base %>%
      add_trace(data = dat_nkids, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "No Kids in Home",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#EE66A6", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "No Kids in home","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("More than $80k" %in% input$multi3) {
    #Filter cases
    dat_inc= dat %>%
      filter(faminc == 1)
    
    base <- base %>%
      add_trace(data = dat_inc, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Family Income $80k or greater",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#795757", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "More than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("Less than $80k" %in% input$multi3) {
    #Filter cases
    dat_ninc= dat %>%
      filter(faminc == 0)
    
    base <- base %>%
      add_trace(data = dat_ninc, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Family Income $80k or less",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#507687", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "Less than $80k","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD1" %in% input$multi3) {
    #Filter cases
    datc1= dat %>%
      filter(CD == 1)
    
    base <- base %>%
      add_trace(data = datc1, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 1",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#7695FF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD1","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD2" %in% input$multi3) {
    #Filter cases
    datc2= dat %>%
      filter(CD == 2)
    
    base <- base %>%
      add_trace(data = datc2, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 2",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FF9874", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD2","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD3" %in% input$multi3) {
    #Filter cases
    datc3= dat %>%
      filter(CD == 3)
    
    base <- base %>%
      add_trace(data = datc3, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 3",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#A2D2DF", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD3","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD4" %in% input$multi3) {
    #Filter cases
    datc4= dat %>%
      filter(CD == 4)
    
    base <- base %>%
      add_trace(data = datc4, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 4",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD4","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD5" %in% input$multi3) {
    #Filter cases
    datc5= dat %>%
      filter(CD == 5)
    
    base <- base %>%
      add_trace(data = datc5, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 5",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#BC7C7C", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD5","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD6" %in% input$multi3) {
    #Filter cases
    datc6= dat %>%
      filter(CD == 6)
    
    base <- base %>%
      add_trace(data = datc6, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 6",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#F6EFBD", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD6","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD7" %in% input$multi3) {
    #Filter cases
    datc7= dat %>%
      filter(CD == 7)
    
    base <- base %>%
      add_trace(data = datc7, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 7",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#E4C087", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD7","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD8" %in% input$multi3) {
    #Filter cases
    datc8= dat %>%
      filter(CD == 8)
    
    base <- base %>%
      add_trace(data = datc8, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 8",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#257180", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD8","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  if("CD9" %in% input$multi3) {
    #Filter cases
    datc9= dat %>%
      filter(CD == 9)
    
    base <- base %>%
      add_trace(data = datc9, x = ~.category, y = ~.value,
                showlegend = TRUE,
                name = "Congressional District 9",
                opacity = 0.5,
                type = 'scatter',
                mode = 'markers',
                marker = list(size = 15, color = "#FD8B51", symbol = 'circle'),
                error_y = ~list(array = .upper - .value,
                                arrayminus = .value - .lower,
                                color = 'grey'),
                text = ~paste0("<b>", "CD9","</b>","<br>",
                               "<b>", "Category: ", "</b>", .category, "<br>",
                               "<b>", "Probability: ","</b>", round(.value, 2), "<br>",
                               "<b>",  "Margin of Error: ", "</b>", "[", round(.lower, 2), ",",
                               round(.upper, 2),  "]<br>"),
                hoverinfo = 'text') %>%
      layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.4)', # White and semi-transparent
                               font = list(color = 'lightgrey',
                                           size = 16)))
  }
  
  base <- base %>%
    layout(hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.7)', # White and semi-transparent
                             font = list(color = 'black', size = 16))) # Black text color and larger font size
  
})





  
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



    
