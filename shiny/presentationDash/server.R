source("functions/functions.R")
source("global.R")


server <- function(input, output) {
  
build_data_outcome <- reactive({
  req(input$group)  
   data %>%
      filter(independent_variable == input$group) %>%
      arrange(input$group, party, dv) 
  })
  
output$feelings <- renderPlotly({
  reactData <- build_data_outcome()
  
  plot <- ggplot(reactData, aes(x = dv,
                                y = .value,
                                ymax = .upper,
                                ymin = .lower,
                                group = party,
                                fill = party,
                                text = paste0("Category: ", party, "<br>Value: ", round(.upper, 2)))) +
    facet_wrap(as.formula(paste("~", input$group)), nrow = 3) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.9) +
    geom_errorbar(width = 0.2, position = position_dodge(width = 0.9), alpha = 0.9) +
    labs(
      title = "",
      x = "",
      y = "Feeling Thermometer"
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 14))  + 
    scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
    scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
    theme(legend.position = "none") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.spacing = unit(1, "lines"))
  
  ggplotly(plot, tooltip = "none") 
})
output$radar <- renderPlotly({
  summary_df <- dat %>%
    group_by(!!sym(input$group)) %>%
    summarize(across(all_of(outcomes), mean, na.rm = TRUE)) %>%
    na.omit() %>%
    rename(
      `Joe Biden` = biden_ft,
      `Katie Hobbs` = hobbs_ft,
      `Mark Kelly` = kelly_ft,
      `Progressive Democrats` = progressive_democrat_feelings,
      `Establishment Democrats` = establishment_democrat_feelings ,
      `Donald Trump` = trump_ft ,
      `Kari Lake` = lake_ft ,
      `Blake Masters` = masters_ft,
      `Establishment Republicans` = establishment_republican_feelings,
      `MAGA Republicans` = maga_republican_feelings
    )
  
  # Extract group names
  group_names <- summary_df[[input$group]]
  
  # Create a list of data frames for each group
  data_list <- lapply(group_names, function(group_name) {
    summary_df %>%
      filter(!!sym(input$group) == group_name) %>%
      select(-!!sym(input$group)) %>%
      as.numeric()
  })
  
  fig <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  )

  for (i in seq_along(data_list)) {
    group_data <- data_list[[i]]
    group_name <- group_names[i]
    
    fig <- fig %>%
      add_trace(
        r = group_data,
        theta = labels,
        name = group_name,
        text = paste0('<b>', group_name, ' evaluation of </b>', "<br><b>", labels, '</b>: ', round(group_data, 1), "/100"),
        hoverinfo = 'text',
        line = list(color = "black"),
        marker = list(color = 'rgba(255, 255, 255, 0.5)')  # 
      )
  }
  
  fig
})


  
  observe({
#    print(build_data_outcome())

  })
  
  
  
}

