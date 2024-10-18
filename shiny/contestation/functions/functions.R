
#UA color palette
az_color <- function(color = c("azblue", "azred", "oasis", "grey", "warmgrey", "midnight", "azurite", "chili", "white")) {
  if(color == "azblue") {return("#0C234B")}
  if(color == "azred")  {return("#AB0520")}
  if(color == "oasis")  {return("#378DBD")}
  if(color == "azgrey")   {return("#E2E9EB")}
  if(color == "warmgrey") {return("#F4EDE5")}
  if(color == "midnight") {return("#001C48")}
  if(color == "azurite") {return("#1E5288")}
  if(color == "chili") {return("#8B0015")}
  if(color == "azwhite") {return("#FFFFFF")}
}

build_data <- function(prediction_data = dat_abortion, 
                       ungrouped_data = full_dat,
                       item = "abortion_legal",
                       recode_scheme = "oppose5r") {
  if (recode_scheme == "oppose5r") {  
    model_df <- prediction_data %>% 
      filter(item == {{item}}) %>%
      mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                   "Value: ", round(.value, 2), "<br>",
                                   "Lower: ", round(.lower, 2), "<br>"),
             jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5))
    
    data_df_weights <- ungrouped_data %>%
      filter(!is.na(!!sym(item))) %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = sum(survey_weight) / sum(full_dat  %>% pull(survey_weight)))
    
    data_df_weights <- data_df_weights %>%
      mutate(.category = recode(.category,
                                `5` = "Strongly Support", 
                                `4` = "Support", 
                                `2` = "Oppose", 
                                `1` = "Strongly Oppose", 
                                `3` = "Not Sure")) %>%
      mutate(.category = factor(.category, 
                                levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                           "Support", "Strongly Support")))
    
    data_df_noweights <- ungrouped_data %>%
      filter(!is.na(!!sym(item))) %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = n() / nrow(full_dat)) %>%
      mutate(.category = recode(.category,
                                `5` = "Strongly Support", 
                                `4` = "Support", 
                                `2` = "Oppose", 
                                `1` = "Strongly Oppose", 
                                `3` = "Not Sure")) %>%
      mutate(.category = factor(.category, 
                                levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                           "Support", "Strongly Support")))
  }
  
  if (recode_scheme == "abortion") {  
    model_df <- prediction_data %>% 
      filter(item == {{item}}) %>%
      mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                   "Value: ", round(.value, 2), "<br>",
                                   "Lower: ", round(.lower, 2), "<br>"),
             jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5))
    
    data_df_weights <- ungrouped_data %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = sum(survey_weight) / sum(full_dat %>% pull(survey_weight)))
    
    data_df_weights <- data_df_weights %>%
      mutate(.category = recode(.category,
                                `4` = "Abortion Not Legal",
                                `3` = "First Trimester",
                                `2` = "First and Second Trimester",
                                `1` = "Completely Legal at Any Point")) %>%
      mutate(.category = factor(.category,
                                levels = c("Abortion Not Legal",
                                           "First Trimester",
                                           "First and Second Trimester",
                                           "Completely Legal at Any Point")))
    
    data_df_noweights <- ungrouped_data %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = n() / nrow(full_dat)) %>%
      mutate(.category = recode(.category,
                                `4` = "Abortion Not Legal", 
                                `3` = "First Trimester", 
                                `2` = "First and Second Trimester", 
                                `1` = "Completely Legal at Any Point")) %>%
      mutate(.category = factor(.category, 
                                levels = c("Abortion Not Legal", 
                                           "First Trimester", 
                                           "First and Second Trimester", 
                                           "Completely Legal at Any Point")))
  }
  
  return(list(model_df = model_df, 
              data_df_noweights = data_df_noweights, 
              data_df_weights = data_df_weights))
}


