
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

# Build Data Oppose Support
build_data = function(prediction_data = dat, 
                      ungrouped_data = full_dat,
                      item = "burn_flag",
                      type = "severe"){
  # Predictions
  if(type == "severe"){
  model_df = prediction_data %>% 
    filter(item == {{item}})  %>%
    mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                 "Value: ", round(.value, 2), "<br>",
                                 "Lower: ", round(.lower, 2), "<br>"),
           jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
  ### 
  data_df_weights<- ungrouped_data %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
  data_df_weights = data_df_weights %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Not Sure", 
                              `4` = "Support", 
                              `5` = "Strongly Support")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                         "Support", "Strongly Support")))
  data_df_noweights<- ungrouped_data %>%
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Not Sure", 
                              `4` = "Support", 
                              `5` = "Strongly Support")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                         "Support", "Strongly Support")))
  }
  
  return(list(model_df, data_df_noweights, data_df_weights))
}

# Fix this: When the separation question was asked; neither was included.
# That was an error; and not sure should be appropriately classified

build_data_agree = function(prediction_data = dat, 
                            ungrouped_data = full_dat,
                            item = "stolen_2020"){
  # Predictions
  model_df = prediction_data %>% 
    filter(item == {{item}})  %>%
    mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                 "Value: ", round(.value, 2), "<br>",
                                 "Lower: ", round(.lower, 2), "<br>"),
           jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
  ### 
  data_df_weights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    group_by(.category = !!sym(item)) %>%
  summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
  data_df_weights = data_df_weights %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Agree", 
                              `2` = "Somewhat Agree", 
                              `3` = "Neither Agree nor Disagree", 
                              `4` = "Somewhat Disagree", 
                              `5` = "Strongly Disagree")  
           ) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Somewhat Disagree","Neither Agree nor Disagree", 
                                         "Somewhat Agree", "Strongly Agree")))
  data_df_noweights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Agree", 
                              `2` = "Somewhat Agree", 
                              `3` = "Neither Agree nor Disagree", 
                              `4` = "Somewhat Disagree", 
                              `5` = "Strongly Disagree")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Somewhat Disagree","Neither Agree nor Disagree", 
                                         "Somewhat Agree", "Strongly Agree")))
  
  
  return(list(model_df = model_df, data_df_noweights = data_df_noweights,data_df_weights= data_df_weights))
}


build_data_agree_4 = function(prediction_data = dat, 
                            ungrouped_data = full_dat,
                            item = "citizen"){
  # Predictions
  model_df = prediction_data %>% 
    filter(item == {{item}})  %>%
    mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                 "Value: ", round(.value, 2), "<br>",
                                 "Lower: ", round(.lower, 2), "<br>"),
           jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
  ### 
  data_df_weights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    group_by(.category = !!sym(item)) %>%
    summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
  data_df_weights = data_df_weights %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Agree", 
                              `2` = "Agree", 
                              `3` = "Disagree", 
                              `4` = "Strongly Disagree")  
    ) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Disagree",
                                         "Agree", "Strongly Agree")))
  
  data_df_noweights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Agree", 
                              `2` = "Agree", 
                              `3` = "Disagree", 
                              `4` = "Strongly Disagree")  
    ) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Disagree",
                                         "Agree", "Strongly Agree")))
  
  
  return(list(model_df = model_df, data_df_noweights = data_df_noweights,data_df_weights= data_df_weights))
}



build_data_immigration = function(prediction_data = dat, 
                              ungrouped_data = full_dat,
                              item = "citizen"){
  # Predictions
  model_df = prediction_data %>% 
    filter(item == {{item}})  %>%
    mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                 "Value: ", round(.value, 2), "<br>",
                                 "Lower: ", round(.lower, 2), "<br>"),
           jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
  ### 
  data_df_weights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    group_by(.category = !!sym(item)) %>%
    summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
  data_df_weights = data_df_weights %>%
    mutate(.category = recode(.category,
                              `5` = "Decrease a lot", 
                              `4` = "Decrease", 
                              `3` = "Stay the same", 
                              `2` = "Increase", 
                              `1` = "Increase a lot")  
    ) %>%
    mutate(.category = factor(.category, 
                              levels = c("Decrease a lot", "Decrease", "Stay the same",
                                         "Increase", "Increase a lot")))
  
  data_df_noweights<- ungrouped_data %>%
    filter(!is.na(!!sym(item))) %>%  # Omit rows where .category is NA
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `5` = "Decrease a lot", 
                              `4` = "Decrease", 
                              `3` = "Stay the same", 
                              `2` = "Increase", 
                              `1` = "Increase a lot")  
    ) %>%
    mutate(.category = factor(.category, 
                              levels = c("Decrease a lot", "Decrease", "Stay the same",
                                         "Increase", "Increase a lot")))
  
  return(list(model_df = model_df, data_df_noweights = data_df_noweights,data_df_weights= data_df_weights))
}



build_data_abortion = function(prediction_data = dat, 
                            ungrouped_data = full_dat,
                            item = "abortion_jail"){
if(item == "abortion_legal"){  
    model_df = prediction_data %>% 
      filter(item == {{item}})  %>%
      mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                   "Value: ", round(.value, 2), "<br>",
                                   "Lower: ", round(.lower, 2), "<br>"),
             jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
    ### 
    data_df_weights<- ungrouped_data %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
    
    data_df_weights = data_df_weights %>%
      mutate(.category = recode(.category,
                                `1` = "Strongly Support", 
                                `2` = "Support", 
                                `3` = "Oppose", 
                                `4` = "Strongly Oppose", 
                                `5` = "Not Sure")) %>%
      mutate(.category = factor(.category, 
                                levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                           "Support", "Strongly Support")))
    data_df_noweights<- ungrouped_data %>%
      # filter(survey == "AVP1") %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = n() / nrow(full_dat )) %>%
      mutate(.category = recode(.category,
                                `1` = "Strongly Support", 
                                `2` = "Support", 
                                `3` = "Oppose", 
                                `4` = "Strongly Oppose", 
                                `5` = "Not Sure"))  %>%
      mutate(.category = factor(.category, 
                                levels = c("Strongly Oppose", "Oppose", "Not Sure",
                                           "Support", "Strongly Support")))
}
    
if(item == "abortion_jail"){  
    model_df = prediction_data %>% 
      filter(item == {{item}})  %>%
      mutate(tooltip_text = paste0("Category: ", .category, "<br>",
                                   "Value: ", round(.value, 2), "<br>",
                                   "Lower: ", round(.lower, 2), "<br>"),
             jittered_category = jitter(as.numeric(as.factor(.category)), amount = 0.5)) 
    ### 
    data_df_weights<- ungrouped_data %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
    
    data_df_weights = data_df_weights %>%
      mutate(.category = recode(.category,
                                `1` = "Abortion Not Legal", 
                                `2` = "First and Second Trimester", 
                                `3` = "Not Sure", 
                                `4` = "First Trimester", 
                                `5` = "Completely Legal at Any Point")) %>%

      mutate(.category = factor(.category, levels = c("Not Sure", "Completely Legal at Any Point", 
                                                      "First and Second Trimester",
                                                      "First Trimester", 
                                                      "Abortion Not Legal"))) 
    data_df_noweights<- ungrouped_data %>%
      # filter(survey == "AVP1") %>%
      group_by(.category = !!sym(item)) %>%
      summarise(.value = n() / nrow(full_dat )) %>%
      mutate(.category = recode(.category,
                                `1` = "Abortion Not Legal", 
                                `2` = "First and Second Trimester", 
                                `3` = "Not Sure", 
                                `4` = "First Trimester", 
                                `5` = "Completely Legal at Any Point")) %>%
      
      mutate(.category = factor(.category, levels = c("Not Sure", "Completely Legal at Any Point", 
                                                      "First and Second Trimester",
                                                      "First Trimester", 
                                                      "Abortion Not Legal")))
    }
  
  
  return(list(model_df = model_df, data_df_noweights = data_df_noweights,data_df_weights= data_df_weights))
}



