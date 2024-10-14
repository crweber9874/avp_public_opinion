
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
                      item = "burn_flag"){
  # Predictions
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
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Oppose", "Oppose", "Neutral",
                                         "Support", "Strongly Support")))
  data_df_noweights<- ungrouped_data %>%
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Oppose", 
                              `2` = "Oppose", 
                              `3` = "Neutral", 
                              `4` = "Support", 
                              `5` = "Strongly Support")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Oppose", "Oppose", "Neutral",
                                         "Support", "Strongly Support")))
  
  
  return(list(model_df, data_df_noweights, data_df_weights))
}

# Build Data  Agree

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
    group_by(.category = !!sym(item)) %>%
    summarise(.value = sum(survey_weight) / sum(full_dat %>% filter(survey == "AVP2") %>% pull(survey_weight))) 
  data_df_weights = data_df_weights %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Disagree", 
                              `2` = "Somewhat Disagree", 
                              `3` = "Neutral", 
                              `4` = "Somewhat Agree", 
                              `5` = "Strongly Agree")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Somewhat Disagree", "Neutral",
                                         "Somewhat Agree", "Strongly Agree")))
  data_df_noweights<- ungrouped_data %>%
    # filter(survey == "AVP1") %>%
    group_by(.category = !!sym(item)) %>%
    summarise(.value = n() / nrow(full_dat )) %>%
    mutate(.category = recode(.category,
                              `1` = "Strongly Disagree", 
                              `2` = "Somewhat Disagree", 
                              `3` = "Neutral", 
                              `4` = "Somewhat Agree", 
                              `5` = "Strongly Agree")) %>%
    mutate(.category = factor(.category, 
                              levels = c("Strongly Disagree", "Somewhat Disagree", "Neutral",
                                         "Somewhat Agree", "Strongly Agree")))
  
  
  return(list(model_df, data_df_noweights, data_df_weights))
}
