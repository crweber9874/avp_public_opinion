### Run a series of ordered logit models on the AVP Survey
library(tidyverse)
library(brms)
library(dplyr)
library(purrr)

# Built from "buildDataAVP22.R #
df = read.csv("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avp_wave_1_wide.csv") %>%
  # Create an age cohort variable based on the variable age ( in  years old in 2022)
  mutate(age_cohort = case_when(
    age < 30 ~ "18-29",
    age >= 30 & age < 45 ~ "30-45",
    age >= 45 & age < 65 ~ "45-65",
    age >= 65 ~ "65+",
  ),
  rr = ifelse(racial_resentment > quantile(racial_resentment, 0.5), 1, 0)
  ) 

library(brms)
ordered_model <- function(dependent_variable = "attend_march", 
                          independent_variable = "party_identification3",
                          data = df, 
                          cores = 9,
                          iterations = 1000,
                          chains = 4,
                          priors =   c(prior(normal(0, 10), class = "b")),
                          ...) {
  data[[dependent_variable]] <- factor(data[[dependent_variable]], ordered = TRUE)
  data[[independent_variable]] <- as.character(data[[independent_variable]])
  
  formula <- as.formula(paste(dependent_variable, "~", independent_variable))
  
  # Define priors
  
  # Fit the model
  model <- brm(
    formula,
    data = data, 
    family = cumulative("logit"), 
    iter = iterations, 
    chains = 4,
    cores = getOption("mc.cores", cores),  # Adjust the number of cores as needed
    prior = priors
  )
  
  # survey = unique(model$data$survey)
  dat = expand_grid( 
    !!sym(independent_variable) := unique(na.omit(data[[independent_variable]]))) %>% as.data.frame() %>%
    tidybayes::add_epred_draws(model) %>%
    group_by(!!sym(independent_variable), .category) %>%
    summarize(               
      .value = mean(.epred),
      .lower = quantile(.epred, 0.025),
      .upper = quantile(.epred, 0.975)
    )  %>%
    mutate(
      item = dependent_variable
    )
  return(list(model = model, dat= dat))
}





run_models <- function(dependent_variables, independent_variables, data, cores = 9, 
                       iterations = 2000, chains = 4, ...) {
  results <- lapply(dependent_variables, function(dep_var) {
    lapply(independent_variables, function(ind_var) {
      model_result <- ordered_model(dep_var, ind_var, data, cores, iterations, chains, ...)$dat
      model_result <- model_result %>%
        mutate(dependent_variable = dep_var, independent_variable = ind_var)
      return(model_result)
    })
  })
  
  combined_results <- bind_rows(flatten(results))
  return(combined_results)
}


##### 

dat_guns <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = c( "background_guns", 
                           "registry_guns", 
                           "age_guns"),
  independent_variables = c("party_identification3", "conservative3", "female", "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc",  "kids_in_home", "college", "christian" ),
  data = df)

save(dat_guns, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/guns/data_guns.RData")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/guns/dat1.rda")




## These models need to be run in batches ####
dat_immigration <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = c("immigration_rate_long", "smart_border", 
                          "immigration_to_az", "border_wall", 
                          "citizen", "separate_parents", 
                          "cali_migration"),
  independent_variables = c("party_identification3", "conservative3", "female", 
                            "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc",  "kids_in_home", "college", "christian" ),
  data = df)

save(dat_immigration, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/immigration/data_immigration.rda")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/immigration/dat1.rda")




## These models need to be run in batches ####
dat_abortion <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = c("abortion_legal", 
                          "abortion_jail"),
  independent_variables = c("party_identification3", "conservative3", "female", "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc",  "kids_in_home", "college", "christian" ),
  data = df)

save(dat_abortion, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/abortion/data_abortion.rda")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/abortion/dat1.rda")


#### Water

dat_water <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = c( "limit_water", 
                           "tax_water",
                           "water_supply"),
  independent_variables = c("party_identification3", "conservative3", "female", 
                            "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc", 
                            "kids_in_home", "college", "christian" ),
  data = df)

save(dat_water, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/water/data_water.RData")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/water/dat1.rda")


# Example usage
dat_contest <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = c("attend_march", "burn_flag" , "recount",
                          "court", "criticize_election", "state_certify",
                          "certify" , "new_election", "stolen_2020"),
  independent_variables = c("party_identification3", "conservative3", "female", "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc",  "kids_in_home", "college", "christian" ),
  data = df)

save(dat_contest, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/dat_contest.rda")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/dat1.rda")

### Bind everything and save
rm(list = ls())
load("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/data.RData")
contestation = dat
load("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/data2.RData")
policy1 = dat
load("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/data3.RData")
policy2 = dat
bind_rows(contestation, policy1, policy2) %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/
            contestation/contestation.csv", row.names = FALSE)



### Run a series of ordered logit models on the AVP Survey
library(tidyverse)
library(brms)
library(dplyr)
library(purrr)

# Built from "buildDataAVP22.R #
df = read.csv("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avp_wave_1_wide.csv") %>%
  # Create an age cohort variable based on the variable age ( in  years old in 2022)
  mutate(age_cohort = case_when(
    age < 30 ~ "18-29",
    age >= 30 & age < 45 ~ "30-45",
    age >= 45 & age < 65 ~ "45-65",
    age >= 65 ~ "65+",
  ),
  rr = ifelse(racial_resentment > quantile(racial_resentment, 0.5), 1, 0)
  ) 

library(brms)
linear_model <- function(dependent_variable = "biden_ft", 
                         independent_variable = "party_identification3",
                         data = df, 
                         cores = 9,
                         iterations = 1000,
                         chains = 4,
                         priors =   c(prior(normal(0, 10), class = "b")),
                         ...) {
  data[[independent_variable]] <- as.character(data[[independent_variable]])
  
  formula <- as.formula(paste(dependent_variable, "~", independent_variable))
  
  # Define priors
  
  # Fit the model
  model <- brm(
    formula,
    data = data, 
    family = gaussian(), 
    iter = iterations, 
    chains = 4,
    cores = getOption("mc.cores", cores),  # Adjust the number of cores as needed
    prior = priors
  )
  
  # survey = unique(model$data$survey)
  dat = expand_grid( 
    !!sym(independent_variable) := unique(na.omit(data[[independent_variable]]))) %>% as.data.frame() %>%
    tidybayes::add_epred_draws(model) %>%
    group_by(!!sym(independent_variable)) %>%
    summarize(               
      .value = mean(.epred),
      .lower = quantile(.epred, 0.025),
      .upper = quantile(.epred, 0.975)
    )  %>%
    mutate(
      item = dependent_variable
    )
  return(list(model = model, dat= dat))
}

run_models <- function(dependent_variables, independent_variables, data, cores = 9, 
                       iterations = 2000, chains = 4, ...) {
  results <- lapply(dependent_variables, function(dep_var) {
    lapply(independent_variables, function(ind_var) {
      model_result <- linear_model(dep_var, ind_var, data, cores, iterations, chains, ...)$dat
      model_result <- model_result %>%
        mutate(dependent_variable = dep_var, independent_variable = ind_var)
      return(model_result)
    })
  })
  
  combined_results <- bind_rows(flatten(results))
  return(combined_results)
}

outcomes <- c(
  "biden_ft",
  "trump_ft",
  "lake_ft",
  "masters_ft",
  "hobbs_ft",
  "kelly_ft",
  "finchem_ft",
  "establishment_republican_feelings",
  "maga_republican_feelings",
  "establishment_democrat_feelings",
  "progressive_democrat_feelings"
)

dat_fit <- run_models(
  cores = 10,
  iterations = 1000,
  dependent_variables = outcomes,
  independent_variables = c("party_identification3", "conservative3", "female", "age_cohort", "latino", "white", 
                            "authoritarianism", "CD", "rr", "faminc",  "kids_in_home", "college", "christian" ),
  data = df)

save(dat_fit, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/ft/data_ft.RData")
save(df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/ft/dat1.rda")



# 
# g
# ###################################################################################################
# ###################################################################################################
# ###################################################################################################
# 
# ordered_model_groups <- function(dependent_variable = "attend_march", 
#                           independent_variable = "party_identification3",
#                           vector_of_controls = c("age_cohort", "female"),
#                           data = df, 
#                           cores = 9,
#                           iterations = 1000,
#                           chains = 4,
#                           priors =   c(prior(normal(0, 10), class = "b")),
#                           
#                           ...) {
#   data[[dependent_variable]] <- factor(data[[dependent_variable]], ordered = TRUE)
#   data[[independent_variable]] <- as.character(data[[independent_variable]])
#   controls_string <- paste(vector_of_controls, collapse = " + ")
#   formula_string <- paste(dependent_variable, "~", independent_variable, "+ survey +", controls_string)
#   formula <- as.formula(formula_string)  
#   # Define priors
#   
#   # Set levels for factor variables
#   factor_vars <- c("survey", vector_of_controls)
#   for (var in factor_vars) {
#     data[[var]] <- factor(data[[var]])
#   }
#   
#   
#   # Fit the model
#   model <- brm(
#     formula,
#     data = data, 
#     family = cumulative("logit"), 
#     iter = 2000, 
#     chains = 4,
#     cores = getOption("mc.cores", cores),  # Adjust the number of cores as needed
#     prior = priors
#   )
# 
#   # Expand grid for all independent variables and controls
#   expand_vars <- c(independent_variable, "survey", vector_of_controls)
#   grid_list <- lapply(expand_vars, function(var) unique(na.omit(data[[var]])))
#   names(grid_list) <- expand_vars
#   dat <- expand_grid(!!!grid_list) %>%
#     as.data.frame() %>%
#     tidybayes::add_epred_draws(model) %>%
#     group_by(across(all_of(expand_vars)), .category) %>%
#     summarize(               
#       .value = mean(.epred),
#       .lower = quantile(.epred, 0.025),
#       .upper = quantile(.epred, 0.975),
#       .groups = 'drop'
#     ) %>%
#     mutate(
#       item = dependent_variable
#     )
#   return(list(model = model, dat= dat))
# }
# 
# run_models <- function(dependent_variables, independent_variable, data,
#                        cores = 4,
#                        iterations = 2000, chains = 4,
#                        ...) {
#   results <- lapply(dependent_variables, function(dep_var) {
#     ordered_model_groups(dep_var, independent_variable,
#                          vector_of_controls = c("age_cohort", "female", "latino", "conservative3" ),
#                          data = data, cores = cores, iterations = iterations, chains = chains, ...)$dat
#   })
#   combined_results <- bind_rows(results, .id = "dependent_variable")
#   return(combined_results)
# }
# 
# # Example usage
# dat <- run_models(
#   dependent_variables = c("attend_march", "burn_flag", "court", 
#                           "recount", "criticize_election"),
#   independent_variable = c("party_identification3", 'conservative3'),
#   data = df
# )
# 
# save(dat, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/gradations/data.RData")
