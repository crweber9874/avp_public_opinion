df = read.csv("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avpWide.csv")

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
  
  formula <- as.formula(paste(dependent_variable, "~", independent_variable, "+ survey"))
  
  # Define priors
  
  # Fit the model
  model <- brm(
    formula,
    data = data, 
    family = cumulative("logit"), 
    iter = 2000, 
    chains = 4,
    cores = getOption("mc.cores", cores),  # Adjust the number of cores as needed
    prior = priors
  )
  
  dat = expand_grid( 
    !!sym(independent_variable) := unique(na.omit(data[[independent_variable]])),
    survey = unique(model$data$survey)) %>% as.data.frame() %>%
    tidybayes::add_epred_draws(model) %>%
    group_by(!!sym(independent_variable), survey, .category) %>%
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


## Loop through everything 
run_models <- function(dependent_variables, independent_variable, data, cores = 4, iterations = 2000, chains = 4, ...) {
  results <- lapply(dependent_variables, function(dep_var) {
    ordered_model(dep_var, independent_variable, data, cores, iterations, chains, ...)$dat
  })
  combined_results <- bind_rows(results, .id = "dependent_variable")
  return(combined_results)
}

dat = run_models(
  dependent_variables = c("attend_march", "burn_flag", "court", "recount", "criticize_election"),
  independent_variable = "party_identification3",
  data = df
)  

save(dat, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/shiny/contestation/data.RData")
