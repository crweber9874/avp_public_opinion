df = read.csv("~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avpWide.csv")



### Function to generate model 
library(MASS)
library(brms)
library(tidybayes)
library(dplyr)
library(tidyverse)


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
     !!sym(independent_variable) := unique(data[[independent_variable]]),
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

# Example usage
test = rbind(
ordered_model(
  dependent_variable = "attend_march", 
  independent_variable = "party_identification3",
  data = df
)$dat,
ordered_model(
  dependent_variable = "burn_flag", 
  independent_variable = "party_identification3",
  data = df
)$dat
) 

## Loop through everything 
run_models <- function(dependent_variables, independent_variable, data, cores = 4, iterations = 2000, chains = 4, ...) {
  results <- lapply(dependent_variables, function(dep_var) {
    ordered_model(dep_var, independent_variable, data, cores, iterations, chains, ...)$dat
  })
  combined_results <- bind_rows(results, .id = "dependent_variable")
  return(combined_results)
}

## Construct Plotly Histogram Function 
plotly_histogram <- function(data, x, y, color, title, xlab, ylab) {
  p <- ggplot(data, aes(x = x, y = y, fill = color)) +
    geom_bar(stat = "identity") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
  
  p <- ggplotly(p)
  return(p)
}
# example usage
run_models(
  dependent_variables = c("attend_march", "burn_flag", "court", "recount", "criticize_election"),
  independent_variable = "party_identification3",
  data = df
) %>% 
  plotly_histogram(
    x = ".category",
    y = ".value",
    color = "party_identification3",
    title = "Predicted Probability of Attending a March by Party Identification",
    xlab = "Category",
    ylab = "Predicted Probability"
  )




# generate prediction in tidybayes
d = expand_grid( party_identification3 = c("Democrat", "Independent", "Republican"),
                 survey = c("Western", "AVP1", "AVP2")) %>% as.data.frame() %>%
        tidybayes::add_epred_draws(model) %>%
  group_by(party_identification3, survey, .category) %>%
  summarize(
    .value = mean(.epred),
    .lower = quantile(.epred, 0.025),
    .upper = quantile(.epred, 0.975)
  )  %>% mutate(.category = recode(.category, `1` = "Strongy Oppose", `2` ="Somewhat Oppose",
                            `3` = "Neutral", `4` ="Somewhat Support", `5` ="Strongly Support"))%>% 
  filter(survey != "Western") %>%
  filter(party_identification3 == "Democrat") %>%
  mutate(survey = factor(survey, levels = c("AVP1", "AVP2"))) %>%
  mutate(.category = factor(.category, levels = c("Strongy Oppose", "Somewhat Oppose", "Neutral", "Somewhat Support", "Strongly Support"))) %>%
  mutate(survey = recode(survey,  "AVP1" = "2022", "AVP2" = "2024")) %>%
ggplot(aes(x = .category, y = .value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), position = position_dodge(width = 0.5), width = 0.25) +
  facet_wrap(~survey) +
  labs(x = "Category", y = "Predicted Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(dplyr)
library(tidyr)
library(brms)
library(tidybayes)
library(ggplot2)
library(plotly)

# Create the new data frame for predictions
d <- expand_grid(
  party_identification3 = c("Democrat", "Independent", "Republican"),
  survey = c("Western", "AVP1", "AVP2")
) %>% 
  as.data.frame() %>%
  tidybayes::add_epred_draws(model) %>%
  group_by(party_identification3, survey, .category) %>%
  summarize(
    .value = mean(.epred),
    .lower = quantile(.epred, 0.025),
    .upper = quantile(.epred, 0.975)
  ) %>%
  mutate(.category = recode(.category, `1` = "Strongly Oppose", `2` = "Somewhat Oppose",
                            `3` = "Neutral", `4` = "Somewhat Support", `5` = "Strongly Support")) %>%
  filter(survey != "Western") %>%
  filter(party_identification3 == "Democrat") %>%
  mutate(survey = recode(survey, "AVP1" = "2022", "AVP2" = "2024")) 

# Create the bar plot with confidence intervals using ggplot2
color <- ifelse(
  d$party_identification3 == "Democrat",
  "blue",
  ifelse(
    d$party_identification3 == "Independent",
    "blue",
    "red"
  ))

  
  
p <- ggplot(d, aes(x = .category, y = .value, fill = party_identification3)) +
  geom_bar(stat = "identity", fill = color, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), position = position_dodge(width = 0.2), width = 0.25) +
  facet_wrap(~survey) +
  labs(x = "Category", y = "Predicted Probability", fill = "Party Identification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Convert the ggplot2 plot to a Plotly plot
plotly_plot <- ggplotly(p)
# update hover
plotly_plot <- plotly_plot %>%
  layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
# format tooltip
plotly_plot <- plotly_plot %>%
  layout(tooltip = list(
    shared = TRUE,
    formatter = "group",
    mode = "x"
  ))


### the To do: Open the viz directory -- pull out all the necesary functions and what not
### Use the shiny examples as a template, this plot needs ot be colored correclty and 
# Check ordeirng




# modify the tooltip using html
plotly_plot <- plotly_plot %>%
  layout(hoverinfo = "text",
         hoverlabel = list(bgcolor = "white", font = list(color = "black")),
         hovertemplate = paste(
           "<b>Category:</b> %{x}<br>",
           "<b>Predicted Probability:</b> ${round({y}, 2)<br>",
           "<b>Predicted Probability:</b> %{d$party_identification3}<br>"
         )
  )
# Print the Plotly plot
plotly_plot



+ 
  labs(
    x = "Survey",
    y = "Predicted Probability",
    color = "Party Identification",
    fill = "Party Identification",
    title = "Predicted Probability of Attending a March by Survey and Party Identification"
  ) 
  





# Fit the ordinal logistic regression model

df %>% 
  dplyr::select(survey, conservative3, party_identification3, id, attend_march, burn_flag, court, recount, criticize_election) %>%
  pivot_longer(cols = -c(survey, conservative3, party_identification3, id), names_to = "variable", values_to = "value") %>%
  # Create the bar plot
  ggplot(aes(x = value)) +
  facet_wrap(~variable) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

%>%

%>% 
  

  
  formula <- bf(sdo ~ authoritarianism + college + authoritarianism:college)

# Specify the priors
priors <- c(
  prior(normal(0, 10), class = "b"),
  prior(normal(0, 10), class = "Intercept"),
  prior(uniform(0, 100), class = "sigma")
)

# Fit the model
fit <- brm(formula, data = dat, prior = priors, chains = 1, iter = 2000, warmup = 1000)

summary(fit)

# Plot the results
plot(fit)
%>% 
  print()

library(MASS)
library(brms)
library(tidybayes)
library(dplyr)
library(tidyverse)
df$attend_march <- factor(df$attend_march, ordered = TRUE)
df = df %>%
  mutate(
    party_identification3 = recode(party_identification3, `1` = "Democrat", `2` = "Independent", `3` = "Republican"),
    survey = as.character(survey)
  ) 
model <- brm(
  attend_march ~ as.character(party_identification3) + as.character(survey), 
  data = df, 
  family = cumulative("logit"), 
  iter = 2000, 
  chains = 4,
  cores = getOption("mc.cores", 9)
)
# generate prediction in tidybayes
d = expand_grid( party_identification = c("Democrat", "Independent", "Republican"),
                   survey = c("Western", "AVP1", "AVP2")) %>% as.data.frame()

predictions <- 
  tidybayes::add_epred_draws(model, newdata = d)


%>%
  group_by(authoritarianism, college) %>%
  mutate(college = ifelse(college == 1, "College", "Less than College")) %>%
  summarize(
    .value = mean(.epred),
    .lower = quantile(.epred, 0.025),
    .upper = quantile(.epred, 0.975)
  ) %>%
  ggplot(aes(x = authoritarianism, y = .value, color = factor(college))) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3) +
  facet_wrap(~college) +
  labs(
    x = "Authoritarianism",
    y = "Social Dominance Orientation",
    color = "Education",
    title = "Predicted Social Dominance Orientation \nby Authoritarianism and Education"
  ) + 
  theme_minimal()



# Print the model summary
summary(model)
# Generate predictions. The prediction dataset should include the effect of party_identification on a three response variable
# Oppose, Neutral, Support

# The ggplot will include the predicted probabilities for each category of the response variable, crossed with party 
# identification




chains = 4, iter = 2000)
                   
library(tidybayes)
library(dplyr)
library(ggplot2)
library(modelr)
fit <- brm(formula, data = dat, prior = priors, chains = 1, iter = 2000, warmup = 1000)

dat %>% data_grid( authoritarianism = seq_range(authoritarianism, n = 30),
                   college = c(0, 1)) %>%
  add_epred_draws(fit) %>%
  group_by(authoritarianism, college) %>%
  mutate(college = ifelse(college == 1, "College", "Less than College")) %>%
  summarize(
    .value = mean(.epred),
    .lower = quantile(.epred, 0.025),
    .upper = quantile(.epred, 0.975)
  ) %>%
  ggplot(aes(x = authoritarianism, y = .value, color = factor(college))) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3) +
  facet_wrap(~college) +
  labs(
    x = "Authoritarianism",
    y = "Social Dominance Orientation",
    color = "Education",
    title = "Predicted Social Dominance Orientation \nby Authoritarianism and Education"
  ) + 
  theme_minimal()             
                   

# Get the predicted probabilities for each category
predicted_probs <- predict(model, type = "probs")

# Calculate the predicted counts
predicted_counts <- colSums(predicted_probs) * nrow(df)

# Print the predicted counts
print(predicted_counts)