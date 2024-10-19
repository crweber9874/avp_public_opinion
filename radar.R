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
outcomes <- c(
  "biden_ft",
  "hobbs_ft",
  "kelly_ft",
  "progressive_democrat_feelings",
  "establishment_democrat_feelings",
  "trump_ft",
  "lake_ft",
  "masters_ft",
  "establishment_republican_feelings",
  "maga_republican_feelings"
)
labels <- list(
  "Joe Biden",
  "Katie Hobbs (Governor)",
  "Mark Kelly (Senate)",
  "Progressive Democrats",
  "Establishment Democrats",
  "Donald Trump",
  "Kari Lake",
  "Blake Masters",
  "Establishment Republicans",
  "MAGA Republicans"
)

summary_df <- df %>%
  filter(!is.na(party_identification3)) %>%
  group_by(party_identification3) %>%
  summarise(across(all_of(outcomes), mean, na.rm = TRUE)) %>%
  rename(
    `Joe Biden` = biden_ft,
    `Katie Hobbs (Governor)` = hobbs_ft,
    `Mark Kelly (Senate)` = kelly_ft,
    `Progressive Democrats` = progressive_democrat_feelings,
    `Establishment Democrats` = establishment_democrat_feelings ,
    `Donald Trump` = trump_ft ,
    `Kari Lake` = lake_ft ,
    `Blake Masters` = masters_ft,
    `Establishment Republicans` = establishment_republican_feelings,
    `MAGA Republicans` = maga_republican_feelings 
  )
# order columbs by whether dem or rep





# Create radar plot
fig <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
)

# Add traces with styled tooltips
fig <- fig %>%
  add_trace(
    r = rep,
    theta = labels,
    name = 'Republican',
    text = paste0('<b>Republican evaluation of </b>', "<br><b>", labels, '</b>: ', round(rep, 1), "/100"),
    hoverinfo = 'text',
    fillcolor = 'rgba(255, 0, 0, 0.3)',  # Red fill with transparency
    line = list(color = "red"),
    marker = list(color = 'rgba(255, 255, 255, 0.5)')  # White with slight transparency
  ) %>%
  add_trace(
    r = dem,
    theta = labels,
    name = 'Democrat',
    text = paste0('<b>Democratic evaluation of </b>', "<br><b>", labels, '</b>: ', round(dem, 1), "/100"),
    hoverinfo = 'text',
    fillcolor = 'rgba(0, 0, 255, 0.3)',  # Blue fill with transparency
    line = list(color = "blue"),
    marker = list(color = 'rgba(255, 255, 255, 0.5)')) %>%
  add_trace(
    r = ind,
    theta = labels,
    name = 'Independent',
    text = paste0('<b>Independent evaluation of </b>', "<br><b>", labels, '</b>: ', round(ind, 1), "/100"),
    hoverinfo = 'text',
    fillcolor = 'rgba(0, 0, 255, 0.3)',  # Blue fill with transparency
    line = list(color = "purple"),
    marker = list(color = 'rgba(255, 255, 255, 0.5)')) 





add_trace(
  r = ind,
  theta = outcomes,
  name = 'Independent',
  text = paste('Independent:', outcomes, ind),
  hoverinfo = 'text'
) %>%
  add_trace(
    r = rep,
    theta = outcomes,
    name = 'Republican',
    text = ~paste0("<b>", "Category: ", "</b>", outcomes, "<br>", 
                   "<b>","Probability: ","</b>", round(rep, 2)), # Tooltip text
    
    hoverinfo = 'text'
  )