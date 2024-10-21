library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)


load("dat1.rda")
load("data_ft.rda")

full_dat = df
dat = df

# Clean the DF
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
  "maga_republican_feelings",
  "finchem_ft"
)

labels <- c(
  "Joe Biden",
  "Katie Hobbs",
  "Mark Kelly",
  "Progressive Democrats",
  "Establishment Democrats",
  "Donald Trump",
  "Kari Lake",
  "Blake Masters",
  "Establishment Republicans",
  "MAGA Republicans",
  "Mark Finchem"
)

labels_party <- c(
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Democrat",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican",
  "Republican"

)


# Create a named vector for recoding
recode_vector <- setNames(labels, outcomes)
recode_vector_party <- setNames(labels_party, outcomes)

data = dat_fit %>%
  mutate(
    college = ifelse(college == 1, "College", "Less than College"),
    party_identification3 = recode(party_identification3, 
                                   `1` = "Democrat", 
                                   `2` = "Independent", 
                                   `3` = "Republican"),
    conservative3 = recode(conservative3, `1` = "Liberal", 
                           `2` = "Moderate", 
                           `3` = "Conservative"),
    conservative3 = factor(conservative3, levels = c("Liberal", "Moderate", "Conservative")),
    rr = ifelse(rr == 1, "High Racial Resentment", "Low Racial Resentment"),
    authoritarianism = ifelse(authoritarianism == 1, "High Social Conformity Orientation", "Low Social Conformity Orientation"),
    christian = ifelse(christian == 1, "Christian", "Non Christian"),
    faminc = ifelse(faminc == 1, "$80k or more", "Less than $80k"),
    kids_in_home = ifelse(kids_in_home == 1, "Kids live at home", "No kids in home"),
    female = ifelse(female == 1, "Female", "Male"),
    white = ifelse(white == 1, "White", "Person of Color"),
    latino = ifelse(latino == 1, "Latino", "Non Latino")) %>%
  mutate(dv = recode(dependent_variable, !!!recode_vector)) %>%
  mutate(party = recode(dependent_variable, !!!recode_vector_party)) %>%
  filter(item != "finchem_ft")
  # recode authoritarianism as 1 or 0 with everything in between as NA
  # mutate(authoritarianism = case_when(
  #   authoritarianism ==  0 ~  0,
  #   authoritarianism ==  1 ~  1,
  #   TRUE ~ ""
  # )) 

data$dv <- factor(data$dv, levels = unique(data$dv[order(data$party, data$dv)]))


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
  "Katie Hobbs",
  "Mark Kelly",
  "Progressive Democrats",
  "Establishment Democrats",
  "Donald Trump",
  "Kari Lake",
  "Blake Masters",
  "Establishment Republicans",
  "MAGA Republicans"
)

dat = dat %>%
  mutate(
college = ifelse(college == 1, "College", "Less than College"),
party_identification3 = recode(party_identification3, 
                               `1` = "Democrat", 
                               `2` = "Independent", 
                               `3` = "Republican"),
conservative3 = recode(conservative3, `1` = "Liberal", 
                       `2` = "Moderate", 
                       `3` = "Conservative"),
conservative3 = factor(conservative3, levels = c("Liberal", "Moderate", "Conservative")),
rr = ifelse(rr == 1, "High Racial Resentment", "Low Racial Resentment"),
christian = ifelse(christian == 1, "Christian", "Non Christian"),
faminc = ifelse(faminc == 1, "$80k or more", "Less than $80k"),
kids_in_home = ifelse(kids_in_home == 1, "Kids live at home", "No kids in home"),
female = ifelse(female == 1, "Female", "Male"),
white = ifelse(white == 1, "White", "Other Racial Group"),
latino = ifelse(latino == 1, "Latino", "Non Latino")) 



        
  