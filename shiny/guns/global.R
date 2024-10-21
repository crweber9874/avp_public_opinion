library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("data_guns.rda")

dat = df
full_dat = df


dat$age_cohort <- ifelse(dat$age_cohort=="30-44", "30-44", dat$age_cohort)
df$age_cohort <- ifelse(df$age_cohort=="45-64", "45-64", df$age_cohort)


dat_guns$age_cohort <- ifelse(dat_guns$age_cohort=="30-45", "30-44", dat_guns$age_cohort)
dat_guns$age_cohort <- ifelse(dat_guns$age_cohort=="45-65", "45-64", dat_guns$age_cohort)