library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("data_immigration.rda")

full_dat = df
dat = df



dat_immigration$age_cohort <- ifelse(dat_immigration$age_cohort=="30-45", "30-44", dat_immigration$age_cohort)
dat_immigration$age_cohort <- ifelse(dat_immigration$age_cohort=="45-65", "45-64", dat_immigration$age_cohort)