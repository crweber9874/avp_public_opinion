library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("data_abortion.rda")
full_dat = df

table(dat_abortion$age_cohort, exclude = NULL)
dat_abortion$age_cohort <- ifelse(dat_abortion$age_cohort=="30-45", "30-44", dat_abortion$age_cohort)
dat_abortion$age_cohort <- ifelse(dat_abortion$age_cohort=="45-65", "45-64", dat_abortion$age_cohort)