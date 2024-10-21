library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("data_water.rda")

full_dat = df
dat = df



dat_water$age_cohort <- ifelse(dat_water$age_cohort=="30-45", "30-44", dat_water$age_cohort)
dat_water$age_cohort <- ifelse(dat_water$age_cohort=="45-65", "45-64", dat_water$age_cohort)