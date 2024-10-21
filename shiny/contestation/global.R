library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("dat_contest.rda")

full_dat = df
dat = df

dat_contest$age_cohort <- ifelse(dat_contest$age_cohort=="30-45", "30-44", dat_contest$age_cohort)
dat_contest$age_cohort <- ifelse(dat_contest$age_cohort=="45-65", "45-64", dat_contest$age_cohort)