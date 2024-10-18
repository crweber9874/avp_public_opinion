library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

load("dat1.rda")
load("data_guns.rda")

dat = df
full_dat = df