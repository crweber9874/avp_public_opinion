# A YouGov codebook
library(pdftools)
library(tidyverse)
# Read text from the entire PDF
pdf_text <- pdf_text("/Users/Chris/Dropbox/masterData/azDat/western/codebook/BYUC0025_codebook.pdf") 
# View a formatted
cat(pdf_text)
# Format
library(stringr)
# IF Name:          WSS01_16
# Description:   Geographic region you currently live - Chihuahua, Mexico
# Write a regex that pulls out the variable after Name: and also "Description", and also the Code and Label
pattern <- "Name:\\s+([A-Z0-9_]+)\\s+Description:\\s+(.*)"
matches <- str_match(pdf_text, pattern) %>% as_tibble() %>% na.omit()
