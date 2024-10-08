# A YouGov codebook
library(pdftools)
library(tidyverse)
# Read text from the entire PDF
pdf_text <- pdf_text("/Users/Chris/Dropbox/masterData/azDat/western/codebook/BYUC0025_codebook.pdf") 
# View a formatted
cat(pdf_text)
# Format
library(stringr)
pattern <- "Name:\\s+([A-Z0-9_]+)\\s+Description:\\s+(.*)"
matches <- str_match(pdf_text, pattern) %>% as_tibble() %>% na.omit()



# A YouGov codebook
library(pdftools)
library(tidyverse)
# Read text from the entire PDF
pdf_text <- pdf_text("/Users/Chris/Dropbox/masterData/groupAuthoritarianism/AZ2022/UARZ0003_codebook.pdf") 
# View a formatted
cat(pdf_text)
# Format
library(stringr)
# Produces 
# Extract the Name

pattern <- "Name:\\s+(\\w+)[\\s\\S]*?Description:\\s+([\\s\\S]+?)(?:\\n\\s*Count|$)"  
matches <- str_match(pdf_text, pattern) %>% as_tibble() 

write.csv(matches, file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_1_helper.csv", row.names = FALSE)
