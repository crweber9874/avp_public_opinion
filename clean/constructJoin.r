### Create in build.....
w1_long <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_1_long.csv")

w1_wide <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_1_wide.csv")

wave1_names <- names(w1_wide)

w2_long <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_2_long.csv")

w2_wide <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_2_wide.csv")

wave2_names <- names(w2_wide)

western_wide <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/datWestern2020_wide.csv")

western_long <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/datWestern2020_long.csv")


library(dplyr)
library(tidyr)
# Crate common over time data.
common_columns <- intersect(wave1_names, wave2_names)

w1t_wide <- w1_wide %>% select(all_of(common_columns))
w1t_wide$wave <- 2022
w2t_wide <- w2_wide %>% select(all_of(common_columns))
w2t_wide$wave <- 2024
dat <- rbind(w1t_wide, w2t_wide) %>%
  mutate_if(is.numeric, as.character) %>%
  mutate(rid = row_number()) %>%
  pivot_longer(cols = -c(rid, wave, survey_weight, county_fips, CD, LD, party3)) %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_w1w2_long.csv", row.names = FALSE)


### Join Waves 1 and 2 ####
# Label all variables in wave 2 _2 and all variables in wave 1 _1, except caseid22
names(w1_wide) <- paste0(names(w1_wide), "_1")
w1_wide$caseid22 <- w1_wide$caseid22_1
names(w2_wide) <- paste0(names(w2_wide), "_2")
w2_wide$caseid22 <- w2_wide$caseid22_2

inner_join(w1_wide, w2_wide, by = "caseid22") %>% write.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_panel.csv", row.names = FALSE)
