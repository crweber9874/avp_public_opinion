# Build AVP Wave 1, 2022

library(haven)
library(tidyverse)
path <- file.path("/Users/Chris/Dropbox/masterData/AZ_Midterm/UARZ0003_OUTPUT.sav")
df <- read_sav(path)
original_columns <- names(df)
source("~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/R/recode.R")

df <- df %>%
  mutate(
    caseid22 = caseid,
    age = 2022 - birthyr,
    vote_ = recode(as.numeric(vote_2020), `1` = 1, `2` = 0),
    vote_trump_2020 = recode(as.numeric(presvote20post), `1` = 0, `2` = 1),
    vote_candidate = recode(as.numeric(vote_2022), `1` = 1, `2` = 0),
    vote_type = recodeAVP(vote, labels4, reverse = FALSE)$data,
    # Always code republican as 1, democrat as 0.
    vote_senate = recode(as.numeric(general_senate), `1` = 0, `2` = 1),
    vote_gov = recode(as.numeric(general_gov), `1` = 0, `2` = 1),
    vote_sos = recode(as.numeric(SoS), `1` = 0, `2` = 1),
    primary_type = case_when(
      as.numeric(primary) == 1 ~ "Absentee-Mail",
      as.numeric(primary) == 2 ~ "Absentee-Dropbox",
      as.numeric(primary) == 3 ~ "Early-Inperson",
      as.numeric(primary) == 4 ~ "Election Day-Inperson",
      TRUE ~ NA_character_ # Return NA as a character
    ),
    primary_rep = case_when(
      as.numeric(primary_rep) == 1 ~ "Republican",
      as.numeric(primary_rep) == 2 ~ "Democratic",
      as.numeric(primary_rep) == 3 ~ "Not Asked",
      TRUE ~ NA_character_
    ),
    primary_rep_gov = case_when(
      as.numeric(primary_dem) == 1 ~ "Kari Lake",
      as.numeric(primary_dem) == 2 ~ "Karrin Taylor Robson",
      as.numeric(primary_dem) == 3 ~ "Other",
      as.numeric(primary_dem) == 4 ~ "Other",
      TRUE ~ NA_character_
    ),
    primary_dem_gov = case_when(
      as.numeric(primary_dem_gov) == 1 ~ "Katie Hobbs",
      as.numeric(primary_dem_gov) == 2 ~ "Other",
      as.numeric(primary_dem_gov) == 3 ~ "Other",
      as.numeric(primary_dem_gov) == 4 ~ "Other",
      TRUE ~ NA_character_
    ),
    primary_rep_senate = case_when(
      as.numeric(primary_rep_senate) == 1 ~ "Blake Masters",
      as.numeric(primary_rep_senate) == 2 ~ "Jim Lamon",
      as.numeric(primary_rep_senate) == 3 ~ "Mark Brnovich",
      as.numeric(primary_rep_senate) == 4 ~ "Other",
      as.numeric(primary_rep_senate) == 5 ~ "Other",
      TRUE ~ NA_character_
    ),
    senate_alignment = ifelse(rank_order_senate_1 == 1, 1, 0),
    senate_ofparty = ifelse(rank_order_senate_2 == 1, 1, 0),
    senate_likedthem = ifelse(rank_order_senate_3 == 1, 1, 0),
    senate_dislikedother = ifelse(rank_order_senate_4 == 1, 1, 0),
    senate_thepast = ifelse(rank_order_senate_5 == 1, 1, 0),
    governor_alignment = ifelse(rank_order_governor_1 == 1, 1, 0),
    governor_ofparty = ifelse(rank_order_governor_2 == 1, 1, 0),
    governor_likedthem = ifelse(rank_order_governor_3 == 1, 1, 0),
    governor_dislikedother = ifelse(rank_order_governor_4 == 1, 1, 0),
    governor_thepast = ifelse(rank_order_governor_5 == 1, 1, 0),
    sos_alignment = ifelse(rank_order_SoS_1 == 1, 1, 0),
    sos_ofparty = ifelse(rank_order_SoS_2 == 1, 1, 0),
    sos_likedthem = ifelse(rank_order_SoS_3 == 1, 1, 0),
    sos_dislikedother = ifelse(rank_order_SoS_4 == 1, 1, 0),
    biden_ft = Biden,
    trump_ft = Trump,
    lake_ft = Lake,
    masters_ft = Masters,
    hobbs_ft = Hobbs,
    kelly_ft = Kelly,
    finchem_ft = Finchem,
    establishment_republican_feelings = EstabRep,
    maga_republican_feelings = MAGARep,
    establishment_democrat_feelings = EstabDem,
    progressive_democrat_feelings = ProgDem,
    biden_economy = ifelse(biden_economy == 1 | biden_economy == 2, 0, 1), # Worse
    # Policies
    cali_migration = recodeAVP(cali, labels5, reverse = FALSE)$data,
    immigration_rate = ifelse(immiga <= 2, 1, 0),
    immigration_rate_long = recodeAVP(as.numeric(immiga), reverse = TRUE, labels5)$data,
    separate_parents = recodeAVP(immig1, labels5, reverse = FALSE)$data,
    legal_status = recodeAVP(immig2, labels4, reverse = FALSE)$data,
    citizen = recodeAVP(immig3, labels4, reverse = FALSE)$data,
    smart_border = recodeAVP(immig4, labels4, reverse = FALSE)$data,
    water_supply = case_when(
      as.numeric(water1) == 1 ~ 5,
      as.numeric(water1) == 2 ~ 4,
      as.numeric(water1) == 3 ~ 3,
      as.numeric(water1) == 4 ~ 2,
      as.numeric(water1) == 5 ~ 1,
      TRUE ~ NA_real_
    ),
    immigration_to_az = recodeAVP(Q49, labels5, reverse = TRUE)$data,
    mip = case_when(
      as.numeric(MIP) == 1 ~ "economy",
      as.numeric(MIP) == 2 ~ "climate/environment",
      as.numeric(MIP) == 3 ~ "healthcare",
      as.numeric(MIP) == 4 ~ "racism",
      as.numeric(MIP) == 5 ~ "terrorism",
      as.numeric(MIP) == 6 ~ "abortion",
      as.numeric(MIP) == 7 ~ "crime",
      as.numeric(MIP) == 8 ~ "poverty",
      as.numeric(MIP) == 9 ~ "national security",
      as.numeric(MIP) == 10 ~ "education",
      as.numeric(MIP) == 11 ~ "democracy",
      as.numeric(MIP) == 12 ~ "other"
    ),
    limit_water = recode_policy_variables(water2, reverse = TRUE),
    tax_water = recode_policy_variables(water3, reverse = TRUE),
    reduce_water = recode_policy_variables(water5, reverse = TRUE),
    background_guns = recode_policy_variables(gun3, reverse = TRUE),
    registry_guns = recode_policy_variables(gun2, reverse = TRUE),
    age_guns = recode_policy_variables(gun4, reverse = TRUE),
    assault_guns = ifelse(gun5 == 1 | gun5 == 2, 1, 0),
    abortion_legal = recode(as.numeric(abortion2), `1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = NA_real_),
    abortion_jail = recode_policy_variables(abortion1, reverse = TRUE),
    violent_crime_importance = recode_policy_variables(crime2, reverse = TRUE),
    border_wall = recode_policy_variables(wall, reverse = TRUE),
    law_at_border = recode_policy_variables(law_at_border, reverse = TRUE),
    attend_march = recode_policy_variables(contestation1, reverse = TRUE),
    criticize_election = recode_policy_variables(contestation2, reverse = TRUE),
    burn_flag = recode_policy_variables(contestation3, reverse = TRUE),
    recount = recode_policy_variables(contestation4, reverse = TRUE),
    court = recode_policy_variables(contestation5, reverse = TRUE),
    certify = recode_policy_variables(contestation6, reverse = TRUE),
    concede = recode_policy_variables(contestation7, reverse = TRUE),
    state_certify = recode_policy_variables(contestation8, reverse = TRUE),
    violent = recode_policy_variables(contestation9, reverse = TRUE),
    new_election = recode_policy_variables(contestation10, reverse = TRUE),
    stolen_2020 = recode_policy_variables(steal_2020, reverse = TRUE),
    free_and_fair = recode_policy_variables(free_and_fair, reverse = TRUE),
    vote_confidence = recode_policy_variables(vote_confidence, reverse = TRUE),
    violence = recode_policy_variables(violence, reverse = TRUE),
    auth_1 = recode(as.numeric(auth1), `1` = 0, `2` = 1),
    auth_2 = recode(as.numeric(auth2), `1` = 0, `2` = 1),
    auth_3 = recode(as.numeric(auth3), `1` = 1, `2` = 0),
    auth_4 = recode(as.numeric(auth4), `1` = 0, `2` = 1),
    authoritarianism = zero.one(rowMeans(cbind(auth_1, auth_2, auth_3, auth_4), na.rm = TRUE)),
    rr1 = recode_policy_variables(rr1, reverse = FALSE),
    rr2 = recode_policy_variables(rr2, reverse = TRUE),
    rr3 = recode_policy_variables(rr3, reverse = TRUE),
    rr4 = recode_policy_variables(rr4, reverse = TRUE),
    racial_resentment = zero.one(rowMeans(cbind(rr1, rr2, rr3, rr4), na.rm = TRUE)),
    trust_media = case_when(
      as.numeric(news3) == 1 ~ "Always",
      as.numeric(news3) == 2 ~ "Most of the time",
      as.numeric(news3) == 3 ~ "Sometimes",
      as.numeric(news3) == 4 ~ "Almost never",
      TRUE ~ NA_character_
    ),
    political_interest = case_when(
      as.numeric(interest) == 1 ~ "Very interested",
      as.numeric(interest) == 2 ~ "Somewhat interested",
      as.numeric(interest) == 3 ~ "Not very interested",
      as.numeric(interest) == 4 ~ "Not at all interested",
      TRUE ~ NA_character_
    ),
    az_10years = ifelse(as.numeric(az_res) == 4, 1, 0),
    black = ifelse(as.numeric(race) == 2, 1, 0),
    white = ifelse(as.numeric(race) == 1, 1, 0),
    latino = ifelse(as.numeric(race) == 3, 1, 0),
    asian = ifelse(as.numeric(race) == 4, 1, 0),
    american_indian = ifelse(as.numeric(race) == 5, 1, 0),
    other = ifelse(as.numeric(race) == 6 | as.numeric(race) == 7, 1, 0),
    zipcode = as.numeric(inputzip),
    married = ifelse(as.numeric(marstat) == 1, 1, 0),
    female = ifelse(as.numeric(gender) == 2, 1, 0),
    college = ifelse(as.numeric(educ) >= 5, 1, 0),
    faminc = ifelse(as.numeric(faminc_new) > 8, 1, 0),
    kids_in_home = ifelse(as.numeric(child18) == 1, 1, 0),
    party_identification3 = case_when(
      as.numeric(pid7) == 1 ~ 1,
      as.numeric(pid7) == 2 ~ 1,
      as.numeric(pid7) == 3 ~ 2,
      as.numeric(pid7) == 4 ~ 2,
      as.numeric(pid7) == 5 ~ 2,
      as.numeric(pid7) == 6 ~ 3,
      as.numeric(pid7) == 7 ~ 3,
      TRUE ~ NA_real_
    ),
    pid7 = case_when(
      as.numeric(pid7) %in% 8:9 ~ NA_real_,
      TRUE ~ as.numeric(pid7)
    ),
    christian = case_when(
      as.numeric(religpew) %in% c(1, 2, 3, 4) ~ 1,
      as.numeric(religpew) %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 0,
      TRUE ~ NA_real_
    ),
    vote2016 = if_else(presvote16post == 2, 1, 0),
    county_fips = as.numeric(county_AZ),
    CD = as.numeric(cd),
    LD = as.numeric(LD_upper),
    ideology = case_when(
      as.numeric(ideo5) == 1 ~ 1,
      as.numeric(ideo5) == 2 ~ 2,
      as.numeric(ideo5) == 3 ~ 3,
      as.numeric(ideo5) == 4 ~ 4,
      as.numeric(ideo5) == 5 ~ 5,
      TRUE ~ NA_real_
    ),
    conservative3 = case_when(
      as.numeric(ideo5) == 1 ~ 1,
      as.numeric(ideo5) == 2 ~ 1,
      as.numeric(ideo5) == 3 ~ 2,
      as.numeric(ideo5) == 4 ~ 3,
      as.numeric(ideo5) == 5 ~ 3,
      TRUE ~ NA_real_
    ),
    survey_weight = weight
  )

# select all variables
all_columns <- names(df)
new_columns <- setdiff(all_columns, original_columns)

# Select only the newly created columns
clean_df <- df %>% select(all_of(new_columns))

# Save to dat
write.csv(clean_df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avp_wave_1_wide.csv", row.names = FALSE)


# w1_long <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_1_long.csv")

# w1_wide <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_1_wide.csv")

# wave1_names <- names(w1_wide)

# w2_long <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_2_long.csv")

# w2_wide <- read.csv(file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/avp_wave_2_wide.csv")

# wave2_names <- names(w2_wide)

# common_columns <- intersect(wave1_names, wave2_names)

# # Select only the newly created columns
# w1_wide <- w1_wide %>% select(all_of(common_columns))
# w2_wide <- w2_wide %>% select(all_of(common_columns))

# # sort columns
# w1_wide <- w1_wide[, sort(names(w1_wide))]
# w2_wide <- w2_wide[, sort(names(w2_wide))]


# #
