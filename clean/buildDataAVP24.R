# Build AVP Wave 2, 2022

library(haven)
library(tidyverse)
path <- file.path("/Users/Chris/Dropbox/masterData/groupAuthoritarianism/AZ2024/UARZ0004_OUTPUT.sav")
df <- read_sav(path)
original_columns <- names(df)

df <- df %>%
  mutate(
    caseid24 = caseid,
    caseid22 = UARZ0003_caseid,
    age_tr = ifelse(trump_biden_age_order_treat == 1, 1, 0), # Biden first
    birthage = as.numeric(2024 - birthyr),
    vote_ = recode(as.numeric(vote), `1` = 4, `2` = 3, `3` = 2, `4` = 1),
    vote_trump_2020 = recode(as.numeric(voteChoice), `1` = 0, `2` = 1),
    vote_type = recodeAVP(voteType, labels4, reverse = FALSE)$data,
    biden_age = recodeAVP(bidenAge, labels5, reverse = TRUE)$data,
    trump_age = recodeAVP(trumpAge, labels5, reverse = TRUE)$data,
    legal_trump = recodeAVP(legalTrump, labels5, reverse = TRUE)$data,
    jail_trump = recodeAVP(trumpjail, labels5, reverse = FALSE)$data,
    # Likelihood of conviction
    conviction_trump = recodeAVP(trumpCrime, labels4, reverse = TRUE)$data,
    # Vote against rating
    pro_biden_against_trump = recodeAVP(voteAgainst1a, labels5, reverse = TRUE)$data,
    pro_trump_against_biden = recodeAVP(voteAgainst1b, labels5, reverse = TRUE)$data,
    # On a scale of 0 to 100, where 0 represents the most liberal how would you rate your political ideology?
    ideo100 = ideo1 / 100,
    ideo_dem_voters = ideo2 / 100,
    ideo_rep_voters = ideo3 / 100,
    ideo_biden = ideo4 / 100,
    ideo_trump = ideo5 / 100,
    burn_flag = recode_policy_variables(contestation3, reverse = TRUE),
    recount = recode_policy_variables(contestation4, reverse = TRUE),
    court = recode_policy_variables(contestation5, reverse = TRUE),
    certify = recode_policy_variables(contestation6, reverse = TRUE),
    concede = recode_policy_variables(contestation7, reverse = TRUE),
    state_certify = recode_policy_variables(contestation8, reverse = TRUE),
    attend_march = recode_policy_variables(contestation1, reverse = TRUE),
    criticize_election = recode_policy_variables(contestation2, reverse = TRUE),
    biden_ft = Joe_Biden,
    trump_ft = Donald_Trump,
    lake_ft = Kari_Lake,
    gallego_ft = Ruben_Gallego,
    kelly_ft = Mark_Kelly,
    establishment_republican_feelings = EstabRep,
    maga_republican_feelings = MAGARep,
    establishment_democrat_feelings = EstabDem,
    progressive_democrat_feelings = ProgDem,
    republicans_ft = ft3_republican_party,
    democrats_ft = ft3_democratic_party,
    # Among Democrats, how do they feel most Republicans feel about the following:
    meta_democrat_liberal = meta1D_liberals,
    meta_democrat_conservatives = meta1D_conservatives,
    meta_democrat_republicans = meta1D_republicans,
    meta_democrat_democrats = meta1D_democrats,
    # Among Republicans,
    meta_republican_liberal = meta1R_Liberals,
    meta_republican_conservatives = meta1R_Conservatives,
    meta_republican_republicans = meta1R_Republicans,
    meta_republican_democrats = meta1R_Democrats,
    # Ideological placement
    meta_democrat_republican_ideo = meta2D_Republicans_g,
    meta_democrat_democrat_ideo = meta2D_Democrats_g,
    meta_republican_republican_ideo = meta2R_Republicans,
    meta_republican_democrat_ideo = meta2R_Democrats,
    mip = case_when(
      as.numeric(MIP) == 1 ~ "economy",
      as.numeric(MIP) == 2 ~ "climate/environment",
      as.numeric(MIP) == 3 ~ "climate/environment",
      as.numeric(MIP) == 4 ~ "healthcare",
      as.numeric(MIP) == 5 ~ "racism",
      as.numeric(MIP) == 6 ~ "terrorism",
      as.numeric(MIP) == 7 ~ "abortion",
      as.numeric(MIP) == 8 ~ "crime",
      as.numeric(MIP) == 9 ~ "poverty",
      as.numeric(MIP) == 10 ~ "national security",
      as.numeric(MIP) == 11 ~ "education",
      as.numeric(MIP) == 12 ~ "democracy",
      as.numeric(MIP) == 13 ~ "immigration",
      as.numeric(MIP) == 14 ~ "guns",
      as.numeric(MIP) == 15 ~ "foreignpolicy",
      as.numeric(MIP) == 16 ~ "other"
    ),
    biden_economy = ifelse(biden_economy == 1 | biden_economy == 2, 0, 1),
    # Immigration
    cali_migration = recodeAVP(Q49, labels5, reverse = TRUE)$data,
    immigration_rate = ifelse(immiga <= 2, 1, 0),
    separate_parents = recodeAVP(immig1, labels5, reverse = FALSE)$data,
    legal_status = recodeAVP(immig2, labels5, reverse = FALSE)$data,
    citizen = recodeAVP(immig3, labels5, reverse = FALSE)$data,
    smart_border = recodeAVP(immig4, labels5, reverse = FALSE)$data,
    abortion_legal = recode(as.numeric(abortion2), `1` = 1, `2` = 2, `3` = 3, `4` = 4),
    abortion_jail = recodeAVP(as.numeric(abortion1), labels5, reverse = TRUE)$data,
    border_wall = recodeAVP(wall, labels5, reverse = TRUE)$data,
    auth_1 = recode(as.numeric(auth1), `1` = 0, `2` = 1),
    auth_2 = recode(as.numeric(auth2), `1` = 0, `2` = 1),
    auth_3 = recode(as.numeric(auth3), `1` = 1, `2` = 0),
    auth_4 = recode(as.numeric(auth4), `1` = 0, `2` = 1),
    authoritarianism = zero.one(rowMeans(cbind(auth_1, auth_2, auth_3, auth_4), na.rm = TRUE)),
    abortion_rights = recodeAVP(abortion2b, labels5, reverse = FALSE)$data,
    
    
    transport_migrants = recodeAVP(Q162, labels5, reverse = FALSE)$data,
    law_at_border = recodeAVP(law_at_border, labels5, reverse = TRUE)$data,
    violence = recodeAVP(violence, labels4, reverse = TRUE)$data,
    rwa1 = recodeAVP(Q207, labels5, reverse = TRUE)$data,
    rwa2 = recodeAVP(Q208, labels5, reverse = TRUE)$data,
    rwa3 = recodeAVP(Q209, labels5, reverse = TRUE)$data,
    rwa4 = recodeAVP(Q210, labels5, reverse = TRUE)$data,
    rwa5 = recodeAVP(Q211, labels5, reverse = TRUE)$data,
    rwa6 = recodeAVP(Q212, labels5, reverse = TRUE)$data,
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
    black = ifelse(as.numeric(race) == 2, 1, 0),
    white = ifelse(as.numeric(race) == 1, 1, 0),
    latino = ifelse(as.numeric(race) == 3, 1, 0),
    asian = ifelse(as.numeric(race) == 4, 1, 0),
    american_indian = ifelse(as.numeric(race) == 5, 1, 0),
    two_or_more = ifelse(as.numeric(race) == 6, 1, 0),
    other = ifelse(as.numeric(race) == 7 | as.numeric(race) == 8, 1, 0),
    zipcode = as.numeric(inputzip),
    married = ifelse(as.numeric(marstat) == 1, 1, 0),
    college = ifelse(as.numeric(educ) >= 5, 1, 0),
    faminc = ifelse(as.numeric(faminc_new) > 8, 1, 0),
    kids_in_home = ifelse(as.numeric(child18) == 1, 1, 0),
    female = case_when(
      as.numeric(pid7) == 1 ~ 0,
      as.numeric(pid7) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
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
    vote2016 = if_else(presvote16post == 2, 1, 0),
    ideology = case_when(
      as.numeric(ideo5) == 1 ~ 1,
      as.numeric(ideo5) == 2 ~ 2,
      as.numeric(ideo5) == 3 ~ 3,
      as.numeric(ideo5) == 4 ~ 4,
      as.numeric(ideo5) == 5 ~ 5,
      TRUE ~ NA
    ),
    conservative3 = case_when(
      as.numeric(ideo5) == 1 ~ 1,
      as.numeric(ideo5) == 2 ~ 1,
      as.numeric(ideo5) == 3 ~ 2,
      as.numeric(ideo5) == 4 ~ 3,
      as.numeric(ideo5) == 5 ~ 3,
      TRUE ~ NA
    ),
    voter_registration = case_when(
      as.numeric(votereg) == 1 ~ "Yes",
      as.numeric(votereg) == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    christian = case_when(
      (religpew) %in% c(1, 2, 3, 4) ~ 1,
      as.numeric(religpew) %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 0,
      TRUE ~ NA_real_
    ),
    female = case_when(
      as.numeric(gender4) == 1 ~ 0,
      as.numeric(gender4) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    county_fips = as.numeric(county_AZ),
    CD = as.numeric(CD118),
    LD = as.numeric(LD_U),
    # Least Liked Group
    
    least_liked_group = case_when(
      g1 == 1 ~ "Socialists",
      g1 == 2 ~ "Marxists",
      g1 == 3 ~ "Fascists",
      g1 == 4 ~ "Communists",
      g1 == 5 ~ "Ku Klux Klan",
      g1 == 6 ~ "White Supremacists",
      g1 == 7 ~ "Christian Nationalists",
      g1 == 8 ~ "Atheists",
      g1 == 9 ~ "Pro-abortionists",
      g1 == 10 ~ "Anti-abortionist",
      g1 == 11 ~ "Undocumented immigrants",
      g1 == 12 ~ "MAGA supporters",
      g1 == 13 ~ "Muslims",
      g1 == 14 ~ "BLM supporters",
      g1 == 15 ~ "LGBT+ individuals",
      g1 == 16 ~ "Liberals",
      g1 == 17 ~ "Conservatives",
      g1 == 18 ~ "Democrats",
      g1 == 19 ~ "Non voters",
      g1 == 20 ~ "Republicans",
      g1 == 21 ~ "Other",
      TRUE ~ NA_character_
    ),
    second_least_liked_group = case_when(
      g2 == 1 ~ "Socialists",
      g2 == 2 ~ "Marxists",
      g2 == 3 ~ "Fascists",
      g2 == 4 ~ "Communists",
      g2 == 5 ~ "Ku Klux Klan",
      g2 == 6 ~ "White Supremacists",
      g2 == 7 ~ "Christian Nationalists",
      g2 == 8 ~ "Atheists",
      g2 == 9 ~ "Pro-abortionists",
      g2 == 10 ~ "Anti-abortionist",
      g2 == 11 ~ "Undocumented immigrants",
      g2 == 12 ~ "MAGA supporters",
      g2 == 13 ~ "Muslims",
      g2 == 14 ~ "BLM supporters",
      g2 == 15 ~ "LGBT+ individuals",
      g2 == 16 ~ "Liberals",
      g2 == 17 ~ "Conservatives",
      g2 == 18 ~ "Democrats",
      g2 == 19 ~ "Non voters",
      g2 == 20 ~ "Republicans",
      g2 == 21 ~ "Other",
      TRUE ~ NA_character_
    ),
    banned_from_president1 = recodeAVP(t7a, labels5, reverse = TRUE)$data,
    banned_from_teaching1 = recodeAVP(t8a, labels5, reverse = TRUE)$data,
    monitored_by_police1 = recodeAVP(t9a, labels5, reverse = TRUE)$data,
    banned_from_speech1 = recodeAVP(t1a, labels5, reverse = TRUE)$data,
    phones_surveilled1 = recodeAVP(t2a, labels5, reverse = TRUE)$data,
    banned_from_rallies1 = recodeAVP(t3a, labels5, reverse = TRUE)$data,
    banned_from_coaching1 = recodeAVP(t4a, labels5, reverse = TRUE)$data,
    banned_from_office1 = recodeAVP(t5a, labels5, reverse = TRUE)$data,
    banned_from_socialmedia1 = recodeAVP(t6a, labels5, reverse = TRUE)$data,
    banned_from_president2 = recodeAVP(t7b, labels5, reverse = TRUE)$data,
    banned_from_teaching2 = recodeAVP(t8b, labels5, reverse = TRUE)$data,
    monitored_by_police2 = recodeAVP(t9b, labels5, reverse = TRUE)$data,
    banned_from_speech2 = recodeAVP(t1b, labels5, reverse = TRUE)$data,
    phones_surveilled2 = recodeAVP(t2b, labels5, reverse = TRUE)$data,
    banned_from_rallies2 = recodeAVP(t3b, labels5, reverse = TRUE)$data,
    banned_from_coaching2 = recodeAVP(t4b, labels5, reverse = TRUE)$data,
    banned_from_office2 = recodeAVP(t5b, labels5, reverse = TRUE)$data,
    banned_from_socialmedia2 = recodeAVP(t6b, labels5, reverse = TRUE)$data,
    survey_weight = weight
  )
# select all variables
all_columns <- names(df)
new_columns <- setdiff(all_columns, original_columns)

# Select only the newly created columns
clean_df <- df %>% select(all_of(new_columns))

# Save to dat

write.csv(clean_df, file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avp_wave_2_wide.csv", row.names = FALSE)



# 
# 
# clean_df %>%
#   mutate(rid = seq(1:nrow(clean_df))) %>%
#   mutate_if(is.numeric, as.character) %>%
#   pivot_longer(cols = -c(caseid24, caseid22, survey_weight, county_fips, CD, LD, party_identification3)) %>%
#   mutate(county_name = recode(
#     county_fips,
#     `1` = "Apache",
#     `3` = "Cochise",
#     `5` = "Coconino",
#     `7` = "Gila",
#     `9` = "Graham",
#     `11` = "Greenlee",
#     `12` = "La Paz",
#     `13` = "Maricopa",
#     `15` = "Mohave",
#     `17` = "Navajo",
#     `19` = "Pima",
#     `21` = "Pinal",
#     `23` = "Santa Cruz",
#     `25` = "Yavapai",
#     `27` = "Yuma"
#   )) %>%
#   write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avpSurvey/avp_public_opinion/data/avp_wave_2_long.csv", row.names = FALSE)
# 
# 
