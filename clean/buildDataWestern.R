rm(list = ls())

library(tidyr)
library(dplyr)
library(readstata13)
library(labelled)

# ---- Load Data ----
df <- read.dta13(
  "/Users/Chris/Dropbox/masterData/azDat/western/data/data.dta",
  missing.type = TRUE,
  generate.factors = TRUE
)
setwd("/Users/Chris/Dropbox/github_repos/avp-survey-data/public_opinion/R")
source(file = "recode.R")

original_columns <- names(df)

df <- df %>%
  mutate(
    id = 1:nrow(df),
    # Experimental Manipulations
    az_target_tr = recode(as.numeric(WSS28_split), `1` = "people", `2` = "officials"),
    violent_protest_tr = recode(as.numeric(WSS40_1_split), `1` = 1, `2` = 0),
    social_media_tr = recode(as.numeric(WSS40_5_split), `1` = 1, `2` = 0),
    efficacy_tr = recode(as.numeric(WSS33_split), `1` = 1, `2` = 0),
    state_residence = as.character(inputstate),
    state_insult = recodeAVP(WSS02_1, labels5, reverse = TRUE)$data,
    state_common = recodeAVP(WSS02_2, labels5, reverse = TRUE)$data,
    state_interest = recodeAVP(WSS02_3, labels5, reverse = TRUE)$data,
    state_we = recodeAVP(WSS02_4, labels5, reverse = TRUE)$data,
    state_partme = recodeAVP(WSS02_5, labels5, reverse = TRUE)$data,
    state_proud = recodeAVP(WSS02_6, labels5, reverse = TRUE)$data,
    moral_ind1 = recodeAVP(WSS09_b_1, labels5, reverse = TRUE)$data,
    moral_ind2 = recodeAVP(WSS09_b_2, labels5, reverse = TRUE)$data,
    moral_ind3 = recodeAVP(WSS09_b_3, labels5, reverse = TRUE)$data,
    moral_ind4 = recodeAVP(WSS09_b_4, labels5, reverse = TRUE)$data,
    moral_individualism = zero.one(rowMeans(cbind(moral_ind1, moral_ind2, moral_ind3, moral_ind4), na.rm = TRUE)), # 0.81
    sdo1 = recodeAVP(WSS10_1, labels5, reverse = TRUE)$data,
    sdo2r = recodeAVP(WSS10_2, labels5, reverse = FALSE)$data,
    sdo3 = recodeAVP(WSS10_3, labels5, reverse = TRUE)$data,
    sdo4r = recodeAVP(WSS10_4, labels5, reverse = FALSE)$data,
    moral_individualism = zero.one(rowMeans(cbind(sdo1, sdo2r, sdo3, sdo4r), na.rm = TRUE)), # 0.59
    rr1 = recodeAVP(WSS11_1, labels5, reverse = TRUE)$data,
    rr2r = recodeAVP(WSS11_2, labels5, reverse = FALSE)$data,
    rr3r = recodeAVP(WSS11_3, labels5, reverse = FALSE)$data,
    rr4 = recodeAVP(WSS11_4, labels5, reverse = TRUE)$data,
    racial_resentment = zero.one(rowMeans(cbind(rr1, rr2r, rr3r, rr4), na.rm = TRUE)), # 0.89
    emp1 = recodeAVP(WSS12_1, labels5, reverse = TRUE)$data,
    emp2 = recodeAVP(WSS12_2, labels5, reverse = TRUE)$data,
    emp3 = recodeAVP(WSS12_3, labels5, reverse = TRUE)$data,
    emp4 = recodeAVP(WSS12_4, labels5, reverse = TRUE)$data,
    empathy = zero.one(rowMeans(cbind(emp1, emp2, emp3, emp4), na.rm = TRUE)), # 0.87
    anger = recodeAVP(WSS14_2, labels5, reverse = FALSE)$data,
    anxiety = recodeAVP(WSS14_1, labels5, reverse = FALSE)$data,
    hope = recodeAVP(WSS14_3, labels5, reverse = FALSE)$data,
    pride = recodeAVP(WSS14_4, labels5, reverse = FALSE)$data,
    disgust = recodeAVP(WSS14_5, labels5, reverse = FALSE)$data,
    enthusiasm = recodeAVP(WSS14_6, labels5, reverse = FALSE)$data,
    urban_r1 = recodeAVP(WSS31_1, labels5, reverse = TRUE)$data,
    urban_r2 = recodeAVP(WSS31_2, labels5, reverse = TRUE)$data,
    urban_r3 = recodeAVP(WSS31_3, labels5, reverse = TRUE)$data,
    urban_r4 = recodeAVP(WSS31_4, labels5, reverse = TRUE)$data,
    urban_resentment = zero.one(rowMeans(cbind(urban_r1, urban_r2, urban_r3, urban_r4), na.rm = TRUE)), # 0.72
    violent = recodeAVP(WSS40_1, labels5, reverse = TRUE)$data,
    burn = recodeAVP(WSS40_2, labels5, reverse = TRUE)$data,
    court = recodeAVP(WSS40_3, labels5, reverse = TRUE)$data,
    recount = recodeAVP(WSS40_4, labels5, reverse = TRUE)$data,
    criticize = recodeAVP(WSS40_5, labels5, reverse = TRUE)$data,
    trustPres = recodeAVP(WSS54a_1, labels4, reverse = TRUE)$data,
    trustCongress = recodeAVP(WSS54a_2, labels4, reverse = TRUE)$data,
    trustMyGov = recodeAVP(WSS54a_4, labels4, reverse = TRUE)$data,
    trustMyLeg = recodeAVP(WSS54a_5, labels4, reverse = TRUE)$data,
    trustPolice = recodeAVP(WSS54a_6, labels4, reverse = TRUE)$data,
    trustScience = recodeAVP(WSS54a_7, labels4, reverse = TRUE)$data,
    waterFish = recodeAVP(WSS19_1, labels4, reverse = TRUE)$data,
    envHabitat = recodeAVP(WSS19_2, labels4, reverse = TRUE)$data,
    envWater = recodeAVP(WSS19_3, labels4, reverse = TRUE)$data,
    envPollution = recodeAVP(WSS19_4, labels4, reverse = TRUE)$data,
    envTemp = recodeAVP(WSS19_5, labels4, reverse = TRUE)$data,
    waterFish = recodeAVP(WSS19_1, labels4, reverse = TRUE)$data,
    waterDams = recodeAVP(WSS20_1, labels5, reverse = TRUE)$data,
    waterConserv = recodeAVP(WSS20_2, labels5, reverse = TRUE)$data,
    publicConservation = recodeAVP(WSS22, labels5, reverse = TRUE)$data,
    immUndocumented = recodeAVP(WSS23_1, labels5, reverse = FALSE)$data,
    immSeparation = recodeAVP(WSS23_2, labels5, reverse = FALSE)$data,
    immAsylum = recodeAVP(WSS23_3, labels5, reverse = TRUE)$data,
    immBirthright = recodeAVP(WSS23_4, labels5, reverse = TRUE)$data,
    immTech = recodeAVP(WSS23_5, labels5, reverse = TRUE)$data,
    immWall = recodeAVP(WSS23_6, labels5, reverse = TRUE)$data,
    immResources = recodeAVP(WSS23_7, labels5, reverse = FALSE)$data,
    concealCarry = recodeAVP(WSS27_1, labels5, reverse = TRUE)$data,
    background_guns = recodeAVP(WSS27_2, labels5, reverse = FALSE)$data,
    registry_guns = recodeAVP(WSS27_3, labels5, reverse = FALSE)$data,
    eff1 = recodeAVP(WSS33_1, labels5, reverse = FALSE)$data,
    eff2 = recodeAVP(WSS33_2, labels5, reverse = FALSE)$data,
    censusAccurate = recodeAVP(WSS35_b, labels4, reverse = TRUE)$data,
    electionMail = recodeAVP(WSS41_1, labels5, reverse = FALSE)$data,
    electionCovid = recodeAVP(WSS41_2, labels5, reverse = TRUE)$data,
    electionLines = recodeAVP(WSS41_3, labels5, reverse = TRUE)$data,
    electionIntimidate = recodeAVP(WSS41_4, labels5, reverse = TRUE)$data,
    electionConcede = recodeAVP(WSS41_5, labels5, reverse = TRUE)$data,
    electionFraud = recodeAVP(WSS41_6, labels5, reverse = TRUE)$data,
    legCourt = recodeAVP(WSS49_1, labels5, reverse = TRUE)$data,
    legPolitics = recodeAVP(WSS49_2, labels5, reverse = TRUE)$data,
    legPower = recodeAVP(WSS49_3, labels5, reverse = TRUE)$data,
    trustLeg = recodeAVP(WSS54_1, labels4, reverse = TRUE)$data,
    trustPres = recodeAVP(WSS54_2, labels4, reverse = FALSE)$data,
    trustSC = recodeAVP(WSS54_3, labels4, reverse = FALSE)$data,
    trustGov = recodeAVP(WSS54_4, labels4, reverse = TRUE)$data,
    trustStateleg = recodeAVP(WSS54_5, labels4, reverse = FALSE)$data,
    trustPolice = recodeAVP(WSS54_6, labels4, reverse = FALSE)$data,
    myLeg1 = recodeAVP(WSS57_1, labels5, reverse = TRUE)$data,
    myLeg2 = recodeAVP(WSS57_2, labels5, reverse = TRUE)$data,
    myLeg3 = recodeAVP(WSS57_3, labels5, reverse = TRUE)$data,
    myLeg4 = recodeAVP(WSS57_4, labels5, reverse = TRUE)$data,
    gunHome = recode(as.numeric(WSS26), `1` = 1, `2` = 0),
    polMeeting = recode(as.numeric(WSS32_1), `1` = 1, `2` = 0),
    polSign = recode(as.numeric(WSS32_2), `1` = 1, `2` = 0),
    polVolunteer = recode(as.numeric(WSS32_3), `1` = 1, `2` = 0),
    polProtest = recode(as.numeric(WSS32_4), `1` = 1, `2` = 0),
    poLOfficial = recode(as.numeric(WSS32_5), `1` = 1, `2` = 0),
    polDonate = recode(as.numeric(WSS32_6), `1` = 1, `2` = 0),
    polSocial = recode(as.numeric(WSS32_7), `1` = 1, `2` = 0),
    polPersuade = recode(as.numeric(WSS32_8), `1` = 1, `2` = 0),
    polNone = recode(as.numeric(WSS32_9), `1` = 1, `2` = 0),
    buycott1 = recode(as.numeric(WSS34_a), `1` = 1, `2` = 0),
    buycott2 = recode(as.numeric(WSS34_b), `1` = 1, `2` = 0),
    ballotType1 = case_when(
      as.numeric(WSS36_e) == 1 ~ "In Person",
      as.numeric(WSS36_e) == 2 ~ "Mail",
      as.numeric(WSS36_e) == 3 ~ "Drop Off",
      as.numeric(WSS36_e) == 4 ~ "Absentee",
      # Missing
      TRUE ~ NA
    ),
    ballotType2 = case_when(
      as.numeric(WSS36_d) == 1 ~ "In Person",
      as.numeric(WSS36_d) == 2 ~ "Mail",
      as.numeric(WSS36_d) == 3 ~ "Drop Off",
      # Missing
      TRUE ~ NA
    ),
    ballotType = ifelse(is.na(ballotType2), ballotType1, ballotType2),
    completeCensus = case_when(
      as.numeric(WSS35) == 1 ~ "Yes",
      as.numeric(WSS35) == 2 ~ "Yes-Someone else",
      as.numeric(WSS35) == 3 ~ "No",
      as.numeric(WSS35) == 4 ~ "I don't recall",
      # Missing
      TRUE ~ NA
    ),
    publicConservation = case_when(
      as.numeric(WSS22) == 1 ~ "extraction",
      as.numeric(WSS22) == 2 ~ "renewable",
      as.numeric(WSS22) == 3 ~ "recreation",
      as.numeric(WSS22) == 4 ~ "protection",
      as.numeric(WSS22) == 5 ~ "skipped",
      # Missing
      TRUE ~ NA
    ),
    mip = case_when(
      as.numeric(WSS15s) == 1 ~ "environment",
      as.numeric(WSS15s) == 2 ~ "police",
      as.numeric(WSS15s) == 3 ~ "economy",
      as.numeric(WSS15s) == 4 ~ "housing",
      as.numeric(WSS15s) == 5 ~ "immigration",
      as.numeric(WSS15s) == 6 ~ "trade/foreign policy",
      as.numeric(WSS15s) == 7 ~ "trade/foreign policy",
      as.numeric(WSS15s) == 8 ~ "education",
      as.numeric(WSS15s) == 9 ~ "healthcare",
      as.numeric(WSS15s) == 10 ~ "covid",
      as.numeric(WSS15s) == 11 ~ "other",
      # Missing
      TRUE ~ NA
    ),
    survey_weight = weight,
    age = 2020 - birthyr,
    vote_trump_pre = recode(as.numeric(WSS36_b), `1` = 1, `2` = 0),
    vote_trump_post = recode(as.numeric(WSS36_c), `1` = 1, `2` = 0),
    vote_trump = ifelse(is.na(WSS36_b), vote_trump_post, vote_trump_pre),
    gen = case_when(
      age > 17 & age < 40 ~ "Millenial",
      age > 39 & age < 60 ~ "Gen X",
      age > 59 & age < 80 ~ "Boomer",
      age > 79 & age < 100 ~ "Greatest",
      TRUE ~ NA_character_
    ),
    female = if_else(gender == 2, 1, 0),
    white = if_else(race == "White", 1, 0),
    latino = if_else(race == "Hispanic", 1, 0),
    black = if_else(race == "Black", 1, 0),
    asian = if_else(race == "Asian", 1, 0),
    native = if_else(race == "Native American", 1, 0),
    twoplus = if_else(race == "Two or more races", 1, 0),
    other = if_else(race %in% c("Other", "Middle Eastern"), 1, 0),
    vote2016 = if_else(presvote16post == 2, 1, 0),
    college = if_else(educ == "Post-grad" | educ == "4-year", 1, 0),
    income = case_when(
      faminc_new %in% 9:16 ~ 1,
      TRUE ~ 0
    ), # 85k +,
    married = if_else(marstat == 1, 1, 0),
    ideology = case_when(
      as.numeric(ideo5) == 1 ~ 1,
      as.numeric(ideo5) == 2 ~ 2,
      as.numeric(ideo5) == 3 ~ 3,
      as.numeric(ideo5) == 4 ~ 4,
      as.numeric(ideo5) == 5 ~ 5,
      TRUE ~ NA_real_
    ),
    christian = case_when(
      as.numeric(religpew) %in% c(1, 2, 3, 4) ~ 1,
      as.numeric(religpew) %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 0,
      TRUE ~ NA_real_
    ),
    interest = case_when(
      newsint == "Most of the time" ~ 4,
      newsint == "Some of time" ~ 3,
      newsint == "Now and then" ~ 2,
      newsint == "Hardly at all" ~ 1,
      TRUE ~ NA_real_
    ),
    auth1 = case_when(
      as.numeric(WSS07_1a) == 1 ~ 0,
      as.numeric(WSS07_1a) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    auth2 = case_when(
      as.numeric(WSS07_1b) == 1 ~ 1,
      as.numeric(WSS07_1b) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    auth3 = case_when(
      as.numeric(WSS07_1c) == 1 ~ 0,
      as.numeric(WSS07_1c) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    auth4 = case_when(
      as.numeric(WSS07_1d) == 1 ~ 0,
      as.numeric(WSS07_1d) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    az_rep_state = as.numeric(as.character(WSS28_1)),
    az_rep_nat = as.numeric(as.character(WSS28_2)),
    az_dem_state = as.numeric(as.character(WSS28_3)),
    az_dem_national = as.numeric(as.character(WSS28_4)),
    party3 = case_when(
      as.numeric(pid7) == 1 ~ 1,
      as.numeric(pid7) == 2 ~ 1,
      as.numeric(pid7) == 3 ~ 2,
      as.numeric(pid7) == 4 ~ 2,
      as.numeric(pid7) == 5 ~ 3,
      as.numeric(pid7) == 6 ~ 3,
      as.numeric(pid7) == 7 ~ 3,
      TRUE ~ NA_real_
    ),
    pid7 = case_when(
      as.numeric(pid7) %in% 8:9 ~ NA_real_,
      TRUE ~ as.numeric(pid7)
    )
  )

new_columns <- names(df)
new_columns <- setdiff(new_columns, original_columns)
clean_df <- df %>% select(all_of(new_columns))

#---- Save Data -----#
write_csv(clean_df, file = "~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/datWestern2020_wide.csv")

clean_df %>%
  mutate(rid = seq(1:nrow(clean_df))) %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(cols = -c(rid, survey_weight, party3)) %>%
  # save unique values in name
  write.csv("~/Dropbox/github_repos/avp-survey-data/public_opinion/dat/datWestern2020_long.csv", row.names = FALSE)
