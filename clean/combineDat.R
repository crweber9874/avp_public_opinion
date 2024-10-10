## Arizona Voter Survey, Wave 1, 2022

read.csv("~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avp_wave_1_wide.csv") %>%
  select(biden_ft, trump_ft, lake_ft,
         kelly_ft,
         establishment_republican_feelings, 
         maga_republican_feelings, establishment_democrat_feelings, 
         progressive_democrat_feelings, 
         cali_migration, immigration_rate, separate_parents,
         legal_status, citizen, smart_border,
         water_supply,
         limit_water, tax_water, reduce_water,
         background_guns, registry_guns, age_guns,
         assault_guns, abortion_legal, abortion_jail,
         border_wall, 
         attend_march, criticize_election,
         auth_1, auth_2, auth_3,
         auth_4, authoritarianism, party_identification3,
         conservative3, black, white,
         hispanic, asian, american_indian,
         other, zipcode, married,
         female, college, faminc,
         kids_in_home, survey_weight, county_fips,
         LD, CD, caseid22)  %>%
  mutate(survey = "AVP1")->  avp_w1_wide

avp_w1_wide %>%
  mutate(across(-c(caseid22, survey, survey_weight, county_fips, CD, conservative3, party_identification3), as.character)) %>%
  pivot_longer(cols = -c(survey, caseid22, survey_weight, county_fips, CD, conservative3, party_identification3)) %>%
  mutate(county_name = recode(
    county_fips,
    `1` = "Apache",
    `3` = "Cochise",
    `5` = "Coconino",
    `7` = "Gila",
    `9` = "Graham",
    `11` = "Greenlee",
    `12` = "La Paz",
    `13` = "Maricopa",
    `15` = "Mohave",
    `17` = "Navajo",
    `19` = "Pima",
    `21` = "Pinal",
    `23` = "Santa Cruz",
    `25` = "Yavapai",
    `27` = "Yuma"
  ))  -> avp_w1_long   



## Arizona Voter Survey, Wave 2, 2024

clean_df = read.csv( "~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avp_wave_2_wide.csv")
clean_df %>%
  select(conservative3, party_identification3, burn_flag,
         recount, court, certify,
         concede, state_certify, attend_march,
         criticize_election, biden_ft, trump_ft,
         lake_ft, kelly_ft,
         establishment_republican_feelings, maga_republican_feelings, establishment_democrat_feelings,
         progressive_democrat_feelings, republicans_ft, democrats_ft,
         cali_migration, immigration_rate, separate_parents,
         legal_status, citizen, smart_border,
         abortion_legal, abortion_jail, border_wall,
         auth_1, auth_2, auth_3,
         auth_4, authoritarianism, abortion_rights, 
         black, white, latino,
         asian, american_indian, two_or_more,
         other, zipcode, married,
         college, faminc, kids_in_home,
         female, caseid24, caseid22, survey_weight, 
         county_fips,
         LD, CD) %>% 
  mutate(survey = "AVP2")  ->  avp_w2_wide

avp_w2_wide%>% 
  mutate(across(-c(caseid22, survey_weight, county_fips, CD, conservative3, party_identification3), as.character)) %>%
  pivot_longer(cols = -c(survey, caseid24, caseid22, survey_weight, 
                         county_fips, CD, conservative3, 
                         party_identification3))  %>%
  mutate(county_name = recode(
    county_fips,
    `1` = "Apache",
    `3` = "Cochise",
    `5` = "Coconino",
    `7` = "Gila",
    `9` = "Graham",
    `11` = "Greenlee",
    `12` = "La Paz",
    `13` = "Maricopa",
    `15` = "Mohave",
    `17` = "Navajo",
    `19` = "Pima",
    `21` = "Pinal",
    `23` = "Santa Cruz",
    `25` = "Yavapai",
    `27` = "Yuma"
  )) ->  avp_w2_long

## Construct the W1 and W2

# Rename columns in avp_w1_wide to end with _w1
df1 <- avp_w1_wide %>%
  rename_with(~ paste0(., "_w1"), -caseid22)

# Rename columns in avp_w2_wide to end with _w2
df2 <- avp_w2_wide %>%
  rename_with(~ paste0(., "_w2"), -caseid22)

# Perform the right join
avp_w1w2 <- right_join(df1, df2, by = "caseid22")

## Western States Survey, 2020

read.csv("~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/datWestern2020_wide.csv") %>%
  select( id, conservative3, party_identification3,
          attend_march, burn_flag, court,
          recount, criticize_election,
          water_dams, water_conservation,
          legal_status, separate_parents, immAsylum,
          citizen, smart_border, border_wall,
          immResources, conceal_guns, background_guns,
          registry_guns,
          female, white, latino,
          hispanic, asian, american_indian,
          two_or_more, other, vote2016,
          college, income, married,
          survey_weight)  %>% 
  mutate(survey = "Western")  ->  western_wide

western_wide %>% 
  mutate(across(-c(id, survey, survey_weight, conservative3, party_identification3), as.character)) %>%
  pivot_longer(cols = -c(id, survey, survey_weight, conservative3, party_identification3))  -> western_long


## Combine the data

list(western_long, avp_w1_long, avp_w2_long) %>%
  bind_rows() %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avpLong.csv", row.names = FALSE)

list(western_wide, avp_w1_wide, avp_w2_wide) %>%
  bind_rows() %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avpWide.csv", row.names = FALSE)

list(western_wide, avp_w1_wide, avp_w2_wide) %>%
  bind_rows() %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avpWide.csv", row.names = FALSE)

avp_w1w2 %>%
  write.csv(file = "~/Dropbox/github_repos/avp-survey-data/avp_public_opinion/data/avpPanel.csv", row.names = FALSE)


