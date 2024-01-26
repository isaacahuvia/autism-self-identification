######################
####  Clean Data  ####
######################
# Clean and output qual + quant data for analysis


## Startup
library(tidyverse)
library(qualtRics)
`%+%` <- paste0


## Load data
# Raw data (see 1_Pull Raw Data.R)
raw_data <- readRDS("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualtrics Raw Data.rds")

# Coded qualitative data (WIP)


# Lookup table for manually recoding the variable age_first_aware
age_first_aware_recoding <- read.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Manual Recode Tables\\Age First Aware Recode.csv",
                                     stringsAsFactors = F)



####  Clean Data  ####
## Limit to those who consent
consenters <- raw_data %>%
  filter(consent %in% "I agree to participate in this study")


## Manually correct "problems" in speaking variables
# fetch_survey() expects a trailing space for some levels and therefore sets them
# as NA. Luckily, parse_factor() tells us when and where this happens so we can correct
for(x in c("speaking_child", "speaking_child_AAC", "speaking_now", "speaking_now_AAC")) {
  
  levels(raw_data[[x]]) <- trimws(levels(raw_data[[x]]))
  
  problems <- problems(raw_data[[x]])
  
  raw_data[[x]][problems$row] <- problems$actual
  
  print(x)
  print(levels(raw_data[[x]]))
  print(table(raw_data[[x]]))
  
}


## Manually correct fishy values
# 65425c72d94cab4a2b4caae4 listed their age as 5; set to NA
consenters$age[consenters$prolific_id == "65425c72d94cab4a2b4caae4"] <- NA


## Recode variables
recoded_data <- consenters %>%
  
  left_join(age_first_aware_recoding, by = "aware_when") %>%
  
  ## Recoding variables
  mutate(
    
    ### Self-identification
    ## Self-identification, current
    self_id = dx == "No, I have not received a formal diagnosis",
    
    ## Self-identification, ever
    self_id_ever = self_id | id_before_dx %in% "Yes",
    
    
    ### Demographic factors
    ## Age: Good as-is
    
    ## Gender
    gender = case_when(
      gender_3 == "Non-binary"
      | gender_4 == "Transgender"
      | gender_5 == "Genderqueer" 
      | gender_6 == "Genderfluid"
      | gender_7 == "Agender or no gender"
      | gender_8 == "Gender non-conforming"
      | gender_9 == "Two spirit"
      | gender_10 == "Questioning"
      | gender_12 == "A gender not listed" ~ "TGD",
      gender_1 == "Woman (including girl and young woman)" & sex == "Female" & trans == "No" ~ "Cis Female",
      gender_2 == "Man (including boy and young man)" & sex == "Male" & trans == "No" ~ "Cis Male",
      T ~ NA_character_
    ),
    
    ## Sexuality
    sexuality = case_when(
      sexuality_2 == "Gay, Lesbian, or Homosexual"
      | sexuality_3 == "Bisexual"
      | sexuality_4 == "Pansexual"
      | sexuality_5 == "Queer"
      | sexuality_6 == "Asexual/demisexual"
      | sexuality_7 == "Aromantic/demiromantic"
      | sexuality_8 == "I don't know/questioning"
      | sexuality_9 == "Prefer not to say"
      | sexuality_10 == "A sexual orientation not listed here" ~ "SM",
      sexuality_1 == "Straight or Heterosexual" ~ "Straight",
      T ~ NA_character_
    ),
    
    ## Race/ethnicity
    race_ethnicity = case_when(
      ethnicity == "Yes" ~ "Hispanic",
      (!is.na(race_1))
      + (!is.na(race_2)) 
      + (!is.na(race_3))
      + (!is.na(race_4))
      + (!is.na(race_5))
      + (!is.na(race_6))
      + (!is.na(race_7))
      + (!is.na(race_8)) > 1 ~ "Other or Multiracial non-Hispanic",
      race_1 == "White" ~ "White non-Hispanic",
      race_2 == "Black, African or African American" ~ "Black non-Hispanic",
      race_3 == "Native American or Alaska Native" ~ "NA/AN non-Hispanic",
      race_4 == "Asian" ~ "Asian non-Hispanic",
      race_5 == "Native Hawaiian or Other Pacific Islander" ~ "NH/PI non-Hispanic",
      race_6 == "Middle Eastern or North African" ~ "ME/NA non-Hispanic",
      race_7 == "Other race(s) not listed here" ~ "Other or Multiracial non-Hispanic",
      T ~ NA_character_
    ),
    
    ## Education: Good as-is
    
    ## Employment (NOTE: Differs from pre-registration; better information here)
    # Use of %in% treats missing values (NA) as FALSE. Appropriate for select boxes
    # left un-selected, since there are no participants who skipped all employment questions
    employment_employed = 
      employment_1 %in% "Employed for pay full-time (32 hours or more per week)"
    | employment_2 %in% "Employed for pay part-time (less than 32 hours per week)"
    | employment_3 %in% "Self-employed for pay",
    
    employment_unemployed = 
      employment_4 %in% "Unemployed and looking for paid work"
    | employment_5 %in% "Unemployed and not looking for paid work",
    
    employment_student = 
      employment_7 %in% "Student, full time"
    | employment_8 %in% "Student, part time",
    
    employment_retired = 
      employment_9 %in% "Retired",
    
    employment_homemaker = 
      employment_10 %in% "Stay at home parent or homemaker",
    
    employment_unable = 
      employment_13 %in% "Unable to work",
    
    
    ### Autistic traits
    ## CATI
    # Recode CATI
    across(
      starts_with("cati_"),
      ~ case_match(
        .,
        "Definitely disagree" ~ 1,
        "Somewhat disagree" ~ 2,
        "Neither agree nor disagree" ~ 3,
        "Somewhat agree" ~ 4,
        "Definitely agree" ~ 5
      )
    ),
    
    # Correct reverse-coded items
    across(
      all_of("cati_" %+% c(8, 15, 19, 23, 28)),
      ~ 6 - .
    ),
    
    ## RAADS-14
    # Recode RAADS-14
    across(
      starts_with("raads_"),
      ~ case_match(
        .,
        "Never true" ~ 0,
        "True only when I was younger than 16" ~ 1,
        "True only now" ~ 2,
        "True now and when I was younger than 16" ~ 3
      )
    ),
    
    # Correct reverse-coded item
    raads_6 = 3 - raads_6,
    
    ## Speech: Good as-is
    
    
    ### Psychosocial outcomes
    ## PHQ-4
    across(
      starts_with("phq_"),
      ~ case_match(
        .,
        "Not at All (0 - 1 days out of the last 14 days)" ~ 0,
        "Several Days (2 - 7 days out of the last 14 days)" ~ 1,
        "More Than Half the Days (8 - 12 days out of the last 14 days)" ~ 2,
        "Nearly Every Day (13 - 14 days out of the last 14 days)" ~ 3
      )
    ),
    
    ## Disability: Good as-is
    
    ## Autism as disability
    disability_autism = grepl("autism", disability_which, ignore.case = T),
    
    ## Impairment (WHODAS)
    across(
      starts_with("whodas_"),
      ~ case_match(
        .,
        "None" ~ 0,
        "Mild" ~ 1,
        "Moderate" ~ 2,
        "Severe" ~ 3,
        "Extreme/Cannot do" ~ 4
      )
    ),
    
    ## Wellbeing (WEMWBS)
    across(
      starts_with("wemwbs_"),
      ~ case_match(
        ., 
        "None of the time" ~ 1,
        "Rarely (about 1/4 or 25% of the time)" ~ 2,
        "Some of the time (about 1/2 or 50% of the time)" ~ 3,
        "Often (about 3/4 or 75% of the time)" ~ 4,
        "All of the time" ~ 5
      )
    ),
    
    ## Social identity (ASIS)
    across(
      starts_with("asis_"),
      ~ case_match(
        ., 
        "Strongly disagree" ~ 1,
        "Disagree" ~ 2,
        "Somewhat disagree" ~ 3,
        "Neither agree nor disagree" ~ 4,
        "Somewhat agree" ~ 5,
        "Agree" ~ 6,
        "Strongly agree" ~ 7
      )
    ),
    
    
    ### Support needs
    ## Current support needs
    support_current_mental_health = supports_1_1 %in% "I currently use this service",
    support_current_functional = supports_2_1 %in% "I currently use this service",
    support_current_case_management = supports_3_1 %in% "I currently use this service",
    support_current_social = supports_4_1 %in% "I currently use this service",
    support_current_vocational = supports_5_1 %in% "I currently use this service",
    support_current_health = supports_6_1 %in% "I currently use this service",
    
    ## Past support needs
    support_past_mental_health = supports_1_2 %in% "I have used this service in the past",
    support_past_functional = supports_2_2 %in% "I have used this service in the past",
    support_past_case_management = supports_3_2 %in% "I have used this service in the past",
    support_past_social = supports_4_2 %in% "I have used this service in the past",
    support_past_vocational = supports_5_2 %in% "I have used this service in the past",
    support_past_health = supports_6_2 %in% "I have used this service in the past",
    
    ## Unmet support needs
    support_need_mental_health = supports_1_3 %in% "I need more of this service",
    support_need_functional = supports_2_3 %in% "I need more of this service",
    support_need_case_management = supports_3_3 %in% "I need more of this service",
    support_need_social = supports_4_3 %in% "I need more of this service",
    support_need_vocational = supports_5_3 %in% "I need more of this service",
    support_need_health = supports_6_3 %in% "I need more of this service",
    
    ## Additional support needs
    other_support_current = other_supports_use,
    other_support_past = other_supports_used,
    other_support_need = other_supports_need,
    
    
    ### Beliefs about autism
    ## Autism definition: Good as-is (autism_def)
    
    ## Autism as disability: Good as-is (autism_dis)
    
    
    ### Other aspects
    ## Age first aware
    age_first_aware = aware_when_clean,
    
    ## Age first diagnosis
    age_first_dx = as.numeric(dx_when),
    
    ## Family/friends with diagnosis
    fam_dx = famfriend_dx_1 %in% "Yes, family (Optional: What is their relationship to you?)",
    friend_dx = famfriend_dx_2 %in% "Yes, friends",
    famfriend_dx = fam_dx | friend_dx,
    
    ## Family/friends who identify
    fam_id = famfriend_id_1 %in% "Yes, family (Optional: What is their relationship to you?)",
    friend_id = famfriend_id_2 %in% "Yes, friends",
    famfriend_id = fam_id | friend_id,
    
    
    ### Preferences regarding diagnosis
    ## Want a diagnosis: Ok as-is (nodx_want)
    
    ## Why do (or would) you want a diagnosis?
    
    ## Why don't (or wouldn't) you want a diagnosis?
    
    ## Perceived barriers to diagnosis
    
    
  ) %>%
  
  ## By row, calculate composite scores
  rowwise() %>%
  mutate(
    
    ## CATI
    # Social interactions
    cati_soc = sum(c_across(all_of("cati_" %+% c(8, 10, 15, 17, 28, 30, 35)))),
    
    # Communication
    cati_com = sum(c_across(all_of("cati_" %+% c(13, 19, 23, 26, 33, 37, 42)))),
    
    # Social camouflage
    cati_cam = sum(c_across(all_of("cati_" %+% c(3, 6, 9, 16, 22, 29, 39)))),
    
    # Cognitive rigidity
    cati_rig = sum(c_across(all_of("cati_" %+% c(2, 5, 14, 21, 27, 34, 38)))),
    
    # Repetitive behaviors
    cati_rep = sum(c_across(all_of("cati_" %+% c(1, 7, 12, 20, 25, 32, 41)))),
    
    # Sensory sensitivity
    cati_sen = sum(c_across(all_of("cati_" %+% c(4, 11, 18, 24, 31, 36, 40)))),
    
    # Sum
    cati_sum = sum(c_across(all_of("cati_" %+% 1:42))),
    
    ## RAADS-14
    # Sum score
    raads_sum = sum(c_across(starts_with("raads_"))),
    
    # Clinical cutoff
    raads_cutoff = raads_sum >= 14,
    
    
    ## PHQ-4
    # Anxiety
    phq_anx = sum(c_across(c(phq_1, phq_2))),
    
    # Depression
    phq_dep = sum(c_across(c(phq_3, phq_4))),
    
    
    ## WHODAS
    # Sum score
    whodas_sum = sum(c_across(starts_with("whodas_"))),
    
    
    ## WEMWBS
    # Sum score
    wemwbs_sum = sum(c_across(starts_with("wemwbs"))),
    
    
    ## ASIS (Autism *Social* Identity Scale, not Spectrum - a different measure)
    # Solidarity
    asis_solidarity = sum(c_across(all_of("asis_" %+% 1:3))),
    
    # Satisfaction
    asis_satisfaction = sum(c_across(all_of("asis_" %+% 4:7))),
    
    # Centrality
    asis_centrality = sum(c_across(all_of("asis_" %+% 8:9))),
    
    # Self-investment (overall)
    asis_self_investment = sum(c_across(all_of("asis_" %+% 1:9))),
    
    # Individual self-stereotyping
    asis_self_stereotyping = sum(c_across(all_of("asis_" %+% 10:11))),
    
    # In-group homogeneity
    asis_in_group_homogeneity = sum(c_across(all_of("asis_" %+% 12:13))),
    
    # Self-definition (overall)
    asis_self_definition = sum(c_across(all_of("asis_" %+% 10:13))),
    
    # ASIS sum score
    asis_sum = sum(c_across(starts_with("asis_"))),
    
    
    ### Support needs
    ## Current supports total
    support_current_total = sum(c_across(starts_with("support_current_"))),
    
    ## Past supports total
    support_past_total = sum(c_across(starts_with("support_past_"))),
    
    ## Unmet support needs total
    support_need_total = sum(c_across(starts_with("support_need_")))
    
  ) %>%
  ungroup()


## Select variables for analyses
selected_data <- recoded_data %>%
  select(response_id = ResponseId, 
         self_id, self_id_ever, age, starts_with("gender"), starts_with("sexuality"), 
         race_ethnicity, education, employment_employed:employment_unable, cati_soc, 
         cati_com, cati_cam, cati_rig, cati_rep, cati_sen, cati_sum, raads_sum, raads_cutoff, 
         speaking_now, speaking_child, phq_dep, phq_anx, disability, whodas_sum, 
         wemwbs_sum, asis_solidarity, asis_satisfaction, asis_centrality, asis_self_stereotyping, 
         asis_in_group_homogeneity, asis_self_investment, asis_self_definition, asis_sum, 
         starts_with("support_"), starts_with("autism_"),
         age_first_aware, age_first_dx, fam_dx, friend_dx, famfriend_dx, fam_id, friend_id, 
         famfriend_id, nodx_want, disability_autism)


## Save complete and pre-registered datasets
saveRDS(selected_data, "H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Analysis-Ready Data.rds")
