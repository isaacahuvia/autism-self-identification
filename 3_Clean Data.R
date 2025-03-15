######################
####  Clean Data  ####
######################

## Startup
library(tidyverse)
library(qualtRics)
library(openxlsx)
library(scales)
library(here)
`%+%` <- paste0


## Load data
# Raw data (see 1_Pull Raw Data.R)
raw_data <- readRDS("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualtrics Raw Data.rds")

# Approved prolific IDs (for filtering and deduplication)
approved_prolific_ids <- bind_rows(
  read.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Prolific Demographics\\prolific_export_654d2307ccdc612e1c3652de.csv"),
  read.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Prolific Demographics\\prolific_export_654d22ce099cb0a0fd753451.csv")
) %>%
  filter(Status == "APPROVED") %>%
  pull(Participant.id)

# Coded qualitative data
q1 <- read.xlsx("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Coded Qualitative Responses\\Q1.xlsx") %>%
  select(response_id, starts_with("q1"))
q2 <- read.xlsx("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Coded Qualitative Responses\\Q2.xlsx") %>%
  select(response_id, starts_with("q2"))
q3 <- read.xlsx("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Coded Qualitative Responses\\Q3.xlsx") %>%
  select(response_id, starts_with("q3"))
q4 <- read.xlsx("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Coded Qualitative Responses\\Q4.xlsx") %>%
  select(response_id, starts_with("q4"))
q5 <- read.xlsx("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Coded Qualitative Responses\\Q5.xlsx") %>%
  select(response_id, starts_with("q5"))

# Lookup table for manually recoding the variable age_first_aware
age_first_aware_recoding <- read.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Manual Recode Tables\\Age First Aware Recode.csv",
                                     stringsAsFactors = F)



####  Clean Data  ####
## Filter responses
# Check rows in raw data
nrow(raw_data)

# Filter to those who passed both screeners
screened_respondents <- raw_data %>%
  filter(
    prolific_screener %in% c(
      "Yes - as a child",
      "Yes - as an adult",
      "No - but I identify as being on the autism spectrum",
      "I am in the process of receiving a diagnosis"
    ),
    identity_screener %in% c(
      "Yes, I identify as autistic or on the autism spectrum",
      "I have a different identity, diagnosis, or condition on the autism spectrum"
    )
  )

nrow(screened_respondents)

# Filter to those who provided consent
consenting_respondents <- screened_respondents %>%
  filter(consent %in% "I agree to participate in this study")

nrow(consenting_respondents)

# Filter to approved (i.e., paid) participant IDs
approved_respondents <- consenting_respondents %>%
  filter(prolific_id %in% approved_prolific_ids)

nrow(approved_respondents)

# Filter to non-duplicated responses
unique_respondents <- approved_respondents %>%
  group_by(prolific_id) %>%
  arrange(desc(Finished)) %>%
  slice_head(n = 1) %>%
  ungroup()

nrow(unique_respondents)

# Filter to high-attention responses
high_attention_respondents <- unique_respondents %>%
  mutate(
    age_implied_by_dob = 2023 - dob,
    age_difference = abs(age - age_implied_by_dob)
  ) %>%
  filter(age_difference <= 1)

nrow(high_attention_respondents)


## Recode variables
# Manually correct "problems" in speaking variables
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

# Recode remaining variables
recoded_data <- high_attention_respondents %>%
  
  left_join(age_first_aware_recoding, by = "aware_when") %>%
  
  ## Recoding variables
  mutate(
    
    ### Self-identification
    ## Self-identification, current
    self_id = dx == "No, I have not received a formal diagnosis",
    self_id_char = case_match(
      self_id, 
      T ~ "Self-Identifying",
      F ~ "Diagnosed"
    ),
    
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
    
    across(
      all_of("gender_" %+% 1:12),
      ~ case_when(
        !is.na(.) ~ .,
        is.na(.) & !is.na(gender) ~ "Not Selected",
        is.na(.) & is.na(gender) ~ NA_character_
      )
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
    
    across(
      all_of("sexuality_" %+% 1:10),
      ~ case_when(
        !is.na(.) ~ .,
        is.na(.) & !is.na(sexuality) ~ "Not Selected",
        is.na(.) & is.na(sexuality) ~ NA_character_
      )
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
    
    race_ethnicity_bin = race_ethnicity == "White non-Hispanic",
    
    ## Education
    education = as.character(education),
    
    education_ord = case_match(
      education,
      c("Less than a high school diploma", "High school degree or equivalent (e.g., GED, modified diploma, certificate of completion)") ~ 1,
      "Some college" ~ 2,
      "Associate degree (e.g., AA, AS)" ~ 3,
      "Bachelor’s degree (e.g., BA, BS)" ~ 4,
      c("Master’s degree (e.g., MA, MS)", "Professional or doctorate degree (e.g., MD, DDS, DVM, PhD, EdD, PsyD)") ~ 5,
       "Other:" ~ NA_real_
    ),
    
    
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
    
    ## Disability
    disability = if_else(
      disability %in% c("Yes", "No", "Unsure"),
      as.character(disability),
      NA_character_
    ),
    
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
    
    ## Autism as disability
    autism_dis = as.character(autism_dis),
    
    ### Other aspects
    ## Age first aware
    age_first_aware = aware_when_clean,
    
    ## Age first diagnosis
    age_first_dx = as.numeric(dx_when), # One coerced NA value (that's ok - this wasn't an age)
    
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
         self_id, self_id_char, self_id_ever, age, starts_with("gender"), starts_with("sexuality"), 
         race_ethnicity, race_ethnicity_bin, education, education_ord, employment_employed:employment_unable, 
         cati_soc, cati_com, cati_cam, cati_rig, cati_rep, cati_sen, cati_sum, raads_sum, raads_cutoff, 
         speaking_now, speaking_child, phq_dep, phq_anx, disability, whodas_sum, 
         wemwbs_sum, asis_solidarity, asis_satisfaction, asis_centrality, asis_self_stereotyping, 
         asis_in_group_homogeneity, asis_self_investment, asis_self_definition, asis_sum, 
         starts_with("support_"), starts_with("autism_"),
         age_first_aware, age_first_dx, fam_dx, friend_dx, famfriend_dx, fam_id, friend_id, 
         famfriend_id, nodx_want, access, understand)


## Merge in qualitative data
# ID Overlap
id_overlap <- function(x, y) {
  
  if(any(duplicated(x))) stop("x should not have duplicates")
  if(any(duplicated(y))) stop("y should not have duplicates")
  
  if(any(is.na(x))) stop("x should not have NAs")
  if(any(is.na(y))) stop("y should not have NAs")
  
  x_in_y <- mean(x %in% y)
  y_in_x <- mean(y %in% x)
  
  print(percent(x_in_y) %+% " of IDs of x in y")
  print(percent(y_in_x) %+% " of IDs of y in x")
  
}

id_overlap(selected_data$response_id, q1$response_id)
id_overlap(selected_data$response_id, q2$response_id)
id_overlap(selected_data$response_id, q3$response_id)
id_overlap(selected_data$response_id, q4$response_id)
id_overlap(selected_data$response_id, q5$response_id)

merged_data <- selected_data %>%
  left_join(q1, by = "response_id", relationship = "one-to-one") %>%
  left_join(q2, by = "response_id", relationship = "one-to-one") %>%
  left_join(q3, by = "response_id", relationship = "one-to-one") %>%
  left_join(q4, by = "response_id", relationship = "one-to-one") %>%
  left_join(q5, by = "response_id", relationship = "one-to-one")
  

## Save complete and pre-registered dataset
saveRDS(merged_data, "H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Analysis-Ready Data.rds")
