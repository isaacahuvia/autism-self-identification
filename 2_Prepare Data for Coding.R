####  Prepare Data for Coding  ####
# Clean and output qualitative data for manual coding

## Startup
library(tidyverse)
set.seed(8472087)


## Load data
# Raw data (see 1_Pull Raw Data.R)
raw_data <- readRDS("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualtrics Raw Data.rds")


## Clean data
# Limit to those who consent
consenters <- raw_data %>%
  filter(consent %in% "I agree to participate in this study")

scrambled_consenters <- consenters %>%
  mutate(runif = runif(n = nrow(.))) %>%
  arrange(runif) %>%
  select(-runif)

# Clean open-ended response variables (to be coded)
qualitative_data <- scrambled_consenters %>%
  mutate(
    
    group = if_else(dx == "No, I have not received a formal diagnosis", "Self-ID", "Diagnosed"),
    
    want_why = case_when(
      nodx_want == "Yes" ~ want_yes_why,
      nodx_want == "No" ~ want_no_why,
      nodx_want == "Unsure" ~ want_unsure_why,
      T ~ NA_character_
    ),
    
    want_whynot = case_when(
      nodx_want == "Yes" ~ want_yes_whynot,
      nodx_want == "No" ~ want_no_whynot,
      nodx_want == "Unsure" ~ want_unsure_whynot,
      T ~ NA_character_
    )
    
  ) %>%
  select(response_id = ResponseId,
         group, 
         want_dx = nodx_want, 
         why_want_dx = want_why, 
         why_dont_want_dx = want_whynot, 
         barriers_to_dx = want_yes_barrier, 
         barriers_to_support = supports_barriers, 
         autism_def, 
         how_first_aware = aware_how)


## Save data
# All responses
qualitative_data %>%
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\All Responses.csv")

# Q1 (why want dx) only
qualitative_data %>%
  select(response_id, group, want_dx, why_want_dx) %>%
  drop_na(why_want_dx) %>%
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q1 Responses.csv")

# Q2 (why don't want dx) only
qualitative_data %>%
  select(response_id, group, want_dx, why_dont_want_dx) %>%
  drop_na(why_dont_want_dx) %>%  
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q2 Responses.csv")

# Q3 (barriers to dx) only
qualitative_data %>%
  select(response_id, group, want_dx, barriers_to_dx) %>%
  drop_na(barriers_to_dx) %>%  
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q3 Responses.csv")

# Q4 (barriers to services) only
qualitative_data %>%
  select(response_id, barriers_to_support) %>%
  drop_na(barriers_to_support) %>%  
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q4 Responses.csv")

# Q5 (definition) only
qualitative_data %>%
  select(response_id, autism_def) %>%
  drop_na(autism_def) %>%  
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q5 Responses.csv")

# Q6 (how first aware) only
qualitative_data %>%
  select(response_id, how_first_aware) %>%
  drop_na(how_first_aware) %>%  
  write.csv("H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualitative Responses to Code\\Q6 Responses.csv")
