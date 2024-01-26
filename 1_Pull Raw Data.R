####  Pull Raw Data  ####
# Pull response data from Qualtrics and save it locally

## Startup
library(tidyverse)
library(qualtRics)

## Pull raw data from Qualtrics
raw_data <- qualtRics::fetch_survey(
  surveyID = "SV_eWiJ8bMnFpaVAGy", 
  force_request = TRUE
)

## Save raw data locally
saveRDS(raw_data, "H:\\My Drive\\Research\\Projects\\Autism Identification\\Data\\Qualtrics Raw Data.rds")