## Reliability coding!

## Load packages
library(openxlsx)
library(irrCAC)
library(tidyverse)
library(here)
library(scales)
`%+%` <- paste0

## Reliability function
calculate_reliability <- function(df1, df2, codes) {
  
  if(
    all(names(df1) != names(df2)) |
    all(dim(df1) != dim(df2))
  ) {
    stop("Datasets must have equal dimensions, column names")
  }
  
  if(
    !all(codes %in% names(df1))
  ) {
    stop("All codes must appear in datasets")
  }
  
  out <- tibble(
    code = codes,
    agreement = NA,
    ac1 = NA
  )
  
  for(i in 1:nrow(out)) {
    
    x <- out$code[i]
    
    ratings <- bind_cols(
      "x1" = df1[[x]],
      "x2" = df2[[x]]
    ) %>%
      replace_na(
        list(
          x1 = 0, 
          x2 = 0
        )
      )
    
    out$agreement[i] <- ratings %>%
      pa.coeff.raw() %>%
      pluck("est") %>%
      pluck("pa")
    
    out$ac1[i] <- ratings %>%
      gwet.ac1.raw() %>%
      pluck("est") %>%
      pluck("coeff.val")
    
  }
  
  return(out)
  
}



####  Q3  ####
# Round 1
q3_r1_eg <- read.xlsx(here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 1 - EG Codes.xlsx")) %>%
  slice(1:50) %>%
  select(-Column1)

q3_r1_jp <- read.xlsx(here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 1 - JP Codes.xlsx")) %>%
  slice(1:50)

q3_r1_codes <- setdiff(names(q3_r1_eg), c("response_id", "group", "want_dx", "barriers_to_dx"))

q3_r1_reliability <- calculate_reliability(q3_r1_eg, q3_r1_jp, q3_r1_codes)

q3_r1_reliability %>%
  arrange(ac1)

q3_r1_codes_to_redo <- q3_r1_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q3_r1_reliability,
  here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 1 - Results.xlsx")
)

# Round 2
q3_r2_eg <- read.xlsx(here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 2 - EG Codes.xlsx")) %>%
  slice(51:100) %>%
  select(response_id, `Healthcare.System:.Professionals`:`Short.Answers`)

q3_r2_jp <- read.xlsx(here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 2 - JP Codes.xlsx")) %>%
  slice(51:100) %>%
  select(response_id, `Healthcare.System:.Professionals`:`Short.Answers`)

# Exploratory: How would a combined healthcare systems code fare?
q3_r2_eg$`Posthoc: Healthcare Systems, Combined` <- q3_r2_eg$`Healthcare.System:.Professionals` | q3_r2_eg$`Healthcare.System:.Other`
q3_r2_jp$`Posthoc: Healthcare Systems, Combined` <- q3_r2_jp$`Healthcare.System:.Professionals` | q3_r2_jp$`Healthcare.System:.Other`

q3_r2_codes <- c(q3_r1_codes_to_redo, "Posthoc: Healthcare Systems, Combined")

q3_r2_reliability <- calculate_reliability(q3_r2_eg, q3_r2_jp, q3_r2_codes)

q3_r2_reliability %>%
  arrange(ac1)

q3_r2_codes_to_redo <- q3_r2_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q3_r2_reliability,
  here("Coding", "Q3 Reliability", "ASIS Coding - Q3 - Reliability 2 - Results.xlsx")
)


####  Q4  ####
# Round 1
q4_r1_eg <- read.xlsx(here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 1 - EG Codes.xlsx")) %>%
  slice(1:50) %>%
  select(-Column1)

q4_r1_jp <- read.xlsx(here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 1 - JP Codes.xlsx")) %>%
  slice(1:50)

q4_r1_codes <- setdiff(names(q4_r1_eg), c("response_id", "barriers_to_support"))

q4_r1_reliability <- calculate_reliability(q4_r1_eg, q4_r1_jp, q4_r1_codes)

q4_r1_reliability %>%
  arrange(ac1)

q4_r1_codes_to_redo <- q4_r1_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q4_r1_reliability,
  here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 1 - Results.xlsx")
)

# Round 2
q4_r2_eg <- read.xlsx(here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 2 - EG Codes.xlsx")) %>%
  slice(51:100) %>%
  select(response_id, `Mistrust,.Fear`)

q4_r2_jp <- read.xlsx(here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 2 - JP Codes.xlsx")) %>%
  slice(51:100) %>%
  select(response_id, `Mistrust,.Fear`)

q4_r2_codes <- q4_r1_codes_to_redo

q4_r2_reliability <- calculate_reliability(q4_r2_eg, q4_r2_jp, q4_r2_codes)

q4_r2_reliability %>%
  arrange(ac1)

q4_r2_codes_to_redo <- q4_r2_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q4_r2_reliability,
  here("Coding", "Q4 Reliability", "ASIS Coding - Q4 - Reliability 2 - Results.xlsx")
)


####  Q5  ####
# Round 1
q5_r1_abby <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 1 - AH Codes.xlsx")) %>%
  slice(1:50)

q5_r1_josie <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 1 - JP Codes.xlsx")) %>%
  slice(1:50)

q5_r1_codes <- setdiff(names(q5_r1_abby), c("response_id", "autism_def"))

q5_r1_reliability <- calculate_reliability(q5_r1_abby, q5_r1_josie, q5_r1_codes)

q5_r1_reliability %>%
  arrange(ac1)

q5_r1_codes_to_redo <- q5_r1_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q5_r1_reliability,
  here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 1 - Results.xlsx")
)

# Round 2
q5_r2_abby <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 2 - AH Codes.xlsx")) %>%
  slice(51:100)

q5_r2_josie <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 2 - JP Codes.xlsx")) %>%
  slice(51:100)

q5_r2_codes <- setdiff(names(q5_r2_abby), c("response_id", "autism_def"))

q5_r2_reliability <- calculate_reliability(q5_r2_abby, q5_r2_josie, q5_r2_codes)

q5_r2_reliability %>%
  arrange(ac1)

q5_r2_reliability %>%
  filter(code %in% q5_r1_codes_to_redo) %>%
  arrange(ac1)

write.xlsx(
  q5_r2_reliability,
  here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 2 - Results.xlsx")
)

# Round 3
q5_r3_abby <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 3 - AH Codes.xlsx"))

q5_r3_josie <- read.xlsx(here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 3 - JP Codes.xlsx")) %>%
  select(autism_def, Comparison) %>%
  slice(101:150)

q5_r3_codes <- "Comparison"

q5_r3_reliability <- calculate_reliability(q5_r3_abby, q5_r3_josie, q5_r3_codes)

q5_r3_reliability

write.xlsx(
  q5_r3_reliability,
  here("Coding", "Q5 Reliability", "ASIS Coding - Q5 - Reliability 3 - Results.xlsx")
)
