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
    !all(names(df1) == names(df2)) |
    !all(dim(df1) == dim(df2))
  ) stop("Datasets must have equal dimensions, column names")
  
  if(!all(codes %in% names(df1))) stop("All codes must appear in datasets")
  
  out <- tibble(
    code = codes,
    agreement = NA,
    ac1 = NA
  )
  
  for(i in 1:nrow(out)) {
    
    x <- out$code[i]
    
    if(
      !is.numeric(df1[[x]]) |
      !is.numeric(df2[[x]])
    ) stop("Variable " %+% x %+% " must be numeric")
    
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



####  Q1  ####
# Round 1
q1_r1_ag <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 1 - AG Codes.xlsx"),
  sheet = "Q1 Ava Coding",
  startRow = 2,
  rows = 2:52
) %>%
  select(
    -c(`Addressing.Stigma.&.Prejudice.(PARENT.CODE)`, Stigma.from.Others, Comments)
  ) %>%
  rename(
    "Brief.Answers" = "Brief.Answers.(PARENT.CODE)",
    "Maybe" = "Maybe/Don't.Know/.Not.Sure",
    "Confirmation/Validation" = "Confirmation/Validation.(PARENT.CODE)",
    "Confirmation.by.Others" = "Confirmation.by.Others.(e.g.,.Family,.Friends,.&.Loved.Ones)",
    "Documentation.of.Diagnosis" = "Documentation.of.Diagnosis.(PARENT.CODE)",
    "Access.to.Help.&.Support" = "Access.to.Help.&.Support.(PARENT.CODE)",
    "Disability.Benefits" = "Disability.Benefits.(e.g.,.Disability.Benefits.(i.e.,.financial,.services.-.Social.Security,.vocational.services)",
    "Personal.Reasons" = "Personal.Reasons.(PARENT.CODE)",
    "To.Understand.Myself" = "To.Understand.Myself/My.Behavior",
    "To.Support.Myself" = "To.Support.Myself.(e.g.,.better.cope.or.manage)",
    "Curious" = "Curious.(without.reason.for.self-understanding)",
    "Peace.of.Mind" = "Peace.of.Mind,.Self-Acceptance,.and/or.Self-Confidence.(i.e.,.fear.of.being.wrong,.or.not.knowing,.faking.it)",
    "To.Feel.Less.Alone" = "To.Feel.Less.Alone/Like.an.Outsider.(i.e.,.Access.to.Community)",
    "For.Others" = "I.Want.a.Diagnosis.for.Others/External.Reasons.for.Diagnosis.(PARENT.CODE)",
    "Increase.Understanding" = "Increase.Understanding.and/or.Acceptance.from.Others",
    "To.Receive.Support" = "To.Receive.Support.from.Others",
    "Addressing.Stigma.&.Prejudice" = "Towards.self-identification.or.self-diagnosis",
    "Don't.Want" = "Don't.Want.or.Need.a.Formal.Diagnosis.(PARENT.CODE)",
    "Not.Helpful" = "A.diagnosis.is.NOT.helpful.at.this.time",
    "Wouldn't.Change.Anything" = "A.diagnosis.wouldn't.change.anything",
    "Other" = "OTHER.-.Mutually.Exclusive.Code"
  )

q1_r1_eg <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 1 - EG Codes.xlsx"),
  startRow = 2,
  rows = 2:52
) %>%
  select(-Stigma.from.Others) %>%
  mutate(
    Other = Other %>%
      gsub("[^01]", "", .) %>%
      as.numeric()
  )

q1_r1_codes <- setdiff(names(q1_r1_ag), c("response_id", "group", "want_dx", "why_want_dx"))

q1_r1_reliability <- calculate_reliability(q1_r1_ag, q1_r1_eg, q1_r1_codes)

q1_r1_reliability %>%
  arrange(ac1)

q1_r1_codes_to_redo <- q1_r1_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q1_r1_reliability,
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 1 - Results.xlsx")
)

# Round 2
q1_r2_ag <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 2 - AG Codes.xlsx"),
  sheet = "Ava Coding",
  rows = c(2, 53:102)
) %>%
  select(
    -c(Comments)
  )

q1_r2_eg <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 2 - EG Codes.xlsx"),
  rows = c(2, 53:102)
)

q1_r2_codes <- q1_r1_codes_to_redo

q1_r2_reliability <- calculate_reliability(q1_r2_ag, q1_r2_eg, q1_r2_codes)

q1_r2_reliability %>%
  arrange(ac1)

q1_r2_codes_to_redo <- q1_r2_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q1_r2_reliability,
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 2 - Results.xlsx")
)

# Round 3
q1_r3_ag <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 3 - AG Codes.xlsx"),
  rows = c(2, 103:158)
) %>%
  select(
    -c(Comments)
  )

q1_r3_eg <- read.xlsx(
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 3 - EG Codes.xlsx"),
  rows = c(2, 103:158)
)

q1_r3_codes <- c("Confirmation/Validation", "Self-Confirmation", "To.Better.Understand.and.Support.Myself")

q1_r3_reliability <- calculate_reliability(q1_r3_ag, q1_r3_eg, q1_r3_codes)

q1_r3_reliability %>%
  arrange(ac1)

q1_r3_codes_to_redo <- q1_r3_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q1_r3_reliability,
  here("Coding", "Q1 Reliability", "ASIS Coding - Q1 - Reliability 3 - Results.xlsx")
)



####  Q2  ####
q2_r1_ag <- read.xlsx(
  here("Coding", "Q2 Reliability", "ASIS Coding - Q2 - Reliability 1 - AG Codes.xlsx"),
  sheet = "Q2 Ava Coded",
  startRow = 2,
  rows = 2:52
) %>%
  select(-Comments) %>%
  rename(
    "Brief.Answers" = "Brief.Answers.(PARENT.CODE)",
    "Maybe" = "Neutral/Can't.Decide/Either.Way",
    "Stigma,.Prejudice,.and.Disc." = "Stigma,.Prejudice,.and.Discrimination.(PARENT.CODE)",
    "Medical" = "Medical.(e.g.,.From.Doctors,.Insurance.Companies)",
    "As.a.Parent" = "As.a.parent/potential.parent.(e.g.,.potential.loss.of.custody.for.children)",
    "Loss.of.Rights" = "Loss.of.Rights.and/or.Autonomy",
    "Burdensome" = 'Getting.a.diagnosis.is.burdensome.or.a.\"hassle\"',
    "Gender.Identity" = "Specific.burdens.related.to.gender.identity.of.the.person.(e.g.,.woman,.assigned.female.at.birth,.transgender)",
    "Labels.are.not.Helpful" = "Labels.are.NOT.helpful.(PARENT.CODE)",
    "Diagnosis.has.no.Benefits" = "Diagnosis.has.NO.Benefits.(PARENT.CODE)",
    "Change.to.how.I.see.myself" = "Change.to.how.I.see.myself.(PARENT.CODE)",
    "Other" = "OTHER.-.Mutually.Exclusive.Code"
  )

q2_r1_eg <- read.xlsx(
  here("Coding", "Q2 Reliability", "ASIS Coding - Q2 - Reliability 1 - EG Codes.xlsx"),
  startRow = 2,
  rows = 2:52
) %>%
  mutate(
    Other = Other %>%
      gsub("[^01]", "", .) %>%
      as.numeric()
  )

q2_r1_codes <- setdiff(names(q2_r1_ag), c("response_id", "group", "want_dx", "why_dont_want_dx"))

q2_r1_reliability <- calculate_reliability(q2_r1_ag, q2_r1_eg, q2_r1_codes)

q2_r1_reliability %>%
  arrange(ac1)

q2_r1_codes_to_redo <- q2_r1_reliability %>%
  filter(ac1 < .8) %>%
  pull(code)

write.xlsx(
  q2_r1_reliability,
  here("Coding", "Q2 Reliability", "ASIS Coding - Q2 - Reliability 1 - Results.xlsx")
)



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
