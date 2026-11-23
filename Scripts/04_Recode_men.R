# 04 - recode men's data
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Same steps as script 03 but for men (15-54). The men's file has no BMI.
# reads : raw men's file (needs men_vars from script 02)
# writes: Clean_data/nfhs_men_clean.rds
# Run script 02 first in the same session.

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))
nfhs_men_raw <- read_dta(here('Raw_data_men','IAMR7EFL.DTA'),
                             col_select = all_of(men_vars))

# 2. row number ----
nfhs_men_raw <- nfhs_men_raw |>
  mutate(row_id_men = row_number())

cat('Row numbers added. Range:', min(nfhs_men_raw$row_id_men),
    'to', max(nfhs_men_raw$row_id_men), '\n')

# 3. ids to link men to the household file ----
# Same logic as in script 03: household_id = cluster + household number (mv001, mv002),
# person_id = household_id + the man's line number in the household list (mv003).
nfhs_men_raw <- nfhs_men_raw |>
  mutate(
    person_id = paste0(as.numeric(mv001), '_',
                       as.numeric(mv002), '_',
                       as.numeric(mv003)),

    household_id = paste0(as.numeric(mv001), '_',
                          as.numeric(mv002)),

    sex = 'Men'
  )

stopifnot(n_distinct(nfhs_men_raw$person_id) == nrow(nfhs_men_raw))

n_total   <- nrow(nfhs_men_raw)
n_unique  <- length(unique(nfhs_men_raw$person_id))
cat('Total men:', n_total, '| Unique person IDs:', n_unique, '\n')

# 4. rename variables ----
nfhs_m <- nfhs_men_raw |>
  rename(
# survey design
    cluster   = mv001,
    hh_num    = mv002,
    line_num  = mv003,
    weight_raw = mv005,
    psu        = mv021,
    sample_strata = mv022,

# demographics
    age_years     = mv012,
    age_5yr_group = mv013,
    state_code    = mv024,
    residence_raw = mv025,
    edu_level     = mv106,
    edu_years     = mv107,
    religion_raw  = mv130,
    caste_raw     = mv131,
    sc_st_raw     = sm118,

# wealth
    wealth_idx_raw  = mv190,
    wealth_score    = mv191,
    wealth_urban_rural = mv190a,
    wealth_score_urban_rural = mv191a,
    wealth_state    = sm190s,
    wealth_score_state = sm191s,

# outcome and exposures
    diabetes_raw      = sm627a,
    alcohol_raw       = sm619,

# tobacco items
    sm_cig_raw = mv463a,
    sm_pipe_raw = mv463b,
    chew_raw = mv463c,
    snuff_nose_raw = mv463d,
    sm_cigars_raw = mv463e,
    sm_hookah_raw = mv463f,
    ghutka_raw = mv463g,
    smoke_status_raw  = mv463x,
    no_smoke_raw      = mv463z,
    cig_freq_raw      = mv463aa,
    other_tobacco_freq_raw = mv463ab,
    num_cigs_raw      = mv464,
    smoke_bidi_raw    = sm607,
    smoke_dur_cig_raw = sm606,
    smoke_freq_bidi_raw = sm608,
    smoke_dur_bidi_raw = sm609,
    paan_smk_raw      = sm611f,
    slt_dur_raw   = sm613,
  )

# 5. check the codes ----
table(as.numeric(nfhs_m$wealth_idx_raw) , useNA = "always")
table(as.numeric(nfhs_m$residence_raw) , useNA = "always")
table(as.numeric(nfhs_m$edu_level) , useNA = "always")
table(as.numeric(nfhs_m$religion_raw) , useNA = "always")
table(as.numeric(nfhs_m$caste_raw) , useNA = "always")
table(as.numeric(nfhs_m$age_years) , useNA = "always")
table(as.numeric(nfhs_m$wealth_idx_raw) , useNA = "always")
table(as.numeric(nfhs_m$diabetes_raw), useNA = "always")
table(as.numeric(nfhs_m$sm_cig_raw),  useNA = "always")
table(as.numeric(nfhs_m$alcohol_raw),  useNA = "always")

# 6. sampling weight (v005 / 1,000,000) ----
nfhs_m <- nfhs_m |>
  mutate(
    wt = as.numeric(weight_raw) / 1000000
  )

# 7. recode core variables ----
# residence
nfhs_m <- nfhs_m %>%
  mutate(
    residence = case_when(
      as.numeric(residence_raw) == 1 ~ "Urban",
      as.numeric(residence_raw) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    residence = factor(residence, levels = c("Rural", "Urban"))
  )

# age group
nfhs_m <- nfhs_m %>%
  mutate(
    age_group = case_when(
      age_years >= 15 & age_years <= 19 ~ "15-19",
      age_years >= 20 & age_years <= 24 ~ "20-24",
      age_years >= 25 & age_years <= 29 ~ "25-29",
      age_years >= 30 & age_years <= 34 ~ "30-34",
      age_years >= 35 & age_years <= 39 ~ "35-39",
      age_years >= 40 & age_years <= 44 ~ "40-44",
      age_years >= 45 & age_years <= 49 ~ "45-49",
      age_years >= 50 & age_years <= 54 ~ "50-54",
      age_years >= 55 & age_years <= 59 ~ "55-59",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c(
      "15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59"
    ))
  )

# education
nfhs_m <- nfhs_m %>%
  mutate(
    edu_cat = case_when(
      as.numeric(edu_level) == 0 ~ "No education",
      as.numeric(edu_level) == 1 ~ "Primary",
      as.numeric(edu_level) == 2 ~ "Secondary",
      as.numeric(edu_level) == 3 ~ "Higher",
      TRUE ~ NA_character_
    ),
    edu_cat = factor(edu_cat,
                     levels = c("No education", "Primary", "Secondary", "Higher"))
  )

# religion
nfhs_m <- nfhs_m %>%
  mutate(
    religion = case_when(
      as.numeric(religion_raw) == 1 ~ "Hindu",
      as.numeric(religion_raw) == 2 ~ "Muslim",
      as.numeric(religion_raw) == 3 ~ "Christian",
      as.numeric(religion_raw) == 4 ~ "Sikh",
      as.numeric(religion_raw) %in% c(5,6,7,8,9,96) ~ "Other",
      TRUE ~ NA_character_
    ),
    religion = factor(religion,
                      levels = c("Hindu", "Muslim", "Christian", "Sikh", "Other"))
  )

# caste / tribe
nfhs_m <- nfhs_m %>%
  mutate(
    caste = case_when(
      as.numeric(caste_raw) == 991 ~ "Caste",
      as.numeric(caste_raw) == 992 ~ "Tribe",
      as.numeric(caste_raw) == 993 ~ "No caste/tribe",
      as.numeric(caste_raw) == 998 ~ "Don't Know",
      TRUE ~ NA_character_
    ),
    caste = factor(caste,
                   levels = c("Caste","Tribe","No caste/tribe","Don't Know"))
  )

# social group
nfhs_m <- nfhs_m %>%
  mutate(
    social_group = case_when(
  as.numeric(sc_st_raw) == 1 ~ 'Scheduled Caste',
  as.numeric(sc_st_raw) == 2 ~ 'Scheduled Tribe',
  as.numeric(sc_st_raw) == 3 ~ 'OBC',
  as.numeric(sc_st_raw) == 4 ~ 'None of the above',
  TRUE ~ NA_character_
),
social_group = factor(social_group,
                      levels = c('Scheduled Caste','Scheduled Tribe','OBC','None of the above'))
)

# wealth quintile
nfhs_m <- nfhs_m %>%
  mutate(
    wealth_quintile = case_when(
      as.numeric(wealth_idx_raw) == 1 ~ "Poorest",
      as.numeric(wealth_idx_raw) == 2 ~ "Poorer",
      as.numeric(wealth_idx_raw) == 3 ~ "Middle",
      as.numeric(wealth_idx_raw) == 4 ~ "Richer",
      as.numeric(wealth_idx_raw) == 5 ~ "Richest",
      TRUE ~ NA_character_
    ),
    wealth_quintile = factor(wealth_quintile,
                             levels = c("Poorest","Poorer","Middle","Richer","Richest")),
    wealth_rank = as.numeric(wealth_quintile)
  )

# wealth quintile within state
nfhs_m <- nfhs_m %>%
  mutate(
    wealth_quintile_state = case_when(
      as.numeric(wealth_state) == 1 ~ 'Poorest',
      as.numeric(wealth_state) == 2 ~ 'Poorer',
      as.numeric(wealth_state) == 3 ~ 'Middle',
      as.numeric(wealth_state) == 4 ~ 'Richer',
      as.numeric(wealth_state) == 5 ~ 'Richest',
      TRUE ~ NA_character_
    ),
    wealth_quintile_state = factor(wealth_quintile_state,
                                   levels=c('Poorest','Poorer','Middle','Richer','Richest'))
  )

# 8. diabetes (self-report, currently has) ----
nfhs_m <- nfhs_m %>%
  mutate(
    diabetes_status = case_when(
      as.numeric(diabetes_raw) == 0 ~ "No",
      as.numeric(diabetes_raw) == 1 ~ "Yes",
      as.numeric(diabetes_raw) == 8 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    diabetes_status = factor(diabetes_status,
                             levels = c("No","Yes","Don't know")),
    diabetes = case_when(
      diabetes_status == "Yes" ~ 1L,
      diabetes_status == "No" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 9. tobacco ----
# smoked: cigarettes, pipe, cigars, hookah, bidi
nfhs_m <- nfhs_m %>%
  mutate(
    smoke_cig = case_when(
      is.na(as.numeric(sm_cig_raw))    ~ NA_integer_,
      as.numeric(sm_cig_raw) == 1      ~ 1L,
      as.numeric(sm_cig_raw) == 0      ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    smoke_pipe = case_when(
      is.na(as.numeric(sm_pipe_raw))   ~ NA_integer_,
      as.numeric(sm_pipe_raw) == 1     ~ 1L,
      as.numeric(sm_pipe_raw) == 0     ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    smoke_cigars = case_when(
      is.na(as.numeric(sm_cigars_raw)) ~ NA_integer_,
      as.numeric(sm_cigars_raw) == 1   ~ 1L,
      as.numeric(sm_cigars_raw) == 0   ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    smoke_hookah = case_when(
      is.na(as.numeric(sm_hookah_raw)) ~ NA_integer_,
      as.numeric(sm_hookah_raw) == 1   ~ 1L,
      as.numeric(sm_hookah_raw) == 0   ~ 0L,
      TRUE                             ~ NA_integer_
    ),

# bidi: 1 = every day, 2 = some days
    smoke_bidi = case_when(
      is.na(as.numeric(smoke_bidi_raw))~ NA_integer_,
      as.numeric(smoke_bidi_raw) %in% c(1,2) ~ 1L,
      as.numeric(smoke_bidi_raw) == 0         ~ 0L,
      TRUE ~ NA_integer_
    ),
# same rowSums approach as the women's script
    smoke_sum = rowSums(
      across(c(smoke_cig,smoke_pipe,smoke_cigars,smoke_hookah,smoke_bidi), ~.x),
      na.rm = TRUE
    ),
    smoke_valid = rowSums(
      across(c(smoke_cig,smoke_pipe,smoke_cigars,smoke_hookah,smoke_bidi), ~!is.na(.x))
    ),
    smoke_any = case_when(
      smoke_sum >= 1                   ~ 1L,
      smoke_sum == 0 & smoke_valid > 0 ~ 0L,
      TRUE                             ~ NA_integer_
    )
  )

# smokeless: chew, gutka, snuff, paan with tobacco
nfhs_m <- nfhs_m %>%
  mutate(
    slt_chew = case_when(
      is.na(as.numeric(chew_raw))      ~ NA_integer_,
      as.numeric(chew_raw) == 1        ~ 1L,
      as.numeric(chew_raw) == 0        ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    slt_gutka = case_when(
      is.na(as.numeric(ghutka_raw))    ~ NA_integer_,
      as.numeric(ghutka_raw) == 1      ~ 1L,
      as.numeric(ghutka_raw) == 0      ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    slt_paan = case_when(
      is.na(as.numeric(paan_smk_raw))      ~ NA_integer_,
      as.numeric(paan_smk_raw) == 1        ~ 1L,
      as.numeric(paan_smk_raw) == 0        ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    slt_snuff = case_when(
      is.na(as.numeric(snuff_nose_raw))~ NA_integer_,
      as.numeric(snuff_nose_raw) == 1  ~ 1L,
      as.numeric(snuff_nose_raw) == 0  ~ 0L,
      TRUE                             ~ NA_integer_
    ),

    slt_sum = rowSums(
      across(c(slt_chew,slt_gutka,slt_snuff,slt_paan), ~.x),
      na.rm = TRUE
    ),
    slt_valid = rowSums(
      across(c(slt_chew,slt_gutka,slt_snuff,slt_paan), ~!is.na(.x))
    ),
    slt_any = case_when(
      slt_sum >= 1                   ~ 1L,
      slt_sum == 0 & slt_valid > 0   ~ 0L,
      TRUE                           ~ NA_integer_
    ),

# 4 group tobacco status
    tobacco_status = case_when(
      smoke_any == 0L & slt_any == 0L ~ 'Non-user',
      smoke_any == 1L & slt_any == 0L ~ 'Smoking only',
      smoke_any == 0L & slt_any == 1L ~ 'SLT only',
      smoke_any == 1L & slt_any == 1L ~ 'Dual use',
      TRUE ~ NA_character_
    ),
    tobacco_status = factor(tobacco_status,
                            levels = c('Non-user','Smoking only','SLT only','Dual use')),
    tobacco_any = case_when(
      smoke_any == 1L | slt_any == 1L ~ 1L,
      smoke_any == 0L & slt_any == 0L ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  select(-smoke_sum, -smoke_valid, -slt_sum, -slt_valid)

# 10. alcohol ----
nfhs_m <- nfhs_m %>%
  mutate(
    alcohol = case_when(
      is.na(as.numeric(alcohol_raw))~ NA_integer_,
      as.numeric(alcohol_raw) == 1 ~ 1L,
      as.numeric(alcohol_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 11. state names ----
nfhs_m <- nfhs_m %>%
  mutate(
    state_name = as_factor(state_code) %>% as.character() %>%
      str_to_title() %>%
      str_trim() %>%
      factor()
  )

levels(nfhs_m$state_name)

# 12. missing data report ----
key_vars_m <- c(
  "wt", "psu", "sample_strata", "residence", "age_group", "edu_cat",
  "religion", "caste", "social_group", "wealth_quintile", "wealth_quintile_state",
  "diabetes", "diabetes_status", "alcohol",
  "smoke_cig", "smoke_bidi", "smoke_pipe", "smoke_cigars", "smoke_hookah",
  "slt_chew", "slt_gutka", "slt_snuff", "slt_paan",
  "smoke_any", "slt_any", "tobacco_any", "tobacco_status"
)

miss_report_m <- nfhs_m |>
  select(all_of(key_vars_m)) |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_missing"
  ) |>
  mutate(
    pct_missing = round(100 * n_missing / nrow(nfhs_m), 2),
    flag = case_when(
      n_missing == 0
      ~ "OK",
      variable == "diabetes" & n_missing == 1116
      ~ "Expected (code 8 = DK -> NA)",
      variable == "social_group"
      ~ "Note: Real missing (structural NAs + code 8 - document)",
      variable == "caste"
      ~ "Note: Real missing (code 998 = DK - document)",
      n_missing > 0 & pct_missing < 2
      ~ "Note: Small real missing - document",
      TRUE
      ~ "CHECK: unexpected"
    )
) |>
  arrange(desc(n_missing))

print(miss_report_m, n = Inf)

# 13. keep final variables and save ----
nfhs_m_clean <- nfhs_m |>
  select(
    row_id_men, person_id, household_id, sex,
    cluster, hh_num, line_num,
    psu, sample_strata, wt,
    state_code, state_name, residence,
    age_years, age_group, edu_level, edu_cat, religion_raw, religion,
    caste_raw, caste, sc_st_raw, social_group,
    wealth_idx_raw , wealth_score, wealth_quintile, wealth_rank, wealth_quintile_state,
    wealth_state, wealth_score_state,
    diabetes_raw, diabetes, diabetes_status, alcohol_raw, alcohol,
    smoke_cig, smoke_bidi, smoke_pipe, smoke_cigars, smoke_hookah,
    slt_chew, slt_gutka, slt_paan, slt_snuff,
    smoke_any, slt_any, tobacco_any, tobacco_status
  )

cat('Men clean:', nrow(nfhs_m_clean), 'rows,', ncol(nfhs_m_clean), 'cols\n')
dir.create(here('Clean_data'), showWarnings = FALSE)
saveRDS(nfhs_m_clean, here('Clean_data','nfhs_men_clean.rds'))
cat('Saved: nfhs_men_clean.rds\n')

# reload to check the save worked
test_m <- readRDS(here("Clean_data","nfhs_men_clean.rds"))
cat("Rows:",nrow(test_m),"\n")
cat("Cols:",ncol(test_m),"\n")
cat("person_id unique:", n_distinct(test_m$person_id) == nrow(test_m),"\n")
rm(test_m)
