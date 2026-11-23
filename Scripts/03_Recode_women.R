# 03 - recode women's data
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Renames variables, builds the survey weight and recodes demographics, diabetes, tobacco, alcohol and BMI for women (15-49).
# reads : raw women's file (needs women_vars from script 02)
# writes: Clean_data/nfhs_women_clean.rds
# Run script 02 first in the same session or women_vars will not be found.

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))
nfhs_women_raw <- read_dta(here('Raw_data_women','IAIR7EFL.DTA'),
                             col_select = all_of(women_vars))

# 2. row number ----
nfhs_women_raw <- nfhs_women_raw |>
  mutate(row_id_women = row_number())

cat('Row numbers added. Range:', min(nfhs_women_raw$row_id_women),
    'to', max(nfhs_women_raw$row_id_women), '\n')

# 3. ids to link women to the household file ----
# The household file lists every household member, so to attach TB status to a woman
# she needs an id that matches a row in that file.
#   household_id = cluster number + household number          e.g. '120_7'      (v001, v002)
#   person_id    = household_id + the woman's line number     e.g. '120_7_3'    (v001, v002, v003)
# Line number (v003) is the woman's position in the household list. So person_id points to
# one woman, and household_id points to the home she lives in (shared by everyone in it).
nfhs_women_raw <- nfhs_women_raw |>
  mutate(
    person_id = paste0(as.numeric(v001), '_',
                       as.numeric(v002), '_',
                       as.numeric(v003)),

    household_id = paste0(as.numeric(v001), '_',
                          as.numeric(v002)),

    sex = 'Women'
  )

# every woman must have her own id
stopifnot(n_distinct(nfhs_women_raw$person_id) == nrow(nfhs_women_raw))

n_total   <- nrow(nfhs_women_raw)
n_unique  <- length(unique(nfhs_women_raw$person_id))
cat('Total women:', n_total, '| Unique person IDs:', n_unique, '\n')

# 4. rename variables ----
nfhs_w <- nfhs_women_raw |>
  rename(
# survey design
    cluster   = v001,
    hh_num    = v002,
    line_num  = v003,
    weight_raw = v005,
    psu        = v021,
    sample_strata = v022,

# demographics
    age_years     = v012,
    age_5yr_group = v013,
    state_code    = v024,
    residence_raw = v025,
    edu_level     = v106,
    edu_years     = v107,
    religion_raw  = v130,
    caste_raw     = v131,
    sc_st_raw     = s116,

# wealth
    wealth_idx_raw  = v190,
    wealth_score    = v191,
    wealth_urban_rural = v190a,
    wealth_score_urban_rural = v191a,
    wealth_state    = s190s,
    wealth_score_state = s191s,

# bmi (stored x100)
    bmi_raw = v445,

# outcome and exposures
    diabetes_raw      = s728a,
    alcohol_raw       = s720,

# tobacco items
    sm_cig_raw = v463a,
    sm_pipe_raw = v463b,
    chew_raw = v463c,
    snuff_nose_raw = v463d,
    sm_cigars_raw = v463f,
    sm_hookah_raw = v463g,
    betel_raw = v463i,
    ghutka_raw = v463j,
    khaini_raw = v463k,
    paan_raw = v463l,
    smoke_status_raw  = v463x,
    no_smoke_raw      = v463z,
    cig_freq_raw      = v463aa,
    other_tobacco_freq_raw = v463ab,
    num_cigs_raw      = v464,
    smoke_bidi_raw    = s708,
    smoke_dur_cig_raw = s707,
    smoke_freq_bidi_raw = s709,
    smoke_dur_bidi_raw = s710,
    paan_smk_raw      = s712f,
    slt_freq_raw  = s713,
    slt_dur_raw   = s714,
    quit_attempt  = s716
  )

# 5. check the codes ----
table(as.numeric(nfhs_w$wealth_idx_raw) , useNA = "always")
table(as.numeric(nfhs_w$residence_raw) , useNA = "always")
table(as.numeric(nfhs_w$edu_level) , useNA = "always")
table(as.numeric(nfhs_w$religion_raw) , useNA = "always")
table(as.numeric(nfhs_w$caste_raw) , useNA = "always")
table(as.numeric(nfhs_w$age_years) , useNA = "always")
table(as.numeric(nfhs_w$wealth_idx_raw) , useNA = "always")

# 6. sampling weight ----
# DHS rule: v005 / 1,000,000
nfhs_w <- nfhs_w %>%
  mutate(
    wt = as.numeric(weight_raw) / 1000000
  )

# 7. recode core variables ----
# residence
nfhs_w <- nfhs_w %>%
  mutate(
    residence = case_when(
      as.numeric(residence_raw) == 1 ~ "Urban",
      as.numeric(residence_raw) == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    residence = factor(residence, levels = c("Rural", "Urban"))
  )

cat('Residence missing:', sum(is.na(nfhs_w$residence)), '\n')

# age group. 55-59 is empty for women, kept only so the levels match the men's file
nfhs_w <- nfhs_w %>%
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
nfhs_w <- nfhs_w %>%
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
# religion (codes 5-9 and 96 go to Other)
nfhs_w <- nfhs_w %>%
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
# caste / tribe (v131)
nfhs_w <- nfhs_w %>%
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

# social group (SC/ST/OBC). the NAs here are real missing, say so in the paper
nfhs_w <- nfhs_w %>%
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

# wealth quintile (national), wealth_rank is 1 to 5
nfhs_w <- nfhs_w %>%
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
nfhs_w <- nfhs_w %>%
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
# 'don't know' is set to NA
nfhs_w <- nfhs_w %>%
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

cat('Diabetes missing (DK codes):', sum(is.na(nfhs_w$diabetes)), '- expected: 8,227\n')

# 9. tobacco ----
# each item to 0/1, then any-smoked and any-smokeless counted with rowSums
# so one missing item does not make the whole thing NA
nfhs_w <- nfhs_w |>
  mutate(
    smoke_cig = case_when(
      as.numeric(sm_cig_raw) == 1 ~ 1L,
      as.numeric(sm_cig_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    smoke_pipe = case_when(
      as.numeric(sm_pipe_raw) == 1 ~ 1L,
      as.numeric(sm_pipe_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    smoke_cigars = case_when(
      as.numeric(sm_cigars_raw) == 1 ~ 1L,
      as.numeric(sm_cigars_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    smoke_hookah = case_when(
      as.numeric(sm_hookah_raw) == 1 ~ 1L,
      as.numeric(sm_hookah_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    smoke_bidi = case_when(
      as.numeric(smoke_bidi_raw) %in% c(1,2) ~ 1L,
      as.numeric(smoke_bidi_raw) %in% c(0,3) ~ 0L,
      TRUE ~ NA_integer_
    ),

# number of yes answers and number of valid answers
    smoke_sum = rowSums(
      across(c(smoke_cig, smoke_pipe, smoke_cigars, smoke_hookah, smoke_bidi),
             ~ .x), na.rm = TRUE
    ),

    smoke_valid = rowSums(
      across(c(smoke_cig, smoke_pipe, smoke_cigars, smoke_hookah, smoke_bidi),
             ~ !is.na(.x))
    ),

# smoker if any item is yes, non-smoker if answered and none yes, NA only if all items missing
    smoke_any = case_when(
      smoke_sum >= 1            ~ 1L,
      smoke_sum == 0 & smoke_valid > 0 ~ 0L,
      TRUE ~ NA_integer_
    ),

# smokeless items (betel quid with tobacco is not counted)
    slt_chew = case_when(
      as.numeric(chew_raw) == 1 ~ 1L,
      as.numeric(chew_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    slt_gutka = case_when(
      as.numeric(ghutka_raw) == 1 ~ 1L,
      as.numeric(ghutka_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    slt_khaini = case_when(
      as.numeric(khaini_raw) == 1 ~ 1L,
      as.numeric(khaini_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    slt_paan = case_when(
      as.numeric(paan_raw) == 1 ~ 1L,
      as.numeric(paan_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    slt_snuff = case_when(
      as.numeric(snuff_nose_raw) == 1 ~ 1L,
      as.numeric(snuff_nose_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    ),

    slt_sum = rowSums(
      across(c(slt_chew, slt_gutka, slt_khaini, slt_paan, slt_snuff),
             ~ .x), na.rm = TRUE
    ),
    slt_valid = rowSums(
      across(c(slt_chew, slt_gutka, slt_khaini, slt_paan, slt_snuff),
             ~ !is.na(.x))
    ),
    slt_any = case_when(
      slt_sum >= 1                ~ 1L,
      slt_sum == 0 & slt_valid > 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 10. tobacco status (4 groups) and any tobacco ----
nfhs_w <- nfhs_w |>
  mutate(
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

# prevalence check against NFHS-5 report ----
nfhs_w |>
  summarise(
    pct_smoke    = round(mean(smoke_any==1L, na.rm=TRUE)*100, 1),
    pct_slt      = round(mean(slt_any==1L,   na.rm=TRUE)*100, 1),
    pct_any_tob  = round(mean(tobacco_any==1L, na.rm=TRUE)*100, 1),
    pct_bidi     = round(mean(smoke_bidi==1L, na.rm=TRUE)*100, 1)
  )

nfhs_w |> count(tobacco_status) |> mutate(pct=round(n/sum(n)*100,1))

# 11. alcohol ----
nfhs_w <- nfhs_w %>%
  mutate(
    alcohol = case_when(
      is.na(as.numeric(alcohol_raw))~ NA_integer_,
      as.numeric(alcohol_raw) == 1 ~ 1L,
      as.numeric(alcohol_raw) == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

cat('Alcohol missing:', sum(is.na(nfhs_w$alcohol)), '- expected: 0\n')

# 12. bmi (women only) ----
# raw is bmi x 100, 9997 to 9999 are missing codes
nfhs_w <- nfhs_w |>
  mutate(
    bmi_num = as.numeric(bmi_raw),

    bmi = case_when(
      bmi_num %in% c(9997, 9998, 9999) ~ NA_real_,
      bmi_num == 0                      ~ NA_real_,
      bmi_num > 0 & bmi_num < 9997     ~ bmi_num / 100
    ),

    bmi = ifelse(!is.na(bmi) & (bmi < 10 | bmi > 60), NA_real_, bmi),

    bmi_cat = case_when(
      bmi < 18.5              ~ 'Underweight',
      bmi >= 18.5 & bmi < 25  ~ 'Normal',
      bmi >= 25  & bmi < 30   ~ 'Overweight',
      bmi >= 30               ~ 'Obese',
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat,
                     levels = c('Underweight','Normal','Overweight','Obese'))
  ) |>
  select(-bmi_num)

cat('BMI missing:', sum(is.na(nfhs_w$bmi)), '- expected: 24,753\n')

# 13. state names from the stata labels ----
nfhs_w <- nfhs_w %>%
  mutate(
    state_name = as_factor(state_code) %>% as.character() %>%
      str_to_title() %>%
      str_trim() %>%
      factor()
  )

levels(nfhs_w$state_name)

# 14. missing data report ----
key_vars_w <- c(
  'wt','psu','sample_strata','residence','age_group','edu_cat',
  'religion','caste','social_group','wealth_quintile','wealth_quintile_state',
  'bmi','bmi_cat','diabetes','diabetes_status','alcohol',
  'smoke_cig','smoke_bidi','smoke_pipe','smoke_cigars','smoke_hookah',
  'slt_chew','slt_gutka','slt_khaini','slt_paan','slt_snuff',
  'smoke_any','slt_any','tobacco_any','tobacco_status'
)

miss_report_w <- nfhs_w |>
  select(all_of(key_vars_w)) |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "n_missing"
  ) |>
  mutate(
    pct_missing = round(100 * n_missing / nrow(nfhs_w), 2),
    flag = case_when(
      n_missing == 0
      ~ "OK",
      variable %in% c("bmi","bmi_cat") & n_missing == 24753
      ~ "Expected (9997+9998 codes = real measurement exclusions)",
      variable == "diabetes" & n_missing == 8227
      ~ "Expected (code 8 = Don't Know -> NA)",
      variable == "social_group"
      ~ "Note: Real missing (sc_st_raw structural NAs + code 8 refusals - document)",
      variable == "caste"
      ~ "Note: Real missing (code 998 = DK in v131 - document in manuscript)",
      n_missing > 0 & pct_missing < 2
      ~ "Note: Small real missing - document",
      TRUE
      ~ "CHECK: unexpected"
    )
  ) |>
  arrange(desc(n_missing))

print(miss_report_w, n = Inf)

# 15. keep final variables and save ----
nfhs_w_clean <- nfhs_w |>
  select(
             row_id_women, person_id, household_id, sex,
             cluster, hh_num, line_num,
             psu, sample_strata, wt,
             state_code, state_name, residence,
             age_years, age_group, edu_level, edu_cat, religion_raw, religion,
             caste_raw, caste, sc_st_raw, social_group,
             wealth_idx_raw , wealth_score, wealth_quintile, wealth_rank, wealth_quintile_state,
             wealth_state, wealth_score_state,
             bmi_raw, bmi, bmi_cat, diabetes_raw, diabetes, diabetes_status, alcohol_raw, alcohol,
             smoke_cig, smoke_bidi, smoke_pipe, smoke_cigars, smoke_hookah,
             slt_chew, slt_gutka, slt_khaini, slt_paan, slt_snuff,
             smoke_any, slt_any, tobacco_any, tobacco_status
           )

cat('Women clean:', nrow(nfhs_w_clean), 'rows,', ncol(nfhs_w_clean), 'cols\n')
saveRDS(nfhs_w_clean, here('Clean_data','nfhs_women_clean.rds'))
cat('Saved: nfhs_women_clean.rds\n')

# reload to check the save worked
test <- readRDS(here("Clean_data","nfhs_women_clean.rds"))
cat("Rows:",nrow(test),"\n")
cat("Cols:",ncol(test),"\n")
cat("person_id unique:", n_distinct(test$person_id) == nrow(test),"\n")
rm(test)
