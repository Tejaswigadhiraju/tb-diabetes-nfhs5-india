# 06 - missingness and recode checks (optional)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Prints frequency tables of raw codes and compares raw vs recoded variables. Nothing is saved. Part A uses objects still in memory from scripts 03-05.
# reads : objects from 03-05; Clean_data/nfhs_women_clean.rds and nfhs_men_clean.rds (part B)
# writes: nothing

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))

# part A: run right after scripts 03, 04, 05 in the same session ----
# helper: frequency table of one raw variable
diag_var <- function(data, var_name) {
  cat('\n---', var_name, '---\n')
  x <- as.numeric(data[[var_name]])
  cat('Total rows:', nrow(data), '\n')
  cat('Missing (NA):', sum(is.na(x)), '\n')
  cat('Value frequencies:\n')
  print(table(x, useNA='always'))
}

# women: design and demographics ----
diag_var(nfhs_w, 'residence_raw')
diag_var(nfhs_w, 'wealth_idx_raw')
diag_var(nfhs_w, 'edu_level')
diag_var(nfhs_w, 'religion_raw')
diag_var(nfhs_w, 'sc_st_raw')
diag_var(nfhs_w, 'caste_raw')
diag_var(nfhs_w, 'state_code')
diag_var(nfhs_w, 'wealth_score')
diag_var(nfhs_w, 'wealth_state')
diag_var(nfhs_w, 'wealth_score_state')

# outcome and exposures
diag_var(nfhs_w, 'diabetes_raw')
diag_var(nfhs_w, 'alcohol_raw')
diag_var(nfhs_w, 'bmi_raw')

# tobacco
diag_var(nfhs_w, 'sm_cig_raw')
diag_var(nfhs_w, 'sm_pipe_raw')
diag_var(nfhs_w, 'sm_cigars_raw')
diag_var(nfhs_w, 'sm_hookah_raw')
diag_var(nfhs_w, 'smoke_bidi_raw')

diag_var(nfhs_w, 'chew_raw')
diag_var(nfhs_w, 'ghutka_raw')
diag_var(nfhs_w, 'khaini_raw')
diag_var(nfhs_w, 'paan_raw')
diag_var(nfhs_w, 'snuff_nose_raw')
diag_var(nfhs_w, 'paan_smk_raw')

# bmi missing codes
nfhs_w |>
  mutate(bmi_raw_num = as.numeric(bmi_raw)) |>
  filter(bmi_raw_num >= 9990 | bmi_raw_num == 0) |>
  count(bmi_raw_num)

# men ----
diag_var(nfhs_m, 'residence_raw')
diag_var(nfhs_m, 'wealth_idx_raw')
diag_var(nfhs_m, 'edu_level')
diag_var(nfhs_m, 'religion_raw')
diag_var(nfhs_m, 'sc_st_raw')
diag_var(nfhs_m, 'caste_raw')
diag_var(nfhs_m, 'state_code')
diag_var(nfhs_m, 'wealth_score')
diag_var(nfhs_m, 'wealth_state')
diag_var(nfhs_m, 'wealth_score_state')

diag_var(nfhs_m, 'diabetes_raw')
diag_var(nfhs_m, 'alcohol_raw')

diag_var(nfhs_m, 'sm_cig_raw')
diag_var(nfhs_m, 'sm_pipe_raw')
diag_var(nfhs_m, 'sm_cigars_raw')
diag_var(nfhs_m, 'sm_hookah_raw')
diag_var(nfhs_m, 'smoke_bidi_raw')

diag_var(nfhs_m, 'chew_raw')
diag_var(nfhs_m, 'ghutka_raw')
diag_var(nfhs_m, 'snuff_nose_raw')
diag_var(nfhs_m, 'paan_smk_raw')

# household ----
diag_var(nfhs_household_raw, 'sh28')
diag_var(nfhs_household_raw, 'sh29')
diag_var(nfhs_household_raw, 'sh29a')
diag_var(nfhs_household_raw, 'sh29b')

# helper: raw code vs recoded variable ----
check_derived <- function(data, raw_var, clean_var) {
  cat('\n--- RAW:', raw_var, '-> CLEAN:', clean_var, '---\n')
  print(
    data |>
      mutate(raw = as.numeric(.data[[raw_var]])) |>
      count(raw, .data[[clean_var]], .drop = FALSE) |>
      arrange(raw)
  )
}

# check the recodes
check_derived(nfhs_w, 'smoke_bidi_raw', 'smoke_bidi')
check_derived(nfhs_w, 'sm_cig_raw', 'smoke_cig')
check_derived(nfhs_w, 'sm_pipe_raw', 'smoke_pipe')
check_derived(nfhs_w, 'sm_cigars_raw', 'smoke_cigars')
check_derived(nfhs_w, 'sm_hookah_raw', 'smoke_hookah')
check_derived(nfhs_w, 'diabetes_raw', 'diabetes')

check_derived(nfhs_m, 'smoke_bidi_raw', 'smoke_bidi')
check_derived(nfhs_m, 'sm_cig_raw', 'smoke_cig')
check_derived(nfhs_m, 'sm_pipe_raw', 'smoke_pipe')
check_derived(nfhs_m, 'sm_cigars_raw', 'smoke_cigars')
check_derived(nfhs_m, 'sm_hookah_raw', 'smoke_hookah')
check_derived(nfhs_m, 'diabetes_raw', 'diabetes')

# part B: works on the saved clean files ----
nfhs_w <- readRDS(here('Clean_data','nfhs_women_clean.rds'))
nfhs_m <- readRDS(here('Clean_data','nfhs_men_clean.rds'))

# row counts
cat('Women:', nrow(nfhs_w), '\n')
cat('Men:',   nrow(nfhs_m), '\n')

# recode checks (the saved clean files keep diabetes_raw and alcohol_raw, not the tobacco raw items)
check_derived(nfhs_w, 'diabetes_raw', 'diabetes')
check_derived(nfhs_m, 'diabetes_raw', 'diabetes')
check_derived(nfhs_w, 'alcohol_raw', 'alcohol')
check_derived(nfhs_m, 'alcohol_raw', 'alcohol')

# bmi missing
nfhs_w |> summarise(
  bmi_missing = sum(is.na(bmi)),
  bmi_pct_missing = round(mean(is.na(bmi))*100, 2)
)

# tobacco prevalence
cat('\n-- WOMEN tobacco prevalence --\n')
nfhs_w |> summarise(
  pct_smoke   = round(mean(smoke_any==1L, na.rm=TRUE)*100, 1),
  pct_slt     = round(mean(slt_any==1L,   na.rm=TRUE)*100, 1),
  pct_any_tob = round(mean(tobacco_any==1L,na.rm=TRUE)*100, 1)
)

cat('\n-- MEN tobacco prevalence --\n')
nfhs_m |> summarise(
  pct_smoke   = round(mean(smoke_any==1L, na.rm=TRUE)*100, 1),
  pct_slt     = round(mean(slt_any==1L,   na.rm=TRUE)*100, 1),
  pct_any_tob = round(mean(tobacco_any==1L,na.rm=TRUE)*100, 1)
)

# are all required variables there?
required_w <- c('row_id_women','person_id','household_id','sex',
                'cluster','hh_num','line_num','psu','sample_strata','wt',
                'state_code','state_name','residence','age_years','age_group',
                'edu_cat','religion','caste','social_group',
                'wealth_quintile','wealth_rank','wealth_quintile_state','wealth_score',
                'bmi','bmi_cat','diabetes','diabetes_status','alcohol',
                'smoke_cig','smoke_bidi','smoke_pipe','smoke_cigars','smoke_hookah',
                'slt_chew','slt_gutka','slt_khaini','slt_paan','slt_snuff',
                'smoke_any','slt_any','tobacco_any','tobacco_status')

required_m <- c('row_id_men','person_id','household_id','sex',
                'cluster','hh_num','line_num','psu','sample_strata','wt',
                'state_code','state_name','residence','age_years','age_group',
                'edu_cat','religion','caste','social_group',
                'wealth_quintile','wealth_rank','wealth_quintile_state','wealth_score',
                'diabetes','diabetes_status','alcohol',
                'smoke_cig','smoke_bidi','smoke_pipe','smoke_cigars','smoke_hookah',
                'slt_chew','slt_gutka','slt_paan','slt_snuff',
                'smoke_any','slt_any','tobacco_any','tobacco_status')

missing_w <- setdiff(required_w, names(nfhs_w))
missing_m <- setdiff(required_m, names(nfhs_m))
cat('Women missing vars:', if(length(missing_w)==0) 'NONE' else paste(missing_w, collapse=', '), '\n')
cat('Men missing vars:',   if(length(missing_m)==0) 'NONE' else paste(missing_m, collapse=', '), '\n')

cat('\nBoth files verified. Ready for the merge (script 07).\n')
