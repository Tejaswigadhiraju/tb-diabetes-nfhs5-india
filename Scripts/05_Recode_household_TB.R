# 05 - household file: TB
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Builds the TB variables from the household member (PR) file. TB is household reported: one question for the household (sh28) and the line numbers of the members named (sh29a-d).
# reads : raw household file (needs household_vars from script 02), nfhs_women_clean.rds
# writes: Clean_data/tb_roster.rds, hh_tb_status.rds, hh_person_agesex.rds
# Run script 02 first in the same session.

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))
nfhs_household_raw <- read_dta(here('Raw_data_household','IAPR7EFL.DTA'),
                               col_select = all_of(household_vars))

# 2. look at sh28 and sh29a before doing anything ----
cat('PR file rows:', nrow(nfhs_household_raw), '\n')

cat('\n-- sh28 (household TB indicator) --\n')
table(as.numeric(nfhs_household_raw$sh28), useNA = 'always')
labelled::val_labels(nfhs_household_raw$sh28)

cat('\n-- sh29a (line number of 1st TB member) --\n')
table(as.numeric(nfhs_household_raw$sh29a), useNA = 'always')

# 3. ids so people and households can be matched ----
# must be built exactly like the ids in scripts 03 and 04
nfhs_household_raw <- nfhs_household_raw |>
  mutate(
    person_id = paste0(
      as.numeric(hv001), '_',
      as.numeric(hv002), '_',
      as.numeric(hvidx)
    ),
    household_id = paste0(as.numeric(hv001), '_', as.numeric(hv002))
  )

cat('Sample person_ids from PR file:\n')
head(nfhs_household_raw$person_id, 5)
# check the id format looks the same as the women's file
nfhs_w_check <- readRDS(here('Clean_data','nfhs_women_clean.rds'))
cat('Sample person_ids from women file:\n')
head(nfhs_w_check$person_id, 5)
rm(nfhs_w_check)

# 4. TB roster: one row per person named as having TB ----
# sh29a-d hold line numbers, reshaped from wide to long
tb_roster <- nfhs_household_raw |>
  select(household_id, sh29a, sh29b, sh29c, sh29d) |>
 pivot_longer(
    cols      = c(sh29a, sh29b, sh29c, sh29d),
    names_to  = 'tb_slot',
    values_to = 'tb_line_num'
  ) |>
  mutate(tb_line_num = as.numeric(tb_line_num)) |>
# drop empty slots (NA, 0) and code 95
    filter(
    !is.na(tb_line_num),
    tb_line_num != 95,
    tb_line_num != 0
  ) |>
  mutate(
    person_id = paste0(household_id, '_', tb_line_num),
    tb = 1L
  ) |>
  select(person_id, tb) |>
  distinct()

cat('TB persons identified in household roster:', nrow(tb_roster), '\n')
head(tb_roster, 10)

# save
saveRDS(tb_roster, here('Clean_data','tb_roster.rds'))
cat('Saved: tb_roster.rds\n')

# 5. household level TB status ----
# needed so 'not named' can be told apart from 'household answer unknown'
hh_tb_status <- nfhs_household_raw |>
  mutate(sh28_num = as.numeric(sh28)) |>
  select(household_id, sh28_num) |>
  distinct(household_id, .keep_all = TRUE) |>
  mutate(
    hh_tb = case_when(
      sh28_num == 0 ~ 0L,
      sh28_num == 1 ~ 1L,
      TRUE ~ NA_integer_
    )
  ) |>
  select(household_id, hh_tb)

cat('Households with known TB status (0/1):', sum(!is.na(hh_tb_status$hh_tb)), '\n')
cat('Households with UNKNOWN TB status:', sum(is.na(hh_tb_status$hh_tb)), '\n')

saveRDS(hh_tb_status, here('Clean_data','hh_tb_status.rds'))
cat('Saved: hh_tb_status.rds\n')

# 6. age and sex of every household member (used in script 07 to check eligibility) ----
hh_person_agesex <- nfhs_household_raw |>
  mutate(
    person_id = paste0(as.numeric(hv001), '_', as.numeric(hv002), '_', as.numeric(hvidx)),
    age    = as.numeric(hv105),
    sex_hh = as.numeric(hv104)
  ) |>
  select(person_id, age, sex_hh) |>
  distinct(person_id, .keep_all = TRUE)

saveRDS(hh_person_agesex, here('Clean_data','hh_person_agesex.rds'))
cat('Saved: hh_person_agesex.rds\n')
