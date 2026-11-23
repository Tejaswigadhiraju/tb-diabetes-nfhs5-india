# 07 - merge women, men and household TB
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Adds TB status to women and men, stacks the two files and runs some checks.
# reads : nfhs_women_clean.rds, nfhs_men_clean.rds, tb_roster.rds, hh_tb_status.rds, hh_person_agesex.rds
# writes: Clean_data/nfhs_merged_full.rds

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))

# 2. load ----
nfhs_w        <- readRDS(here('Clean_data','nfhs_women_clean.rds'))
nfhs_m        <- readRDS(here('Clean_data','nfhs_men_clean.rds'))
tb_roster     <- readRDS(here('Clean_data','tb_roster.rds'))
hh_tb_status  <- readRDS(here('Clean_data','hh_tb_status.rds'))

cat('Women:', nrow(nfhs_w), '| Men:', nrow(nfhs_m),
    '| TB roster (positive persons):', nrow(tb_roster),
    '| Households with TB status:', nrow(hh_tb_status), '\n')

# 3. TB for women ----
# tb = 1 if named in the roster
# tb = 0 if the household answered but she was not named
# tb = NA if the household status is unknown
n_w_before <- nrow(nfhs_w)

nfhs_w <- nfhs_w |>
  left_join(tb_roster,    by = 'person_id') |>
  left_join(hh_tb_status, by = 'household_id') |>
  mutate(
    tb = case_when(
      tb == 1L      ~ 1L,
      is.na(hh_tb)  ~ NA_integer_,
      hh_tb == 0L   ~ 0L,
      hh_tb == 1L   ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  select(-hh_tb)

stopifnot(nrow(nfhs_w) == n_w_before)

cat('\n-- Women TB verification --\n')
cat('Rows before join:', n_w_before, '| Rows after join:', nrow(nfhs_w), '\n')
cat('TB = 1 (positive):', sum(nfhs_w$tb == 1, na.rm = TRUE), '\n')
cat('TB = 0 (negative):', sum(nfhs_w$tb == 0, na.rm = TRUE), '\n')
cat('TB = NA (unknown/household not matched):', sum(is.na(nfhs_w$tb)), '\n')

# 4. TB for men (same logic) ----
n_m_before <- nrow(nfhs_m)

nfhs_m <- nfhs_m |>
  left_join(tb_roster,    by = 'person_id') |>
  left_join(hh_tb_status, by = 'household_id') |>
  mutate(
    tb = case_when(
      tb == 1L      ~ 1L,
      is.na(hh_tb)  ~ NA_integer_,
      hh_tb == 0L   ~ 0L,
      hh_tb == 1L   ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  select(-hh_tb)

stopifnot(nrow(nfhs_m) == n_m_before)

cat('\n-- Men TB verification --\n')
cat('Rows before join:', n_m_before, '| Rows after join:', nrow(nfhs_m), '\n')
cat('TB = 1 (positive):', sum(nfhs_m$tb == 1, na.rm = TRUE), '\n')
cat('TB = 0 (negative):', sum(nfhs_m$tb == 0, na.rm = TRUE), '\n')
cat('TB = NA (unknown/household not matched):', sum(is.na(nfhs_m$tb)), '\n')

# 5. how many roster people were matched? ----
# a gap is expected: children, women over 49, men over 54 and men outside the men's subsample are not in the individual files
n_tb_in_roster    <- nrow(tb_roster)
total_tb_assigned <- sum(nfhs_w$tb == 1, na.rm = TRUE) + sum(nfhs_m$tb == 1, na.rm = TRUE)

cat('\n-- Match rate check --\n')
cat('TB roster size:', n_tb_in_roster, '\n')
cat('Total TB=1 assigned across women+men:', total_tb_assigned, '\n')

# 6. why are some roster people not matched? ----
hh_person_agesex <- readRDS(here('Clean_data','hh_person_agesex.rds'))

tb_roster_tagged <- tb_roster |>
  left_join(hh_person_agesex, by = 'person_id') |>
  mutate(
    matched_any = person_id %in% nfhs_w$person_id | person_id %in% nfhs_m$person_id,
    eligibility = case_when(
      is.na(age) | is.na(sex_hh)              ~ 'Age/sex not found - investigate',
      age < 15                                 ~ 'Child <15 - never individually eligible',
      sex_hh == 2 & age >= 15 & age <= 49      ~ 'Eligible woman (15-49)',
      sex_hh == 2 & age > 49                   ~ 'Woman >49 - never eligible',
      sex_hh == 1 & age >= 15 & age <= 54      ~ 'Eligible man (15-54) - needs men subsample',
      sex_hh == 1 & age > 54                   ~ 'Man >54 - never eligible',
      TRUE ~ 'Check manually'
    )
  )

cat('\n-- Check 3.5: Eligibility breakdown of TB roster match/non-match --\n')
print(
  tb_roster_tagged |>
    group_by(eligibility) |>
    summarise(n = n(), pct_matched = round(mean(matched_any) * 100, 1)) |>
    arrange(desc(n))
)
stopifnot(
  tb_roster_tagged |>
    filter(eligibility == 'Eligible woman (15-49)') |>
    summarise(pct = mean(matched_any)) |>
    pull(pct) > 0.90
)
rm(tb_roster_tagged, hh_person_agesex)

# 7. stack women and men ----
# factors to character first so bind_rows does not complain about levels
nfhs_w2 <- nfhs_w |> mutate(across(where(is.factor), as.character))
nfhs_m2 <- nfhs_m |> mutate(across(where(is.factor), as.character))

# columns only one file has (bmi) are added as NA to the other
for (col in setdiff(names(nfhs_w2), names(nfhs_m2))) nfhs_m2[[col]] <- NA
for (col in setdiff(names(nfhs_m2), names(nfhs_w2))) nfhs_w2[[col]] <- NA

nfhs_all <- bind_rows(nfhs_w2, nfhs_m2)

cat('\nTotal combined rows:', nrow(nfhs_all),
    '(expected:', nrow(nfhs_w) + nrow(nfhs_m), ')\n')

# 8. set the factor levels again ----
# spelling must match scripts 03 and 04 exactly
nfhs_all <- nfhs_all |>
  mutate(
    sex = factor(sex, levels = c('Women','Men')),
    residence = factor(residence, levels = c('Rural','Urban')),
    edu_cat = factor(edu_cat,
                     levels = c('No education','Primary','Secondary','Higher')),
    religion = factor(religion,
                      levels = c('Hindu','Muslim','Christian','Sikh','Other')),
    social_group = factor(social_group,
                          levels = c('Scheduled Caste','Scheduled Tribe','OBC','None of the above')),
    caste = factor(caste,
                   levels = c('Caste','Tribe','No caste/tribe',"Don't Know")),
    wealth_quintile = factor(wealth_quintile,
                             levels = c('Poorest','Poorer','Middle','Richer','Richest')),
    wealth_quintile_state = factor(wealth_quintile_state,
                                   levels = c('Poorest','Poorer','Middle','Richer','Richest')),
    age_group = factor(age_group,
                       levels = c('15-19','20-24','25-29','30-34','35-39',
                                  '40-44','45-49','50-54','55-59')),
    bmi_cat = factor(bmi_cat,
                     levels = c('Underweight','Normal','Overweight','Obese')),
    tobacco_status = factor(tobacco_status,
                            levels = c('Non-user','Smoking only','SLT only','Dual use'))
  )

# no new NAs should appear
refactor_check <- nfhs_all |>
  summarise(
    caste_na           = sum(is.na(caste)),
    social_group_na    = sum(is.na(social_group)),
    wealth_quintile_na = sum(is.na(wealth_quintile)),
    tobacco_status_na  = sum(is.na(tobacco_status))
  )
print(refactor_check)

# 9. save ----
saveRDS(nfhs_all, here('Clean_data','nfhs_merged_full.rds'))
cat('\nSaved: nfhs_merged_full.rds (', nrow(nfhs_all), 'rows,', ncol(nfhs_all), 'columns )\n')

# 10. sanity checks ----
cat('\n== CHECK 1: Total counts ==\n')
print(nfhs_all |> count(sex))

cat('\n== CHECK 2: TB by sex (unweighted) ==\n')
print(
  nfhs_all |>
    group_by(sex) |>
    summarise(
      n      = n(),
      tb_yes = sum(tb == 1, na.rm = TRUE),
      tb_no  = sum(tb == 0, na.rm = TRUE),
      tb_na  = sum(is.na(tb)),
      pct_tb = round(mean(tb == 1, na.rm = TRUE) * 100, 3)
    )
)

cat('\n== CHECK 3: Top 10 states by TB rate (unweighted) ==\n')
print(
  nfhs_all |>
    group_by(state_name) |>
    summarise(pct_tb = round(mean(tb == 1, na.rm = TRUE) * 100, 2)) |>
    arrange(desc(pct_tb)) |>
    head(10)
)

cat('\n== CHECK 4: Diabetes by sex (unweighted) ==\n')
print(
  nfhs_all |>
    group_by(sex) |>
    summarise(
      n_yes  = sum(diabetes == 1, na.rm = TRUE),
      n_no   = sum(diabetes == 0, na.rm = TRUE),
      n_na   = sum(is.na(diabetes)),
      pct_dm = round(mean(diabetes == 1, na.rm = TRUE) * 100, 1)
    )
)

cat('\n== CHECK 5: TB x Diabetes cross-tab ==\n')
print(
  nfhs_all |>
    mutate(
      tb_label = factor(tb, levels = c(0,1), labels = c('No TB','TB')),
      dm_label = factor(diabetes, levels = c(0,1), labels = c('No DM','DM'))
    ) |>
    count(tb_label, dm_label, useNA = 'always') |>
    mutate(pct = round(n / sum(n) * 100, 3)) |>
    arrange(tb_label, dm_label)
)

cat('\n== CHECK 6: Survey design variable completeness ==\n')
print(
  nfhs_all |>
    select(psu, sample_strata, wt) |>
    summarise(across(everything(), ~ sum(is.na(.))))
)
cat('\nScript 07 complete. Proceed to Script 08 (TB x DM exclusion).\n')
