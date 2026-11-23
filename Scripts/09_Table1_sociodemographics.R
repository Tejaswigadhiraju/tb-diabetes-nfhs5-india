# 09 - Table 1
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Sociodemographic and behavioural profile of respondents by sex. Counts are unweighted, percentages are survey weighted.
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/table1_base.rds, table1_corrected.rds, TABLE1_FINAL.rds, TABLE1_FINAL.docx

# 1. setup ----
library(here)
library(survey)
library(gtsummary)
library(dplyr)
library(flextable)

source(here('Scripts','00_Setup and package loading.R'))

# 2. load data ----
nfhs_analytic <- readRDS(here('Clean_data','nfhs_analytic.rds'))
cat('Analytic sample N:', nrow(nfhs_analytic), '\n')

# 3. survey design ----
# psu = cluster, strata = sample_strata, weights = wt
# nest = TRUE because cluster numbers repeat across strata
# weights should be small decimals (about 0.1 to 5)
summary(nfhs_analytic$wt)

stopifnot(sum(is.na(nfhs_analytic$psu)) == 0)
stopifnot(sum(is.na(nfhs_analytic$sample_strata)) == 0)
stopifnot(sum(is.na(nfhs_analytic$wt)) == 0)

# strata with only one PSU
options(survey.lonely.psu = "adjust")

nfhs_design <- svydesign(
  ids     = ~psu,
  strata  = ~sample_strata,
  weights = ~wt,
  data    = nfhs_analytic,
  nest    = TRUE
)
cat('\nSurvey design object built successfully.\n')

# 4. build table 1 ----
# missing = 'no': percentages are among people who answered that question
table1 <- nfhs_design |>
  tbl_svysummary(
    by = sex,
    include = c(age_group, residence, edu_cat, religion, caste,
                wealth_quintile, alcohol, tobacco_status),
    label = list(
      age_group        ~ 'Age group, years',
      residence         ~ 'Place of residence',
      edu_cat           ~ 'Highest education level',
      religion           ~ 'Religion',
      caste               ~ 'Caste/tribe self-identification',
      wealth_quintile     ~ 'Household wealth quintile (national)',
      alcohol              ~ 'Current alcohol use',
      tobacco_status        ~ 'Tobacco use category'
    ),
    statistic = all_categorical() ~ '{n_unweighted} ({p}%)',
    missing = 'no'
  )

saveRDS(table1, here('Tables', 'table1_base.rds'))
table1

# 5. drop the 55-59 row (no one that age in either file) ----
table1 <- table1 |>
  modify_table_body(~ .x |> filter(!(variable == 'age_group' & label == '55-59')))

saveRDS(table1, here('Tables', 'table1_corrected.rds'))
table1

# 6. header, caption, footnote ----
n_total_clean <- format(nrow(nfhs_analytic), big.mark = ',')

table1 <- table1 |>
  modify_header(label = '**Characteristic**') |>
  modify_caption(paste0(
    'Table 1. Sociodemographic and behavioural profile of NFHS-5 respondents, by sex (survey-weighted, N = ',
    n_total_clean, ')'
  )) |>
  modify_footnote(
    all_stat_cols() ~ "Values are unweighted n (survey-weighted %), calculated using the survey package in R to account for NFHS-5's clustering, stratification, and sampling weights."
  ) |>
  bold_labels()

# 7. save ----
saveRDS(table1, here('Tables', 'TABLE1_FINAL.rds'))
table1 |> as_flex_table() |> flextable::save_as_docx(path = here('Tables', 'TABLE1_FINAL.docx'))

table1
cat('\nDone. Final table saved to Tables/TABLE1_FINAL.docx\n')
