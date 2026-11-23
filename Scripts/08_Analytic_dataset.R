# 08 - TB x diabetes groups and analytic dataset
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Builds the four TB x diabetes groups, drops people missing both TB and diabetes and saves the analytic file.
# reads : Clean_data/nfhs_merged_full.rds
# writes: Clean_data/nfhs_analytic.rds

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))
nfhs_all <- readRDS(here('Clean_data','nfhs_merged_full.rds'))

cat('Starting rows (full merged file):', nrow(nfhs_all), '\n')

# 2. four groups ----
# if tb or diabetes is NA, the person gets NA
nfhs_all <- nfhs_all |>
  mutate(
    tb_dm_cat = case_when(
      tb == 0L & diabetes == 0L ~ 'Neither',
      tb == 1L & diabetes == 0L ~ 'TB only',
      tb == 0L & diabetes == 1L ~ 'Diabetes only',
      tb == 1L & diabetes == 1L ~ 'TB + Diabetes',
      TRUE ~ NA_character_
    ),
    tb_dm_cat = factor(tb_dm_cat,
                       levels = c('Neither','TB only','Diabetes only','TB + Diabetes'))
  )

cat('\n==== TB x DIABETES STATUS (full merged file, before exclusion) ====\n')

# 3. count table ----
count_table <- nfhs_all |>
  mutate(
    tb_str = case_when(
      tb == 1   ~ 'TB: YES',
      tb == 0   ~ 'TB: NO',
      is.na(tb) ~ 'TB: MISSING'
    ),
    dm_str = case_when(
      diabetes == 1   ~ 'DM: YES',
      diabetes == 0   ~ 'DM: NO',
      is.na(diabetes) ~ 'DM: MISSING'
    )
  ) |>
  count(tb_str, dm_str) |>
  mutate(pct = round(n / sum(n) * 100, 3)) |>
  arrange(desc(n))

print(count_table, n = Inf)

# 4. who gets excluded ----
# only people missing BOTH tb and diabetes
n_both_missing <- nfhs_all |>
  filter(is.na(tb) & is.na(diabetes)) |>
  nrow()

n_tb_missing_only <- sum(is.na(nfhs_all$tb) & !is.na(nfhs_all$diabetes))
n_dm_missing_only <- sum(!is.na(nfhs_all$tb) & is.na(nfhs_all$diabetes))
n_both_present     <- sum(!is.na(nfhs_all$tb) & !is.na(nfhs_all$diabetes))

cat('\n---- EXCLUSION SUMMARY ----\n')
cat('N with BOTH TB and DM missing (to exclude):', n_both_missing, '\n')
cat('N with TB missing only (kept - has DM data):', n_tb_missing_only, '\n')
cat('N with DM missing only (kept - has TB data):', n_dm_missing_only, '\n')
cat('N with both present:', n_both_present, '\n')
cat('Check - these four should sum to total rows:',
    n_both_missing + n_tb_missing_only + n_dm_missing_only + n_both_present,
    '== ', nrow(nfhs_all), '\n')

# 5. apply the exclusion ----
nfhs_analytic <- nfhs_all |>
  filter(!(is.na(tb) & is.na(diabetes)))

n_analytic <- nrow(nfhs_analytic)
cat('\nFINAL ANALYTIC N:', n_analytic,
    '(', round(n_analytic / nrow(nfhs_all) * 100, 2), '% of full merged file )\n')

stopifnot(n_analytic == n_both_present + n_tb_missing_only + n_dm_missing_only)

# 6. checks ----
cat('\n-- TB x DM categories in analytic file --\n')
print(
  nfhs_analytic |>
    count(tb_dm_cat, useNA = 'always') |>
    mutate(pct = round(n / sum(n) * 100, 3))
)

cat('\n-- By sex in analytic file --\n')
print(
  nfhs_analytic |>
    group_by(sex) |>
    summarise(
      n         = n(),
      n_tb      = sum(tb == 1, na.rm = TRUE),
      pct_tb    = round(mean(tb == 1, na.rm = TRUE) * 100, 2),
      n_dm      = sum(diabetes == 1, na.rm = TRUE),
      pct_dm    = round(mean(diabetes == 1, na.rm = TRUE) * 100, 2),
      n_tb_dm   = sum(tb_dm_cat == 'TB + Diabetes', na.rm = TRUE),
      pct_tb_dm = round(mean(tb_dm_cat == 'TB + Diabetes', na.rm = TRUE) * 100, 3)
    )
)

# 7. save ----
saveRDS(nfhs_analytic, here('Clean_data','nfhs_analytic.rds'))
cat('\nSaved: nfhs_analytic.rds\n')
cat('Rows:', nrow(nfhs_analytic), '| Columns:', ncol(nfhs_analytic), '\n')

cat('\n', strrep('=', 55), '\n', sep = '')

# 8. numbers for the flow chart ----
cat('NUMBERS FOR FIGURE 1 (script 16 reads them from the saved files)\n')
cat(strrep('=', 55), '\n', sep = '')
cat('Women file (nfhs_women_clean.rds):  ', nrow(readRDS(here('Clean_data','nfhs_women_clean.rds'))), '\n')
cat('Men file (nfhs_men_clean.rds):      ', nrow(readRDS(here('Clean_data','nfhs_men_clean.rds'))), '\n')
cat('TB persons in household roster:     ', nrow(readRDS(here('Clean_data','tb_roster.rds'))), '\n')
cat('Total after merge (nfhs_merged_full):', nrow(nfhs_all), '\n')
cat('Excluded (both TB+DM missing):      ', n_both_missing, '\n')
cat('ANALYTIC N (nfhs_analytic):         ', n_analytic, '\n\n')

nfhs_analytic |>
  count(tb_dm_cat, useNA = 'always') |>
  mutate(label = paste0('  ', tb_dm_cat, ' (n=', n, ', ', round(n/sum(n)*100,2), '%)')) |>
  pull(label) |> cat(sep = '\n')

cat('\nScript 08 complete. nfhs_analytic.rds is ready for Table 1.\n')
