# 11c - Supplementary Table S1
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Final state table (per 100,000) with CIs for co-occurrence wherever there is at least one case. Nothing is recomputed, it only reads the files from 11b.
# reads : Tables/table2_*_by_state_PER100K_UNR.rds (from 11b)
# writes: Tables/TableS1_State_Level_PER100K_UNR_withCI.docx and .csv

# 1. read the saved state results ----
library(here); library(dplyr); library(flextable)

tb   <- readRDS(here('Tables', 'table2_tb_by_state_PER100K_UNR.rds'))
dm   <- readRDS(here('Tables', 'table2_dm_by_state_PER100K_UNR.rds'))
tbdm <- readRDS(here('Tables', 'table2_tbdm_by_state_PER100K_UNR.rds'))

# 2. formats and name fixes ----
fmt <- function(v, lo, hi) paste0(formatC(v, format = 'f', digits = 1), ' (',
                                  formatC(lo, format = 'f', digits = 1), '\u2013', formatC(hi, format = 'f', digits = 1), ')')
fmt_cooc <- function(v, lo, hi, cases)
  ifelse(cases == 0, '0 (no case observed)',
  ifelse(is.na(lo) | is.na(hi), paste0(formatC(v, format = 'f', digits = 1), ' (CI not estimable)'),
         fmt(v, lo, hi)))
fix_name <- function(x) { x <- sub('^Nct Of Delhi$', 'NCT of Delhi', x); gsub(' And ', ' and ', x) }

# 3. build the table, sorted by TB ----
tab <- tb |>
  transmute(`State/UT` = fix_name(subgroup), `n (unweighted)` = format(n_unweighted, big.mark = ','),
            `TB per 100,000 (95% CI)` = fmt(per100k, lo, hi), tb_sort = per100k) |>
  left_join(dm |> transmute(`State/UT` = fix_name(subgroup), `Diabetes per 100,000 (95% CI)` = fmt(per100k, lo, hi)),
            by = 'State/UT') |>
  left_join(tbdm |> transmute(`State/UT` = fix_name(subgroup),
                              `TB + diabetes per 100,000 (95% CI)` = fmt_cooc(per100k, lo, hi, cases),
                              `Cases (n)` = cases), by = 'State/UT') |>
  arrange(desc(tb_sort)) |> select(-tb_sort)

# 4. checks before using it ----
chk <- tbdm |> filter(cases > 0)
cat('States with >=1 case:', nrow(chk), '(expect 27); total cases:', sum(tbdm$cases), '(expect 62)\n')
cat('States whose CI is missing or does not bracket the estimate (should be none):\n')
print(chk |> filter(is.na(lo) | is.na(hi) | lo > per100k | hi < per100k) |> select(subgroup, cases, per100k, lo, hi))
cat('States with a very wide interval (upper/lower > 50), expected with 1-2 cases:\n')
print(chk |> filter(!is.na(lo), hi / lo > 50) |> select(subgroup, cases, per100k, lo, hi))

# 5. word table and csv ----
ft <- flextable(tab) |>
  theme_booktabs() |>
  set_caption(caption = 'Supplementary Table S1. State-level weighted prevalence of tuberculosis, diabetes, and their co-occurrence per 100,000 adults, NFHS-5, India (2019\u201321)') |>
  bold(part = 'header') |>
  flextable::align(align = 'center', part = 'header') |>
  fontsize(size = 8, part = 'all') |>
  padding(padding = 2, part = 'all') |>
  autofit() |>
  footnote(i = 1, j = 1, part = 'header', ref_symbols = '1', value = as_paragraph(
    'Values are weighted cases per 100,000 adults with 95% confidence intervals (design-based, logit-transformed), computed from unrounded proportions. Cases (n) is the unweighted number of adults with both TB and diabetes. Every state with co-occurrence had fewer than 10 such cases (range 1 to 8), so the co-occurrence intervals are unstable and the estimates are descriptive only; states with no case observed are shown as 0.'))
save_as_docx(ft, path = here('Tables', 'TableS1_State_Level_PER100K_UNR_withCI.docx'))
write.csv(tab, here('Tables', 'TableS1_State_Level_PER100K_UNR_withCI.csv'), row.names = FALSE)
cat('\nDone. Saved TableS1_State_Level_PER100K_UNR_withCI.docx/.csv\n')
print(tab, n = Inf)
