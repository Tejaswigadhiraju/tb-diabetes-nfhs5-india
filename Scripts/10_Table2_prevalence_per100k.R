# 10 - Table 2
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Weighted prevalence of TB, diabetes and TB + diabetes per 100,000 adults, overall and by subgroup. Estimates are kept unrounded and rounded only for display.
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/Table2_PER100K_UNR.rds/.csv, Table2_PER100K_UNR_list.rds, Table2_PER100K_UNR_publication.docx
# Self-contained. This replaces my earlier percentage version of Table 2.

# 1. setup ----
library(here); library(survey); library(dplyr); library(tibble); library(flextable)
source(here('Scripts', '00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
dir.create(here('Tables'), showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data', 'nfhs_analytic.rds'))

# keep only the columns needed, makes it faster (results are the same)
nfhs_analytic <- nfhs_analytic |> dplyr::select(dplyr::any_of(c('psu','sample_strata','wt','tb','diabetes','tb_dm_cat',
  'state_name','sex','age_group','residence','edu_cat','wealth_quintile','tobacco_status')))
# survey design (same as every other script)
nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

# 2. helper: one estimate, or one per level of a grouping variable ----
# svyciprop with logit CI keeps the interval above 0 for rare outcomes
# subgroups use design[idx, ], not filter(), so the variance stays correct
get_per100k <- function(design, outcome_expr, group_var = NULL) {
  formula_obj <- as.formula(paste0('~I(', outcome_expr, ')'))
  one <- function(d, label) {
    est <- tryCatch(svyciprop(formula_obj, d, method = 'logit', na.rm = TRUE),
                    error = function(e) NULL)
    if (is.null(est)) {
      return(tibble(subgroup = label, n_unweighted = nrow(d$variables),
                    per100k = NA_real_, lo = NA_real_, hi = NA_real_))
    }
    ci <- confint(est)
    tibble(subgroup = label, n_unweighted = nrow(d$variables),
           per100k = as.numeric(est) * 1e5,
           lo = ci[1] * 1e5, hi = ci[2] * 1e5)
  }
  if (is.null(group_var)) return(one(design, 'Overall'))
  levels_present <- na.omit(unique(design$variables[[group_var]]))
  bind_rows(lapply(levels_present, function(lvl) {
    idx <- design$variables[[group_var]] == lvl
    idx[is.na(idx)] <- FALSE
    one(design[idx, ], as.character(lvl))
  }))
}

# helper: overall + every stratifier
build_outcome <- function(expr, strat_vars) {
  strat_labels <- c(sex = 'Sex', age_group = 'Age group', residence = 'Residence',
                    edu_cat = 'Education', wealth_quintile = 'Wealth quintile',
                    tobacco_status = 'Tobacco status')
  out <- get_per100k(nfhs_design, expr) |> mutate(stratifier = 'Overall')
  for (v in strat_vars) {
    out <- bind_rows(out, get_per100k(nfhs_design, expr, v) |> mutate(stratifier = strat_labels[[v]]))
  }
  out
}

# 3. run TB, diabetes and co-occurrence ----
# education is left out for co-occurrence (too few cases)
all_strata   <- c('sex', 'age_group', 'residence', 'edu_cat', 'wealth_quintile', 'tobacco_status')
no_edu_strata <- setdiff(all_strata, 'edu_cat')

tb_100k   <- build_outcome('tb == 1', all_strata)
dm_100k   <- build_outcome('diabetes == 1', all_strata)
tbdm_100k <- build_outcome("tb_dm_cat == 'TB + Diabetes'", no_edu_strata)

# 4. display format, rounding to 1 decimal happens only here ----
fmt_100k <- function(val, lo, hi) {
  ifelse(is.na(val), 'N/A',
         paste0(formatC(val, format = 'f', digits = 1), ' (',
                formatC(lo,  format = 'f', digits = 1), '\u2013',
                formatC(hi,  format = 'f', digits = 1), ')'))
}

tb_wide   <- tb_100k   |> transmute(stratifier, subgroup, n = n_unweighted,
                                    `TB per 100,000 (95% CI)` = fmt_100k(per100k, lo, hi))
dm_wide   <- dm_100k   |> transmute(stratifier, subgroup,
                                    `Diabetes per 100,000 (95% CI)` = fmt_100k(per100k, lo, hi))
tbdm_wide <- tbdm_100k |> transmute(stratifier, subgroup,
                                    `TB + Diabetes per 100,000 (95% CI)` = fmt_100k(per100k, lo, hi))

# 5. join into one wide table ----
table2_per100k <- tb_wide |>
  left_join(dm_wide,   by = c('stratifier', 'subgroup')) |>
  left_join(tbdm_wide, by = c('stratifier', 'subgroup')) |>
  mutate(
    stratifier = factor(stratifier, levels = c('Overall', 'Sex', 'Age group', 'Residence',
                                               'Education', 'Wealth quintile', 'Tobacco status')),
    subgroup = factor(subgroup, levels = c(
      'Overall', 'Women', 'Men',
      '15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54',
      'Rural','Urban',
      'No education','Primary','Secondary','Higher',
      'Poorest','Poorer','Middle','Richer','Richest',
      'Non-user','Smoking only','SLT only','Dual use'))
  ) |>
  arrange(stratifier, subgroup) |>
  rename(Stratifier = stratifier, Subgroup = subgroup, `n (unweighted)` = n)

# 6. save ----
saveRDS(list(tb = tb_100k, dm = dm_100k, tbdm = tbdm_100k), here('Tables', 'Table2_PER100K_UNR_list.rds'))
saveRDS(table2_per100k, here('Tables', 'Table2_PER100K_UNR.rds'))
write.csv(table2_per100k, here('Tables', 'Table2_PER100K_UNR.csv'), row.names = FALSE)

# 7. word table ----
ft_100k <- flextable(table2_per100k) |>
  merge_v(j = 'Stratifier') |>
  theme_booktabs() |>
  set_caption(caption = 'Table 2 (per 100,000). Weighted prevalence of tuberculosis, diabetes, and their co-occurrence per 100,000 population, by subgroup, NFHS-5, India (2019\u201321)') |>
  bold(part = 'header') |>
  bold(i = ~ Subgroup == 'Overall', bold = TRUE) |>
  flextable::align(align = 'center', part = 'header') |>
  flextable::align(j = c('n (unweighted)', 'TB per 100,000 (95% CI)',
                         'Diabetes per 100,000 (95% CI)',
                         'TB + Diabetes per 100,000 (95% CI)'), align = 'center', part = 'body') |>
  valign(valign = 'top', part = 'body') |>
  fontsize(size = 9, part = 'all') |>
  padding(padding = 3, part = 'all') |>
  autofit() |>
  footnote(
    i = 1, j = 1, part = 'header',
    value = as_paragraph(
      'Values expressed as weighted cases per 100,000 population, with 95% confidence intervals, estimated using design-based, logit-transformed methods (svyciprop, survey package, R) and converted to the per 100,000 scale from unrounded proportions. This scale follows standard tuberculosis epidemiology convention (e.g. WHO Global TB Report). n reflects unweighted respondent counts for the TB estimate; diabetes and co-occurrence denominators are marginally smaller due to a small amount of item non-response (~1% \u201cdon\u2019t know\u201d for diabetes). N/A indicates an estimate could not be computed for that cell. Co-occurrence estimates rest on few cases (62 in total) and are imprecise.'
    ),
    ref_symbols = '1'
  )
save_as_docx(ft_100k, path = here('Tables', 'Table2_PER100K_UNR_publication.docx'))

cat('\nDone. Unrounded per-100,000 Table 2 saved to Tables/Table2_PER100K_UNR_publication.docx\n')
print(table2_per100k, n = Inf)

# quick check on the overall TB row
print(tb_100k |> dplyr::filter(stratifier == 'Overall'))
