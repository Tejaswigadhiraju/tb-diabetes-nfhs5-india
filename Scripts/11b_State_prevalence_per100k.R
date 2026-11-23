# 11b - state level prevalence per 100,000 (unrounded)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Recomputes the state estimates at full precision and saves them for the maps and for Table S1. States with 0 cases are shown as 0, states with fewer than 10 cases are shown without a CI.
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/table2_tb_by_state_PER100K_UNR.rds, table2_dm_..., table2_tbdm_... (the Word version of the state table, Supplementary Table S1, is made in script 11c)
# Self-contained.

# 1. setup ----
library(here); library(survey); library(dplyr); library(tibble)
source(here('Scripts', '00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
dir.create(here('Tables'), showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data', 'nfhs_analytic.rds'))

# keep only the columns needed (faster, same results)
nfhs_analytic <- nfhs_analytic |> dplyr::select(dplyr::any_of(c('psu','sample_strata','wt','tb','diabetes','tb_dm_cat',
  'state_name','sex','age_group','residence','edu_cat','wealth_quintile','tobacco_status')))
nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

# 2. helper: estimate for every state ----
# cases = unweighted number of cases. 0 cases gives 0 with no CI
states <- sort(unique(na.omit(as.character(nfhs_design$variables$state_name))))
cat('States/UTs found:', length(states), '(expect 36)\n')

state_estimates <- function(expr) {
  formula_obj <- as.formula(paste0('~I(', expr, ')'))
  bind_rows(lapply(states, function(s) {
    sub    <- subset(nfhs_design, state_name == s)
    n      <- nrow(sub$variables)
    cases  <- sum(eval(parse(text = expr), envir = sub$variables), na.rm = TRUE)
    if (cases == 0) {
      return(tibble(subgroup = s, n_unweighted = n, cases = 0L,
                    per100k = 0, lo = NA_real_, hi = NA_real_))
    }
    est <- tryCatch(svyciprop(formula_obj, sub, method = 'logit', na.rm = TRUE),
                    error = function(e) NULL)
    if (is.null(est)) {
      return(tibble(subgroup = s, n_unweighted = n, cases = as.integer(cases),
                    per100k = NA_real_, lo = NA_real_, hi = NA_real_))
    }
    ci <- confint(est)
    tibble(subgroup = s, n_unweighted = n, cases = as.integer(cases),
           per100k = as.numeric(est) * 1e5, lo = ci[1] * 1e5, hi = ci[2] * 1e5)
  })) |>
    mutate(prevalence_pct = per100k / 1000)
}

# 3. run ----
tb_by_state_100k   <- state_estimates('tb == 1')
dm_by_state_100k   <- state_estimates('diabetes == 1')
tbdm_by_state_100k <- state_estimates("tb_dm_cat == 'TB + Diabetes'")

# 4. save unrounded files (maps and table S1 use these) ----
saveRDS(tb_by_state_100k,   here('Tables', 'table2_tb_by_state_PER100K_UNR.rds'))
saveRDS(dm_by_state_100k,   here('Tables', 'table2_dm_by_state_PER100K_UNR.rds'))
saveRDS(tbdm_by_state_100k, here('Tables', 'table2_tbdm_by_state_PER100K_UNR.rds'))

# 5. display formats (rounding only here) ----
fmt_100k <- function(val, lo, hi) {
  ifelse(is.na(val), 'N/A',
         paste0(formatC(val, format = 'f', digits = 1), ' (',
                formatC(lo,  format = 'f', digits = 1), '\u2013',
                formatC(hi,  format = 'f', digits = 1), ')'))
}
# co-occurrence: no CI when there are fewer than 10 cases
fmt_sparse <- function(val, lo, hi, cases) {
  ifelse(cases == 0, '0 (no case observed)',
  ifelse(is.na(val), 'N/A',
  ifelse(cases < 10,
         paste0(formatC(val, format = 'f', digits = 1), ' (CI not shown)'),
         paste0(formatC(val, format = 'f', digits = 1), ' (',
                formatC(lo,  format = 'f', digits = 1), '\u2013',
                formatC(hi,  format = 'f', digits = 1), ')'))))
}

# 6. state table ----
state_table_100k <- tb_by_state_100k |>
  transmute(`State/UT` = subgroup, `n (unweighted)` = format(n_unweighted, big.mark = ','),
            `TB per 100,000 (95% CI)` = fmt_100k(per100k, lo, hi), tb_sort = per100k) |>
  left_join(dm_by_state_100k |>
              transmute(`State/UT` = subgroup,
                        `Diabetes per 100,000 (95% CI)` = fmt_100k(per100k, lo, hi)),
            by = 'State/UT') |>
  left_join(tbdm_by_state_100k |>
              transmute(`State/UT` = subgroup,
                        `TB + diabetes per 100,000 (95% CI)` = fmt_sparse(per100k, lo, hi, cases),
                        `Cases (n)` = cases),
            by = 'State/UT') |>
  arrange(desc(tb_sort)) |>
  select(-tb_sort)

cat('\nCo-occurrence: states with 0 cases =', sum(tbdm_by_state_100k$cases == 0),
    '| with <10 cases =', sum(tbdm_by_state_100k$cases < 10),
    '| total cases =', sum(tbdm_by_state_100k$cases), '(expect 62)\n')

cat('\nDone. Unrounded state estimates saved to Tables/ (used by script 11c and the maps).\n')
print(state_table_100k, n = Inf)
