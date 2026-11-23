# 11 - state level prevalence (percent)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# State level TB, diabetes and co-occurrence as percentages, rounded to 2 decimals. Script 12 (maps) reads these files. The per 100,000 state tables are made at full precision in script 11b.
# reads : Clean_data/nfhs_analytic.rds, Raw_data_women (state labels)
# writes: Tables/table2_tb_by_state.rds, table2_dm_by_state.rds, table2_tbdm_by_state.rds and csv copies; also re-saves Clean_data/nfhs_analytic.rds

# 1. load ----
library(here); library(survey); library(dplyr); library(tibble)
library(haven); library(stringr)

source(here('Scripts', '00_Setup and package loading.R'))
dir.create(here('Tables'), showWarnings = FALSE)
options(survey.lonely.psu = "adjust")

nfhs_analytic <- readRDS(here('Clean_data', 'nfhs_analytic.rds'))
cat('STAGE 1 - Loaded data:', nrow(nfhs_analytic), 'rows\n')

# 2. check state names are text, not numeric codes ----
# state_name is already built in scripts 03/04, so the fix below is normally skipped
women_raw_state <- haven::read_dta(here('Raw_data_women', 'IAIR7EFL.DTA'), col_select = 'v024')
state_code_to_name <- labelled::val_labels(women_raw_state$v024)

cat('STAGE 2 - Number of states found:', length(state_code_to_name), '\n')
if (length(state_code_to_name) < 30) {
  stop('Expected ~36 states, got fewer. Stopping here. ',
       'Check that haven/labelled loaded correctly and try restarting R again before rerunning.')
}

if (is.numeric(nfhs_analytic$state_name) || all(!is.na(suppressWarnings(as.numeric(nfhs_analytic$state_name))))) {

state_lookup_df <- tibble(
  state_code = as.numeric(state_code_to_name),
  state_name_clean = stringr::str_to_title(names(state_code_to_name))
)

nfhs_analytic$state_name <- state_lookup_df$state_name_clean[
  match(as.numeric(as.character(nfhs_analytic$state_name)), state_lookup_df$state_code)
]

cat('STAGE 2 - state_name was numeric; corrected to real state names.\n')
} else {
  cat('STAGE 2 - state_name already contains real state names, as expected. No fix needed.\n')
}

n_missing_states <- sum(is.na(nfhs_analytic$state_name))
cat('STAGE 2 - People with no matching state name (should be 0):', n_missing_states, '\n')
stopifnot(n_missing_states == 0)

cat('STAGE 2 complete - state names fixed. Distinct states:',
    dplyr::n_distinct(nfhs_analytic$state_name), '\n')
# note: this re-saves the analytic file (no change if the fix was skipped)
saveRDS(nfhs_analytic, here('Clean_data', 'nfhs_analytic.rds'))

# 3. survey design ----
nfhs_design <- survey::svydesign(
  ids = ~psu, strata = ~sample_strata, weights = ~wt,
  data = nfhs_analytic, nest = TRUE
)
cat('STAGE 3 complete - survey design rebuilt.\n')

# 4. prevalence by state ----
# design[idx, ] keeps the full design for the variance (not filter)
get_prevalence_by_state <- function(design, outcome_expr) {
  states <- sort(unique(design$variables$state_name))
  results <- lapply(states, function(one_state) {
    idx <- design$variables$state_name == one_state
    idx[is.na(idx)] <- FALSE
    sub_design <- design[idx, ]

    est <- tryCatch(
      survey::svyciprop(as.formula(paste0('~I(', outcome_expr, ')')),
                        sub_design, method = 'logit', na.rm = TRUE),
      error = function(e) NULL
    )
    if (is.null(est)) {
      tibble(subgroup = one_state, n_unweighted = nrow(sub_design$variables),
             prevalence_pct = NA, ci_low_pct = NA, ci_high_pct = NA)
    } else {
      ci <- confint(est)
      tibble(subgroup = one_state, n_unweighted = nrow(sub_design$variables),
             prevalence_pct = round(as.numeric(est) * 100, 2),
             ci_low_pct = round(ci[1] * 100, 2), ci_high_pct = round(ci[2] * 100, 2))
    }
  })
  dplyr::bind_rows(results)
}

# TB and diabetes
cat('STAGE 4 - Calculating TB by state (takes about a minute)...\n')
tb_by_state <- get_prevalence_by_state(nfhs_design, 'tb == 1') |> arrange(desc(prevalence_pct))
cat('STAGE 4 - TB done. Rows:', nrow(tb_by_state), '(should be same as number of states)\n')
print(tb_by_state, n = Inf)

cat('\nSTAGE 4 - Calculating Diabetes by state...\n')
dm_by_state <- get_prevalence_by_state(nfhs_design, 'diabetes == 1') |> arrange(desc(prevalence_pct))
cat('STAGE 4 - Diabetes done. Rows:', nrow(dm_by_state), '\n')
print(dm_by_state, n = Inf)

# co-occurrence, with a flag for states with few cases
cat('STAGE 4b - Calculating TB+Diabetes co-occurrence by state...\n')
tbdm_by_state <- get_prevalence_by_state(nfhs_design, "tb_dm_cat == 'TB + Diabetes'") |>
  arrange(desc(prevalence_pct))
cat('STAGE 4b - Done. Rows:', nrow(tbdm_by_state), '\n')

tbdm_by_state <- tbdm_by_state |>
  mutate(
    implied_cases = round(n_unweighted * prevalence_pct / 100, 1),
    reliability = case_when(
      is.na(prevalence_pct) ~ 'Not estimable',
      implied_cases < 25     ~ 'Unstable (<25 cases) - interpret with caution',
      TRUE ~ 'Stable'
    )
  )

print(tbdm_by_state, n = Inf)

cat('\nStates with unstable co-occurrence estimates:',
    sum(tbdm_by_state$reliability != 'Stable'), 'of', nrow(tbdm_by_state), '\n')

saveRDS(tbdm_by_state, here('Tables', 'table2_tbdm_by_state.rds'))
write.csv(tbdm_by_state, here('Tables', 'datawrapper_TBDM_by_state.csv'), row.names = FALSE)

cat('STAGE 4b complete - saved.\n')

# 5. save (script 12 reads these) ----
saveRDS(tb_by_state, here('Tables', 'table2_tb_by_state.rds'))
saveRDS(dm_by_state, here('Tables', 'table2_dm_by_state.rds'))
write.csv(tb_by_state, here('Tables', 'datawrapper_TB_by_state.csv'), row.names = FALSE)
write.csv(dm_by_state, here('Tables', 'datawrapper_Diabetes_by_state.csv'), row.names = FALSE)

cat('\nSTAGE 5 complete - all files saved to the Tables folder.\n')
cat('Ready for the maps (script 12). The per 100,000 state tables are made in script 11b.\n')
