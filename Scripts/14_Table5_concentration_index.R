# 14 - Table 5: concentration index
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Wealth related inequality in TB, diabetes and co-occurrence using the convenient regression method (O'Donnell et al. 2008) with design based SEs and the Wagstaff correction.
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/Table5_FINAL.rds, Table5_FINAL.docx

# 1. setup ----
library(here); library(survey); library(dplyr); library(flextable)
source(here('Scripts','00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
dir.create(here('Tables'), showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data','nfhs_analytic.rds'))

conflicts_prefer(dplyr::filter)

# 2. weighted fractional wealth rank ----
# rank = (weight of everyone poorer + half own weight) / total weight
# uses the continuous wealth score, quintiles are too coarse
nfhs_analytic <- nfhs_analytic |>
  filter(!is.na(wealth_score), !is.na(wt)) |>
  arrange(wealth_score) |>
  mutate(
    cum_wt_before = cumsum(wt) - wt,
    total_wt      = sum(wt),
    frac_rank     = (cum_wt_before + wt/2) / total_wt
  )

cat('Fractional rank built. Range:', round(min(nfhs_analytic$frac_rank), 4),
    'to', round(max(nfhs_analytic$frac_rank), 4), '(should span close to 0 to 1)\n')

nfhs_analytic <- nfhs_analytic |>
# co-occurrence as 0/1
  mutate(tb_dm_binary = ifelse(tb_dm_cat == 'TB + Diabetes', 1L, 0L))

nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

# 3. concentration index function ----
calculate_concentration_index <- function(design, outcome_var) {

  d <- design$variables
  y <- d[[outcome_var]]
  valid <- !is.na(y)
  d_valid <- d[valid, ]

  sub_design <- design[valid, ]

# mean of the outcome
  mu <- svymean(as.formula(paste0('~', outcome_var)), sub_design, na.rm = TRUE)
  mu_val <- as.numeric(mu)

# variance of the rank
  var_rank <- as.numeric(svyvar(~frac_rank, sub_design, na.rm = TRUE))

# transformed outcome, the slope on rank is the concentration index
  sub_design$variables$y_star <- (2 * var_rank * d_valid[[outcome_var]]) / mu_val

  model <- svyglm(y_star ~ frac_rank, design = sub_design, family = gaussian())

  ci_estimate <- coef(model)['frac_rank']
  ci_ci       <- confint(model)['frac_rank', ]

# Wagstaff correction for binary outcomes: divide by (1 - mean)
  ci_corrected     <- ci_estimate / (1 - mu_val)
  ci_ci_corrected  <- ci_ci / (1 - mu_val)

  tibble::tibble(
    outcome = outcome_var,
    mean_prevalence_pct = round(mu_val * 100, 3),
    raw_CI = round(ci_estimate, 4),
    corrected_CI = round(ci_corrected, 4),
    ci_low = round(ci_ci_corrected[1], 4),
    ci_high = round(ci_ci_corrected[2], 4)
  )
}

# 4. run for TB, diabetes and co-occurrence ----
ci_tb    <- calculate_concentration_index(nfhs_design, 'tb')
ci_dm    <- calculate_concentration_index(nfhs_design, 'diabetes')
ci_tbdm  <- calculate_concentration_index(nfhs_design, 'tb_dm_binary')

print(ci_tb)
print(ci_dm)
print(ci_tbdm)

# 5. sanity check on the direction ----
cat('\nSanity check: TB corrected CI should be NEGATIVE (pro-poor, matching',
    'the Table 2 pattern of highest TB in the poorest quintile):', ci_tb$corrected_CI, '\n')
cat('Sanity check: Diabetes corrected CI should be POSITIVE (pro-rich,',
    'matching the Table 2 pattern of highest diabetes in the richest quintile):', ci_dm$corrected_CI, '\n')

# 6. table 5 ----
table5_data <- bind_rows(ci_tb, ci_dm, ci_tbdm) |>
  mutate(
    outcome_label = case_when(
      outcome == 'tb' ~ 'Tuberculosis',
      outcome == 'diabetes' ~ 'Diabetes',
      outcome == 'tb_dm_binary' ~ 'TB + Diabetes co-occurrence'
    ),
    direction = case_when(
      ci_low > 0 ~ 'Concentrated among the wealthy',
      ci_high < 0 ~ 'Concentrated among the poor',
      TRUE ~ 'No significant inequality detected'
    ),
    'Concentration Index (95% CI)' = paste0(
      formatC(corrected_CI, format = 'f', digits = 3), ' (',
      formatC(ci_low, format = 'f', digits = 3), ', ',
      formatC(ci_high, format = 'f', digits = 3), ')'
    )
  ) |>
  select(Outcome = outcome_label, 'Concentration Index (95% CI)', 'Direction of inequality' = direction)

print(table5_data)

ft5 <- flextable(table5_data)
ft5 <- theme_booktabs(ft5)
ft5 <- bold(ft5, part = 'header')
ft5 <- set_caption(ft5, caption = 'Table 5. Socioeconomic-related inequality in tuberculosis, diabetes, and their co-occurrence, NFHS-5, India (2019-21)')
ft5 <- fontsize(ft5, size = 10, part = 'all')
ft5 <- padding(ft5, padding = 4, part = 'all')
ft5 <- autofit(ft5)
ft5 <- add_footer_lines(ft5, values = 'Concentration index calculated using the convenient regression method (O\u2019Donnell et al., 2008), ranked by continuous household wealth score, with design-based standard errors (svyglm, survey package, R). Values are Wagstaff-corrected for bounded/binary outcomes. Negative values indicate concentration among poorer households; positive values indicate concentration among wealthier households. A confidence interval crossing zero indicates no statistically significant inequality.')
ft5 <- fontsize(ft5, size = 8, part = 'footer')

ft5

saveRDS(table5_data, here('Tables', 'Table5_FINAL.rds'))
save_as_docx(ft5, path = here('Tables', 'Table5_FINAL.docx'))

cat('\nDone. Table 5 saved to Tables/Table5_FINAL.docx\n')
