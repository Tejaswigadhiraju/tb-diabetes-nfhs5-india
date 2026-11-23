# 15 - Supplementary Tables S4 and S5
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Does the diabetes-TB association differ by household wealth (model 5) or by residence (model 6)? Same design, reference levels and covariates as script 13. State is not used as an effect modifier (36 levels would leave most cells with almost no TB + diabetes cases).
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/TableS4_Diabetes_by_Wealth.docx/.rds, TableS5_Diabetes_by_Residence.docx/.rds

# 1. setup ----
library(here); library(survey); library(dplyr); library(flextable)
source(here('Scripts','00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
conflicts_prefer(dplyr::filter)
dir.create(here('Tables'), showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data','nfhs_analytic.rds'))

nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

# 2. reference categories (same as script 13) ----
nfhs_design$variables <- nfhs_design$variables |>
  mutate(
    diabetes_f      = factor(diabetes, levels = c(0,1), labels = c('No','Yes')),
    tobacco_status  = relevel(tobacco_status, ref = 'Non-user'),
    residence       = relevel(residence, ref = 'Rural'),
    edu_cat         = relevel(edu_cat, ref = 'No education'),
    wealth_quintile = relevel(wealth_quintile, ref = 'Poorest'),
    sex             = relevel(sex, ref = 'Women'),
    age_group       = relevel(age_group, ref = '15-19')
  )

# 3. model 5: diabetes x wealth quintile ----
# wealth main effect comes in through the interaction term, tobacco stays as an additive covariate
model5 <- svyglm(
  tb ~ diabetes_f * wealth_quintile + tobacco_status + age_group + sex +
    residence + edu_cat,
  design = nfhs_design, family = quasibinomial()
)

test5 <- regTermTest(model5, ~diabetes_f:wealth_quintile)
print(test5)

# 4. model 6: diabetes x residence ----
model6 <- svyglm(
  tb ~ diabetes_f * residence + tobacco_status + age_group + sex +
    edu_cat + wealth_quintile,
  design = nfhs_design, family = quasibinomial()
)

test6 <- regTermTest(model6, ~diabetes_f:residence)
print(test6)

# 5. helpers ----
# same svycontrast method as script 13: diabetes coefficient + the matching interaction term
fmt_p <- function(p) {
  if (p < 0.001) '<0.001' else formatC(p, format = 'f', digits = 3)
}

stratum_table <- function(model, design, var, ref, label) {
  cn   <- names(coef(model))
  d    <- design$variables
  lvls <- levels(d[[var]])

  out <- lapply(lvls, function(l) {
    terms <- 'diabetes_fYes'
    if (l != ref) terms <- c(terms, paste0('diabetes_fYes:', var, l))
    stopifnot(all(terms %in% cn))

    v <- setNames(rep(0, length(cn)), cn)
    v[terms] <- 1
    res <- svycontrast(model, v)
    est <- as.numeric(res)
    se  <- sqrt(attr(res, 'var'))
    lo  <- exp(est - 1.96 * se)
    hi  <- exp(est + 1.96 * se)
    star <- if (lo > 1 | hi < 1) '*' else ''

    in_grp <- !is.na(d[[var]]) & d[[var]] == l
    data.frame(
      label = paste0(l, if (l == ref) ' (reference)' else ''),
      n     = format(sum(in_grp), big.mark = ','),
      n_dm  = format(sum(in_grp & d$diabetes_f == 'Yes', na.rm = TRUE), big.mark = ','),
      n_tbdm = format(sum(in_grp & d$diabetes_f == 'Yes' & d$tb == 1, na.rm = TRUE), big.mark = ','),
      or_ci = paste0(formatC(exp(est), format = 'f', digits = 2), star, ' (',
                     formatC(lo, format = 'f', digits = 2), '\u2013',
                     formatC(hi, format = 'f', digits = 2), ')'),
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, out)
  names(tab) <- c(label, 'n (unweighted)', 'n with diabetes',
                  'n with TB and diabetes', 'Diabetes OR (95% CI)')
  tab
}

make_flextable <- function(tab, caption, footer) {
  ft <- flextable(tab)
  ft <- theme_booktabs(ft)
  ft <- bold(ft, part = 'header')
  ft <- flextable::align(ft, j = 2:5, align = 'center', part = 'all')
  ft <- set_caption(ft, caption = caption)
  ft <- add_footer_lines(ft, values = footer)
  ft <- fontsize(ft, size = 10, part = 'body')
  ft <- fontsize(ft, size = 10, part = 'header')
  ft <- fontsize(ft, size = 8,  part = 'footer')
  ft <- padding(ft, padding = 4, part = 'all')
  autofit(ft)
}

# 6. table S4: diabetes OR within each wealth quintile ----
tab_s4 <- stratum_table(model5, nfhs_design, 'wealth_quintile', 'Poorest', 'Wealth quintile')
print(tab_s4)

ft_s4 <- make_flextable(
  tab_s4,
  'Supplementary Table S4. Association between diabetes and tuberculosis within each household wealth quintile, NFHS-5, India (2019\u201321)',
  paste0('Odds ratios from the survey-weighted interaction model (diabetes \u00d7 wealth quintile), adjusted for tobacco use, age group, sex, residence and education. ',
         'Joint interaction F-test p=', fmt_p(test5$p), '. * 95% CI excludes 1. ',
         'Cells with few TB-and-diabetes cases give imprecise estimates; read the intervals, not just the point estimates.')
)
saveRDS(tab_s4, here('Tables', 'TableS4_Diabetes_by_Wealth.rds'))
save_as_docx(ft_s4, path = here('Tables', 'TableS4_Diabetes_by_Wealth.docx'))

# 7. table S5: diabetes OR by residence ----
tab_s5 <- stratum_table(model6, nfhs_design, 'residence', 'Rural', 'Residence')
print(tab_s5)

ft_s5 <- make_flextable(
  tab_s5,
  'Supplementary Table S5. Association between diabetes and tuberculosis by place of residence, NFHS-5, India (2019\u201321)',
  paste0('Odds ratios from the survey-weighted interaction model (diabetes \u00d7 residence), adjusted for tobacco use, age group, sex, education and wealth quintile. ',
         'Interaction F-test p=', fmt_p(test6$p), '. * 95% CI excludes 1.')
)
saveRDS(tab_s5, here('Tables', 'TableS5_Diabetes_by_Residence.rds'))
save_as_docx(ft_s5, path = here('Tables', 'TableS5_Diabetes_by_Residence.docx'))

# 8. checks ----
# reference stratum OR should equal exp(diabetes_fYes). single digit case counts in some quintiles mean wide CIs, that is expected
cat('\nWealth interaction p =', fmt_p(test5$p), '| Residence interaction p =', fmt_p(test6$p), '\n')
cat('Reference-stratum check (wealth): ', round(exp(coef(model5)['diabetes_fYes']), 2), '\n')
cat('Reference-stratum check (residence): ', round(exp(coef(model6)['diabetes_fYes']), 2), '\n')
cat('\nScript 15 complete. Tables S4 and S5 saved to Tables/.\n')
