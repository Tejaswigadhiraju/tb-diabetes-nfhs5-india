# 13 - Tables 3 and 4 (and supplementary models)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Survey weighted logistic regression of TB on diabetes and tobacco use, the diabetes x tobacco interaction, and the diabetes odds ratio within each tobacco group.
# reads : Clean_data/nfhs_analytic.rds
# writes: Tables/Table3_FINAL_v4.docx/.rds, Table3_Supplementary_FullModel_v4.docx, Table4_FINAL_v4.docx/.rds, TableS3_Model4_full_v4.docx

# 1. setup ----
library(here); library(survey); library(gtsummary); library(dplyr); library(flextable)
source(here('Scripts','00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
dir.create(here('Tables'), showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data','nfhs_analytic.rds'))

nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

# 2. reference categories ----
# relevel() sets what each odds ratio is compared against
nfhs_design$variables <- nfhs_design$variables |>
  mutate(
    diabetes_f = factor(diabetes, levels = c(0,1), labels = c('No','Yes')),
    tobacco_status = relevel(tobacco_status, ref = 'Non-user'),
    residence       = relevel(residence, ref = 'Rural'),
    edu_cat         = relevel(edu_cat, ref = 'No education'),
    wealth_quintile = relevel(wealth_quintile, ref = 'Poorest'),
    sex             = relevel(sex, ref = 'Women'),
    age_group       = relevel(age_group, ref = '15-19')
  )

# 3. model 1: crude, diabetes only ----
model1 <- svyglm(tb ~ diabetes_f, design = nfhs_design, family = quasibinomial())
summary(model1)
exp(coef(model1))
exp(confint(model1))

# 4. model 2: crude, tobacco only ----
model2 <- svyglm(tb ~ tobacco_status, design = nfhs_design, family = quasibinomial())
summary(model2)
exp(coef(model2))
exp(confint(model2))

# 5. model 3: diabetes + tobacco + age, sex, residence, education, wealth ----
# state is not added (36 categories use up a lot of df), state variation is shown in table S1 / figure 2
model3 <- svyglm(
  tb ~ diabetes_f + tobacco_status + age_group + sex + residence + edu_cat + wealth_quintile,
  design = nfhs_design, family = quasibinomial()
)
summary(model3)
exp(coef(model3))
exp(confint(model3))

# 6. model 4: model 3 + diabetes x tobacco interaction ----
model4 <- svyglm(
  tb ~ diabetes_f * tobacco_status + age_group + sex + residence + edu_cat + wealth_quintile,
  design = nfhs_design, family = quasibinomial()
)
summary(model4)
exp(coef(model4))
exp(confint(model4))

# 7. number used in each model and design df ----
model_n <- data.frame(model = c('Model 1','Model 2','Model 3','Model 4'),
                      n_used = c(length(model1$y), length(model2$y), length(model3$y), length(model4$y)))
print(model_n)
cat('Design degrees of freedom (PSUs - strata):', degf(nfhs_design), '\n')

# 8. joint test for the interaction (design based F test) ----
interaction_test <- regTermTest(model4, ~diabetes_f:tobacco_status)
print(interaction_test)
cat('\nInteraction term joint p-value:', round(interaction_test$p, 4), '\n')

# unweighted counts behind the tobacco specific ORs
sparse_counts <- nfhs_design$variables |>
  dplyr::filter(!is.na(diabetes_f)) |>
  dplyr::count(tobacco_status, diabetes_f, tb)
print(tibble::as_tibble(sparse_counts), n = Inf)

# 9. table 3 ----
# bold_p is applied before tbl_merge (gtsummary 2.0 renames the p column after merging)
tbl1 <- tbl_regression(model1, exponentiate = TRUE,
                       label = list(diabetes_f ~ 'Diabetes')) |>
  bold_p(t = 0.05)

tbl2 <- tbl_regression(model2, exponentiate = TRUE,
                       label = list(tobacco_status ~ 'Tobacco use')) |>
  bold_p(t = 0.05)

tbl3 <- tbl_regression(
  model3, exponentiate = TRUE,
  include = c(diabetes_f, tobacco_status),
  label = list(diabetes_f ~ 'Diabetes', tobacco_status ~ 'Tobacco use')
) |>
  bold_p(t = 0.05)

table3 <- tbl_merge(
  tbls = list(tbl1, tbl2, tbl3),
  tab_spanner = c('**Crude (Diabetes only)**', '**Crude (Tobacco only)**', '**Mutually Adjusted**')
) |>
  modify_caption('Table 3. Crude and mutually adjusted associations of diabetes and tobacco use with tuberculosis, NFHS-5, India (2019\u201321)') |>
  modify_footnote(
    everything() ~ paste0(
      'Odds ratios (OR) with 95% confidence intervals and p-values from survey-weighted logistic regression (svyglm, quasibinomial family, survey package, R), accounting for NFHS-5 clustering, stratification, and sampling weights. ',
      'Reference categories: Diabetes = No; Tobacco use = Non-user. ',
      'The mutually adjusted model additionally controls for age group, sex, residence, education, and wealth quintile (full model available in Supplementary Table). ',
      'Bold values indicate p<0.05.'
    )
  )

table3
saveRDS(table3, here('Tables', 'Table3_FINAL_v4.rds'))
table3 |> as_flex_table() |> flextable::save_as_docx(path = here('Tables', 'Table3_FINAL_v4.docx'))

# 10. supplementary tables with readable labels ----
nice_labels <- list(diabetes_f ~ 'Diabetes', tobacco_status ~ 'Tobacco use',
                    age_group ~ 'Age group (years)', sex ~ 'Sex', residence ~ 'Residence',
                    edu_cat ~ 'Education', wealth_quintile ~ 'Household wealth quintile')
tbl3_full <- tbl_regression(model3, exponentiate = TRUE, label = nice_labels) |>
  modify_caption('Supplementary Table S2. Full mutually adjusted model (Model 3), including all covariates')
tbl3_full |> as_flex_table() |> flextable::save_as_docx(path = here('Tables', 'Table3_Supplementary_FullModel_v4.docx'))

# 11. table 4: diabetes OR within each tobacco group ----
# contrast = diabetes coefficient + its interaction term, variance from the covariance matrix
coef_names <- names(coef(model4))

make_contrast <- function(terms_to_sum) {
  v <- setNames(rep(0, length(coef_names)), coef_names)
  v[terms_to_sum] <- 1
  v
}

contrast_nonuser <- make_contrast('diabetes_fYes')
contrast_smoking <- make_contrast(c('diabetes_fYes', 'diabetes_fYes:tobacco_statusSmoking only'))
contrast_slt     <- make_contrast(c('diabetes_fYes', 'diabetes_fYes:tobacco_statusSLT only'))
contrast_dual    <- make_contrast(c('diabetes_fYes', 'diabetes_fYes:tobacco_statusDual use'))

result_nonuser <- svycontrast(model4, contrast_nonuser)
result_smoking <- svycontrast(model4, contrast_smoking)
result_slt     <- svycontrast(model4, contrast_slt)
result_dual    <- svycontrast(model4, contrast_dual)

d <- nfhs_design$variables
n_nonuser <- sum(d$tobacco_status == 'Non-user', na.rm = TRUE)
n_smoking <- sum(d$tobacco_status == 'Smoking only', na.rm = TRUE)
n_slt     <- sum(d$tobacco_status == 'SLT only', na.rm = TRUE)
n_dual    <- sum(d$tobacco_status == 'Dual use', na.rm = TRUE)

build_row <- function(result, group_label, n_group) {
  est <- as.numeric(result)
  se  <- sqrt(attr(result, 'var'))
  ci_low  <- exp(est - 1.96 * se)
  ci_high <- exp(est + 1.96 * se)
  sig_flag <- if (ci_low > 1 | ci_high < 1) '*' else ''
  data.frame(
    'Tobacco use category' = group_label,
    'n (unweighted)' = format(n_group, big.mark = ','),
    'Diabetes OR (95% CI)' = paste0(
      formatC(exp(est), format = 'f', digits = 2), sig_flag, ' (',
      formatC(ci_low,  format = 'f', digits = 2), '\u2013',
      formatC(ci_high, format = 'f', digits = 2), ')'
    ),
    check.names = FALSE
  )
}

table4_data <- rbind(
  build_row(result_nonuser, 'Non-user (reference)', n_nonuser),
  build_row(result_smoking, 'Smoking only', n_smoking),
  build_row(result_slt,     'SLT only', n_slt),
  build_row(result_dual,    'Dual use', n_dual)
)

print(table4_data)

# word table. the interaction p value in the footnote is taken from the F test above
ft4 <- flextable(table4_data)
ft4 <- theme_booktabs(ft4)
ft4 <- bold(ft4, part = 'header')
ft4 <- flextable::align(ft4, j = c('n (unweighted)', 'Diabetes OR (95% CI)'), align = 'center', part = 'body')
ft4 <- flextable::align(ft4, align = 'center', part = 'header')
ft4 <- set_caption(ft4, caption = 'Table 4. Association between diabetes and tuberculosis, stratified by tobacco use category (effect modification), NFHS-5, India (2019\u201321)')
ft4 <- add_footer_lines(ft4, values = paste0('Diabetes odds ratios calculated within each tobacco category from the interaction model. Joint interaction F-test: p=', formatC(interaction_test$p, format = 'f', digits = 4), '.'))
ft4 <- fontsize(ft4, size = 10, part = 'body')
ft4 <- fontsize(ft4, size = 10, part = 'header')
ft4 <- fontsize(ft4, size = 8, part = 'footer')
ft4 <- padding(ft4, padding = 4, part = 'all')
ft4 <- autofit(ft4)

ft4
saveRDS(table4_data, here('Tables', 'Table4_FINAL_v4.rds'))
save_as_docx(ft4, path = here('Tables', 'Table4_FINAL_v4.docx'))

cat('\nTables 3 and 4 saved: Tables/Table3_FINAL_v4.docx and Tables/Table4_FINAL_v4.docx.\n')
cat('Full Model 3 saved as Tables/Table3_Supplementary_FullModel_v4.docx.\n')

# 12. full model 4 output (supplementary) ----
tbl4_full <- tbl_regression(model4, exponentiate = TRUE, label = nice_labels) |>
  modify_caption('Supplementary Table S3. Full output of Model 4 (diabetes \u00d7 tobacco interaction model, including covariates)')

tbl4_full |> as_flex_table() |> flextable::save_as_docx(path = here('Tables', 'TableS3_Model4_full_v4.docx'))

cat('\nScript 13 complete.\n')
