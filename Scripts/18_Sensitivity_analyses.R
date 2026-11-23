# 18 - sensitivity analyses
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# A. Do results change with the de-normalised weight (wt_dn)?
# B. Do the main ORs hold within each sex, and as prevalence ratios?
# C. Is the large dual-use OR a small numbers problem (counts, Fisher, Firth)?
# D. Do prevalence CIs change with the Korn-Graubard (beta) method?
# E. (optional) additive interaction, RERI, for diabetes x any tobacco.
# reads : Clean_data/nfhs_analytic_wtdn.rds (from script 08b)
# writes: Tables/Sensitivity_*.csv
# Needs the logistf package (install.packages('logistf')).

# 1. setup ----
library(here); library(survey); library(dplyr); library(tidyr); library(tibble)
library(logistf)
source(here('Scripts','00_Setup and package loading.R'))
options(survey.lonely.psu = 'adjust')
conflicts_prefer(dplyr::filter)
dir.create(here('Tables'), showWarnings = FALSE)

# 2. load data (with wt_dn) ----
dat <- readRDS(here('Clean_data','nfhs_analytic_wtdn.rds'))
stopifnot('wt_dn' %in% names(dat))

# reference categories, and 'both' = TB + diabetes
dat <- dat |>
  mutate(
    diabetes_f      = factor(diabetes, levels = c(0,1), labels = c('No','Yes')),
    tobacco_status  = relevel(tobacco_status, ref = 'Non-user'),
    residence       = relevel(residence, ref = 'Rural'),
    edu_cat         = relevel(edu_cat, ref = 'No education'),
    wealth_quintile = relevel(wealth_quintile, ref = 'Poorest'),
    sex             = relevel(sex, ref = 'Women'),
    age_group       = relevel(age_group, ref = '15-19'),
    both = case_when(tb_dm_cat == 'TB + Diabetes' ~ 1L, !is.na(tb_dm_cat) ~ 0L)
  )

# helper: survey design with any weight
mk_design <- function(data, w) {
  svydesign(ids = ~psu, strata = ~sample_strata,
            weights = as.formula(paste0('~', w)), data = data, nest = TRUE)
}
WTS <- c(original = 'wt', denormalised = 'wt_dn')

# part A1: prevalence per 100,000 under both weights ----
get_prev <- function(des, var, method = 'logit') {
  r  <- svyciprop(as.formula(paste0('~', var)), des, method = method, na.rm = TRUE)
  ci <- confint(r)
  tibble(per100k = as.numeric(r) * 1e5, lo = ci[1] * 1e5, hi = ci[2] * 1e5)
}

prev_rows <- list()
for (wn in names(WTS)) {
  des <- mk_design(dat, WTS[[wn]])
  for (grp in c('All adults', 'Women', 'Men')) {
    d_use <- if (grp == 'All adults') des else subset(des, sex == grp)
    for (v in c('tb', 'diabetes', 'both')) {
      prev_rows[[length(prev_rows) + 1]] <-
        bind_cols(tibble(weights = wn, group = grp, outcome = v), get_prev(d_use, v))
    }
  }
}
prev_tab <- bind_rows(prev_rows)
prev_wide <- prev_tab |>
  pivot_wider(names_from = weights, values_from = c(per100k, lo, hi)) |>
  mutate(pct_change = round(100 * (per100k_denormalised - per100k_original) / per100k_original, 1))
cat('\n== A1. Prevalence per 100,000: original vs de-normalised weights ==\n')
print(prev_wide, n = Inf, width = Inf)
write.csv(prev_wide, here('Tables','Sensitivity_A1_prevalence_weights.csv'), row.names = FALSE)

# part A2: models 1, 3, 4, 5, 6 under both weights ----
covs <- 'age_group + sex + residence + edu_cat + wealth_quintile'
or_ci <- function(m, terms) {
  est <- exp(cbind(OR = coef(m), confint(m)))
  out <- est[terms, , drop = FALSE]
  tibble(term = rownames(out), OR = out[, 1], lo = out[, 2], hi = out[, 3])
}
make_contrast <- function(m, terms_to_sum) {
  v <- setNames(rep(0, length(coef(m))), names(coef(m)))
  v[terms_to_sum] <- 1
  v
}
or_from_contrast <- function(m, terms_to_sum) {
  r <- svycontrast(m, make_contrast(m, terms_to_sum))
  est <- as.numeric(r); se <- sqrt(attr(r, 'var'))
  tibble(OR = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se))
}

res_models <- list(); res_int <- list(); res_dual <- list()
for (wn in names(WTS)) {
  des <- mk_design(dat, WTS[[wn]])
  m1 <- svyglm(tb ~ diabetes_f, des, family = quasibinomial())
  m3 <- svyglm(as.formula(paste('tb ~ diabetes_f + tobacco_status +', covs)), des, family = quasibinomial())
  m4 <- svyglm(as.formula(paste('tb ~ diabetes_f * tobacco_status +', covs)), des, family = quasibinomial())
  m5 <- svyglm(as.formula(paste('tb ~ diabetes_f * wealth_quintile + tobacco_status + age_group + sex + residence + edu_cat')), des, family = quasibinomial())
  m6 <- svyglm(as.formula(paste('tb ~ diabetes_f * residence + tobacco_status + age_group + sex + edu_cat + wealth_quintile')), des, family = quasibinomial())

  res_models[[wn]] <- bind_rows(
    or_ci(m1, 'diabetes_fYes')                         |> mutate(model = 'Model 1 crude diabetes'),
    or_ci(m3, c('diabetes_fYes', 'tobacco_statusSmoking only', 'tobacco_statusSLT only', 'tobacco_statusDual use')) |>
      mutate(model = 'Model 3 mutually adjusted')) |> mutate(weights = wn)
  res_int[[wn]] <- tibble(weights = wn,
    p_diab_x_tobacco = regTermTest(m4, ~diabetes_f:tobacco_status)$p,
    p_diab_x_wealth  = regTermTest(m5, ~diabetes_f:wealth_quintile)$p,
    p_diab_x_resid   = regTermTest(m6, ~diabetes_f:residence)$p)
  res_dual[[wn]] <- bind_rows(
    or_from_contrast(m4, 'diabetes_fYes')                                         |> mutate(group = 'Non-user'),
    or_from_contrast(m4, c('diabetes_fYes','diabetes_fYes:tobacco_statusSmoking only')) |> mutate(group = 'Smoking only'),
    or_from_contrast(m4, c('diabetes_fYes','diabetes_fYes:tobacco_statusSLT only'))     |> mutate(group = 'SLT only'),
    or_from_contrast(m4, c('diabetes_fYes','diabetes_fYes:tobacco_statusDual use'))     |> mutate(group = 'Dual use')) |>
    mutate(weights = wn)
}
cat('\n== A2. Odds ratios: original vs de-normalised weights ==\n')
print(bind_rows(res_models) |> arrange(model, term, weights), n = Inf, width = Inf)
cat('\n-- Interaction p-values --\n');   print(bind_rows(res_int))
cat('\n-- Diabetes OR within tobacco category --\n'); print(bind_rows(res_dual), n = Inf)
write.csv(bind_rows(res_models), here('Tables','Sensitivity_A2_ORs_weights.csv'), row.names = FALSE)
write.csv(bind_rows(res_int),    here('Tables','Sensitivity_A2_interaction_p.csv'), row.names = FALSE)
write.csv(bind_rows(res_dual),   here('Tables','Sensitivity_A2_diabetes_OR_by_tobacco.csv'), row.names = FALSE)

# part A3: concentration index under both weights (same method as script 14) ----
ci_fun <- function(data, w, outcome) {
  d <- data |>
    filter(!is.na(wealth_score), !is.na(.data[[w]])) |>
    arrange(wealth_score) |>
    mutate(ww = .data[[w]], frac_rank = (cumsum(ww) - ww + ww / 2) / sum(ww))
  des <- mk_design(d, w)
  des <- des[!is.na(d[[outcome]]), ]
  mu  <- as.numeric(svymean(as.formula(paste0('~', outcome)), des, na.rm = TRUE))
  vr  <- as.numeric(svyvar(~frac_rank, des, na.rm = TRUE))
  des$variables$y_star <- 2 * vr * des$variables[[outcome]] / mu
  m   <- svyglm(y_star ~ frac_rank, design = des, family = gaussian())
  est <- coef(m)['frac_rank']; ci <- confint(m)['frac_rank', ]
  tibble(outcome = outcome, CI = est / (1 - mu), lo = ci[1] / (1 - mu), hi = ci[2] / (1 - mu))
}
ci_tab <- bind_rows(lapply(names(WTS), function(wn)
  bind_rows(lapply(c('tb', 'diabetes', 'both'), function(o) ci_fun(dat, WTS[[wn]], o))) |> mutate(weights = wn)))
cat('\n== A3. Concentration index: original vs de-normalised weights ==\n')
print(ci_tab |> mutate(across(c(CI, lo, hi), ~round(.x, 3))), n = Inf)
write.csv(ci_tab, here('Tables','Sensitivity_A3_concentration_index.csv'), row.names = FALSE)

# part B: sex-stratified model 3 and prevalence ratios ----
# original weights, they do not change the sex mix within a sex
des0 <- mk_design(dat, 'wt')
strat_f <- tb ~ diabetes_f + tobacco_status + age_group + residence + edu_cat + wealth_quintile
strat <- bind_rows(lapply(c('Women', 'Men'), function(s) {
  m <- svyglm(strat_f, subset(des0, sex == s), family = quasibinomial())
  or_ci(m, c('diabetes_fYes', 'tobacco_statusSmoking only', 'tobacco_statusSLT only', 'tobacco_statusDual use')) |>
    mutate(sex = s)
}))
cat('\n== B1. Model 3 within each sex ==\n'); print(strat, n = Inf)
write.csv(strat, here('Tables','Sensitivity_B1_sex_stratified.csv'), row.names = FALSE)

# quasi-Poisson with a log link gives prevalence ratios (modified Poisson, Zou 2004)
pr_m <- svyglm(as.formula(paste('tb ~ diabetes_f + tobacco_status +', covs)), des0, family = quasipoisson(link = 'log'))
pr_tab <- or_ci(pr_m, c('diabetes_fYes', 'tobacco_statusSmoking only', 'tobacco_statusSLT only', 'tobacco_statusDual use')) |>
  rename(PR = OR)
cat('\n== B2. Prevalence ratios (quasi-Poisson, log link) ==\n'); print(pr_tab)
write.csv(pr_tab, here('Tables','Sensitivity_B2_prevalence_ratios.csv'), row.names = FALSE)

# part C: is the dual-use OR a small numbers problem? ----
dd <- dat |> filter(!is.na(diabetes_f))
cat('\n== C1. Unweighted counts: TB by diabetes within each tobacco category ==\n')
print(dd |> count(tobacco_status, diabetes_f, tb) |>
        pivot_wider(names_from = tb, values_from = n, names_prefix = 'tb_', values_fill = 0), n = Inf)

dual <- dd |> filter(tobacco_status == 'Dual use') |> droplevels()
cat('\n== C2. Fisher exact test within dual users (unweighted, unadjusted) ==\n')
print(fisher.test(table(dual$diabetes_f, dual$tb)))

cat('\n== C3. Firth penalised logistic within dual users (unweighted, adjusted for age group and sex) ==\n')
# Firth penalised logistic (unweighted), compare with the survey-weighted OR
fm <- logistf(tb ~ diabetes_f + age_group + sex, data = dual)
print(summary(fm))

# part D: Korn-Graubard (beta) vs logit CIs ----
kg <- bind_rows(lapply(c('logit', 'beta'), function(mt)
  bind_rows(lapply(c('tb', 'diabetes', 'both'), function(v)
    bind_cols(tibble(method = mt, outcome = v), get_prev(des0, v, method = mt))))))
cat('\n== D1. Overall prevalence per 100,000: logit vs Korn-Graubard beta CI ==\n'); print(kg)

des_dual <- subset(des0, tobacco_status == 'Dual use')
des_smk  <- subset(des0, tobacco_status == 'Smoking only')
kg2 <- bind_rows(
  bind_cols(tibble(group = 'Dual use',     method = 'logit'), get_prev(des_dual, 'both', 'logit')),
  bind_cols(tibble(group = 'Dual use',     method = 'beta'),  get_prev(des_dual, 'both', 'beta')),
  bind_cols(tibble(group = 'Smoking only', method = 'logit'), get_prev(des_smk,  'both', 'logit')),
  bind_cols(tibble(group = 'Smoking only', method = 'beta'),  get_prev(des_smk,  'both', 'beta')))
cat('\n-- D2. Co-occurrence among tobacco groups (the sparsest estimates) --\n'); print(kg2)
write.csv(kg,  here('Tables','Sensitivity_D1_KG_overall.csv'), row.names = FALSE)
write.csv(kg2, here('Tables','Sensitivity_D2_KG_cooccurrence_tobacco.csv'), row.names = FALSE)

# part E (optional): additive interaction, RERI for diabetes x any tobacco ----
datE <- dat |>
  filter(!is.na(diabetes_f)) |>
  mutate(tob_any = tobacco_status != 'Non-user',
         dm_tob = factor(case_when(
           diabetes_f == 'No'  & !tob_any ~ 'Neither',
           diabetes_f == 'Yes' & !tob_any ~ 'DM_only',
           diabetes_f == 'No'  &  tob_any ~ 'Tob_only',
           diabetes_f == 'Yes' &  tob_any ~ 'Both'),
         levels = c('Neither', 'DM_only', 'Tob_only', 'Both')))
mE <- svyglm(tb ~ dm_tob + age_group + sex + residence + edu_cat + wealth_quintile,
             design = mk_design(datE, 'wt'), family = quasibinomial())
reri <- svycontrast(mE, quote(exp(dm_tobBoth) - exp(dm_tobDM_only) - exp(dm_tobTob_only) + 1))
r_est <- as.numeric(reri); r_se <- sqrt(attr(reri, 'var'))
cat('\n== E. RERI (OR scale, delta-method CI) ==\n')
cat('RERI =', round(r_est, 2), ' 95% CI', round(r_est - 1.96 * r_se, 2), 'to', round(r_est + 1.96 * r_se, 2), '\n')
cat('RERI > 0 and CI excluding 0 = more than additive joint effect; CI including 0 = no evidence either way.\n')

cat('\nScript 18 complete. Output tables are in Tables/ (Sensitivity_*.csv).\n')
