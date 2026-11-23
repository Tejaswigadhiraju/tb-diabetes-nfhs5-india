# 08b - de-normalised weight (for sensitivity analysis only)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Women's and men's weights are each scaled to average 1 in their own file, so when the two files are stacked each man counts about as much as each woman although there are many more women interviewed. This script adds a second weight, wt_dn, that corrects for that. The main analysis still uses wt. wt_dn is used only in script 18.
# The correction multiplies each weight by (weighted number of eligible people of that sex) / (number interviewed of that sex), using the household member file. It is my approximation of the DHS de-normalising step and should be described that way.
# reads : Clean_data/nfhs_analytic.rds, raw household file (hv005, hv103, hv104, hv105)
# writes: Clean_data/nfhs_analytic_wtdn.rds (new name, nfhs_analytic.rds is not overwritten)
# Run after script 08.

# 1. setup ----
library(here); library(dplyr); library(haven)
source(here('Scripts','00_Setup and package loading.R'))
conflicts_prefer(dplyr::filter)

nfhs <- readRDS(here('Clean_data','nfhs_analytic.rds'))

# 2. household member file: weight, slept last night, sex, age ----
pr <- read_dta(here('Raw_data_household','IAPR7EFL.DTA'),
               col_select = c(hv005, hv103, hv104, hv105)) |>
  mutate(w = as.numeric(hv005) / 1000000,
         slept = as.numeric(hv103), sx = as.numeric(hv104), age = as.numeric(hv105))

# 3. weighted number of eligible people (slept in the household last night) ----
S_w <- pr |> filter(slept == 1, sx == 2, age >= 15, age <= 49) |> summarise(s = sum(w)) |> pull(s)
S_m <- pr |> filter(slept == 1, sx == 1, age >= 15, age <= 54) |> summarise(s = sum(w)) |> pull(s)

# 4. number interviewed and the scaling factor for each sex ----
n_w <- sum(nfhs$sex == 'Women')
n_m <- sum(nfhs$sex == 'Men')
f_w <- S_w / n_w
f_m <- S_m / n_m
cat('Eligible (weighted) women 15-49:', round(S_w), '| interviewed:', n_w, '| factor:', round(f_w, 3), '\n')
cat('Eligible (weighted) men 15-54  :', round(S_m), '| interviewed:', n_m, '| factor:', round(f_m, 3), '\n')
cat('Each man now counts', round(f_m / f_w, 2), 'times as much as each woman (relative to before)\n')

# 5. de-normalised weight ----
# original wt x factor for that sex, then rescaled to mean 1
nfhs <- nfhs |>
  mutate(wt_dn0 = wt * if_else(sex == 'Women', f_w, f_m),
         wt_dn  = wt_dn0 / mean(wt_dn0, na.rm = TRUE)) |>
  select(-wt_dn0)

# 6. checks ----
cat('\nShare of men in the weighted sample:  before =',
    round(100 * sum(nfhs$wt[nfhs$sex == 'Men']) / sum(nfhs$wt), 1), '%  |  after =',
    round(100 * sum(nfhs$wt_dn[nfhs$sex == 'Men']) / sum(nfhs$wt_dn), 1), '%\n')
stopifnot(!anyNA(nfhs$wt_dn), abs(mean(nfhs$wt_dn) - 1) < 1e-8)

# 7. save under a new name ----
saveRDS(nfhs, here('Clean_data', 'nfhs_analytic_wtdn.rds'))
cat('Saved nfhs_analytic_wtdn.rds with the new column wt_dn.\n')
