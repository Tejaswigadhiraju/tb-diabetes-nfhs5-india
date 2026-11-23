# 20 - consistency checks and numbers for the Results text (optional)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Checks that the unrounded numbers are consistent and prints every per 100,000 figure used in the Results. Each check prints PASS or CHECK, a CHECK is not always an error. Changes no table.
# reads : Clean_data/nfhs_analytic.rds, outputs of scripts 10, 11b, 13
# writes: Tables/Results_text_numbers_per100k.csv
# Run last. The expected values typed into the checks (sample sizes, 62 cases and so on) are for my results and must be updated if the data change.

# 1. setup ----
library(here); library(dplyr); library(tidyr); library(tibble)
# helper: prints PASS or CHECK
chk <- function(label, ok, note = '') cat(sprintf('%-6s %s %s\n', if (isTRUE(ok)) 'PASS' else 'CHECK', label, if (isTRUE(ok)) '' else paste0('  <- ', note)))

a <- readRDS(here('Clean_data','nfhs_analytic.rds'))
WT_USED <- 'wt'

# 2. sample counts ----
cat('\n-- 1. SAMPLE --\n')
chk('Analytic N = 825,954', nrow(a) == 825954, paste('got', nrow(a)))
chk('Women = 724,115', sum(a$sex == 'Women') == 724115)
chk('Men = 101,839',   sum(a$sex == 'Men') == 101839)
chk('Diabetes missing = 9,343', sum(is.na(a$diabetes)) == 9343, paste('got', sum(is.na(a$diabetes))))
cases <- a |> dplyr::filter(tb_dm_cat == 'TB + Diabetes')
chk('TB+diabetes cases = 62', nrow(cases) == 62, paste('got', nrow(cases)))
chk('Rural 40 / urban 22', identical(as.integer(table(cases$residence)[c('Rural','Urban')]), c(40L, 22L)))
chk('Wealth quintile split 15/11/15/12/9',
    identical(as.integer(table(cases$wealth_quintile)[c('Poorest','Poorer','Middle','Richer','Richest')]), c(15L,11L,15L,12L,9L)))
chk('Respondents with diabetes = 14,294', sum(a$diabetes == 1, na.rm = TRUE) == 14294, paste('got', sum(a$diabetes == 1, na.rm = TRUE)))
chk('Dual users = 7,657 (839 women + 6,818 men)', sum(a$tobacco_status == 'Dual use') == 7657)

# 3. table 2 (unrounded) ----
cat('\n-- 2. TABLE 2 (unrounded) --\n')
u <- readRDS(here('Tables','Table2_PER100K_UNR_list.rds'))
for (nm in names(u)) {
  x <- u[[nm]]
  ok <- all(x$lo <= x$per100k & x$per100k <= x$hi, na.rm = TRUE)
  chk(paste0(nm, ': lower <= estimate <= upper in every row'), ok)
  chk(paste0(nm, ': no estimate is a multiple of 10 for all rows (rounding gone)'),
      !all(abs(x$per100k %% 10) < 1e-6, na.rm = TRUE))
}
chk('Overall: TB < diabetes', u$tb$per100k[u$tb$subgroup == 'Overall'] < u$dm$per100k[u$dm$subgroup == 'Overall'])
chk('Overall co-occurrence < TB and < diabetes',
    u$tbdm$per100k[u$tbdm$subgroup == 'Overall'] < u$tb$per100k[u$tb$subgroup == 'Overall'])
for (nm in c('tb','dm','tbdm')) {
  x <- u[[nm]]; o <- x$per100k[x$subgroup == 'Overall']
  w <- x$per100k[x$subgroup == 'Women']; m <- x$per100k[x$subgroup == 'Men']
  chk(paste0(nm, ': Overall lies between Women and Men'), o >= min(w, m) && o <= max(w, m))
}

# 4. new values vs the old rounded ones ----
# only works if the old files exist, otherwise skipped
cat('\n-- 3. NEW vs OLD ROUNDED VALUES (valid only if WT_USED = "wt") --\n')
old_ok <- file.exists(here('Tables','table2_tb_full.rds'))
if (old_ok && WT_USED == 'wt') {
  for (nm in c('tb','dm','tbdm')) {
    f <- here('Tables', paste0('table2_', nm, '_full.rds'))
    if (!file.exists(f)) next
    old <- readRDS(f)
    if (!all(c('subgroup','prevalence_pct') %in% names(old))) { cat('SKIP ', nm, '(old file has different columns)\n'); next }
    j <- inner_join(u[[nm]] |> select(subgroup, per100k), old |> select(subgroup, prevalence_pct), by = 'subgroup') |>
      mutate(diff = abs(per100k - prevalence_pct * 1000))
    chk(paste0(nm, ': ', nrow(j), ' matched rows, every new value within 5 per 100,000 of the old rounded one'),
        all(j$diff <= 5.0001), paste('largest difference', round(max(j$diff), 2)))
  }
} else cat('SKIPPED (old files not found, or the de-normalised weight is used so pooled rows are EXPECTED to differ)\n')

# 5. state level ----
cat('\n-- 4. STATE LEVEL --\n')
st <- readRDS(here('Tables','table2_tbdm_by_state_PER100K_UNR.rds'))
chk('36 states/UTs', nrow(st) == 36, paste('got', nrow(st)))
chk('State cases add up to 62', sum(st$cases) == 62, paste('got', sum(st$cases)))
chk('Co-occurrence seen in 27 states, none in 9', sum(st$cases > 0) == 27, paste('states with cases:', sum(st$cases > 0)))
chk('No state has more than 8 cases', max(st$cases) <= 8, paste('max', max(st$cases)))
chk('TB and diabetes state tables have 36 rows each',
    nrow(readRDS(here('Tables','table2_tb_by_state_PER100K_UNR.rds'))) == 36 && nrow(readRDS(here('Tables','table2_dm_by_state_PER100K_UNR.rds'))) == 36)

# 6. table 4 ----
cat('\n-- 5. TABLE 4 (expected under the weights supplied; will change only if you change weights) --\n')
t4 <- readRDS(here('Tables','Table4_FINAL_v4.rds')); print(t4)
exp4 <- c('1.74', '2.34', '2.27', '26.83')
chk('Table 4 diabetes ORs = 1.74 / 2.34 / 2.27 / 26.83',
    all(mapply(function(s, e) grepl(e, s, fixed = TRUE), t4[['Diabetes OR (95% CI)']], exp4)),
    'values differ from the Results text: update the text (expected only if weights changed)')

# 7. numbers for the results text ----
cat('\n-- 6. NUMBERS FOR THE RESULTS TEXT (per 100,000; whole numbers) --\n')
fmt <- function(x) sprintf('%s (%s to %s)', format(round(x$per100k), big.mark=','), format(round(x$lo), big.mark=','), format(round(x$hi), big.mark=','))
getrow <- function(nm, s) { x <- u[[nm]]; x[x$subgroup == s, ] }
want <- c('Overall','Women','Men','15-19','30-34','35-39','40-44','45-49','50-54','Rural','Urban',
          'No education','Primary','Secondary','Higher','Poorest','Poorer','Middle','Richer','Richest',
          'Non-user','Smoking only','SLT only','Dual use')
out <- bind_rows(lapply(want, function(s) tibble(
  subgroup = s,
  TB   = if (nrow(getrow('tb', s)))   fmt(getrow('tb', s))   else NA,
  DM   = if (nrow(getrow('dm', s)))   fmt(getrow('dm', s))   else NA,
  TBDM = if (nrow(getrow('tbdm', s))) fmt(getrow('tbdm', s)) else NA)))
print(out, n = Inf, width = Inf)
write.csv(out, here('Tables','Results_text_numbers_per100k.csv'), row.names = FALSE)
cat('\nDerived ratios used in the text:\n')
ov <- function(nm) getrow(nm, 'Overall')$per100k
cat('  diabetes / TB overall            :', round(ov('dm') / ov('tb'), 1), '(text says "roughly eleven times")\n')
cat('  TB 45-49 / 15-19                 :', round(getrow('tb','45-49')$per100k / getrow('tb','15-19')$per100k, 1), '(text says 2.8)\n')
cat('  diabetes 45-49 / 15-19           :', round(getrow('dm','45-49')$per100k / getrow('dm','15-19')$per100k, 1), '(text says "more than fifteen")\n')
cat('  diabetes urban / rural           :', round(getrow('dm','Urban')$per100k / getrow('dm','Rural')$per100k, 2), '(text says 1.6)\n')
exp_ind <- ov('tb') * ov('dm') / 1e5
cat('  expected co-occurrence if independent (per 100,000):', round(exp_ind, 2), '| observed:', round(ov('tbdm'), 2),
    '| ratio:', round(ov('tbdm') / exp_ind, 1), '(text says about three)\n')
cat('\nSaved Tables/Results_text_numbers_per100k.csv.\n')
