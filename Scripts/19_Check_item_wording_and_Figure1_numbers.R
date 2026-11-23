# 19 - check item wording and Figure 1 numbers (optional)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Prints the question wording stored in the Stata variable labels (for the Methods) and the Figure 1 numbers. Output is on the console only.
# reads : raw .DTA files, Clean_data/*.rds
# writes: nothing

# 1. variable labels = the question text in the Stata files ----
library(here); library(haven); library(dplyr)

lab <- function(path, vars) {
  d <- read_dta(path, n_max = 5)
  for (v in vars) if (v %in% names(d)) {
    cat(sprintf('%-9s | %s\n', v, attr(d[[v]], 'label')))
    print(attr(d[[v]], 'labels'))
    cat('\n')
  } else cat(v, ': NOT FOUND in this file\n\n')
}
cat('==== WOMEN (IAIR7EFL) ====\n')
lab(here('Raw_data_women','IAIR7EFL.DTA'), c('s728a','v463a','v463b','v463c','v463d','v463e','v463f','v463g','v463x','v463z','s720','s721'))
cat('==== MEN (IAMR7EFL) ====\n')
lab(here('Raw_data_men','IAMR7EFL.DTA'), c('sm627a','mv463a','mv463b','mv463c','mv463d','mv463e','mv463f','mv463g','mv463x','mv463z'))
cat('==== HOUSEHOLD MEMBERS (IAPR7EFL) ====\n')
lab(here('Raw_data_household','IAPR7EFL.DTA'), c('sh28','sh29a','sh29b','sh29c','sh29d'))

# 2. figure 1 numbers ----
a <- readRDS(here('Clean_data','nfhs_analytic.rds'))
cat('\n==== FIGURE 1 ====\n')
cat('Women interviewed :', sum(a$sex == 'Women'), '\n')
cat('Men interviewed   :', sum(a$sex == 'Men'), '\n')
cat('Merged total      :', nrow(readRDS(here('Clean_data','nfhs_merged_full.rds'))), '\n')
cat('Analytic N        :', nrow(a), '\n')
cat('Diabetes missing  :', sum(is.na(a$diabetes)), '\n')
cat('Complete-case N for models with diabetes:', sum(!is.na(a$diabetes)), '\n')
