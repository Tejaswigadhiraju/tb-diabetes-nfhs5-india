# run_all - runs the whole pipeline in order, in ONE R session
# Scripts 03, 04 and 05 need objects made by script 02 (women_vars etc.) and
# script 12b needs objects made by script 12, so they must share a session.
#
# Needs the raw DHS files in place first, see data/README.md.
# Scripts 01 (exploration), 06, 19 and 20 (checks) are optional and not run here.
# Script 18 needs the logistf package: install.packages('logistf')

library(here)

# stop early, with a clear message, if a raw file is missing
raw_files <- c(
  here('Raw_data_women',     'IAIR7EFL.DTA'),
  here('Raw_data_men',      'IAMR7EFL.DTA'),
  here('Raw_data_household', 'IAPR7EFL.DTA'),
  here('Raw_data_household', 'india_states.geojson')
)
missing_files <- raw_files[!file.exists(raw_files)]
if (length(missing_files) > 0) {
  stop('Missing input file(s), see data/README.md:\n  ',
       paste(missing_files, collapse = '\n  '), call. = FALSE)
}

steps <- c(
  '02_Variable_import.R',
  '03_Recode_women.R',
  '04_Recode_men.R',
  '05_Recode_household_TB.R',
  '07_Merge_women_men_TB.R',
  '08_Analytic_dataset.R',
  '08b_Add_denormalised_weight.R',
  '09_Table1_sociodemographics.R',
  '10_Table2_prevalence_per100k.R',
  '11_State_prevalence_percent.R',
  '11b_State_prevalence_per100k.R',
  '11c_TableS1_state_table_with_CI.R',
  '12_Maps_TB_diabetes_state.R',
  '12b_Maps_per100k_and_cooccurrence.R',
  '13_Table3_Table4_regression.R',
  '14_Table5_concentration_index.R',
  '15_TableS4_S5_diabetes_by_wealth_residence.R',
  '16_Figure1_sample_flow.R',
  '18_Sensitivity_analyses.R'
)

start <- Sys.time()
for (s in steps) {
  cat('\n=====', s, '=====\n')
  source(here('Scripts', s), echo = FALSE)
}

# save the package versions used
writeLines(capture.output(sessioninfo::session_info()), here('session_info.txt'))
cat('\nAll done in', round(as.numeric(difftime(Sys.time(), start, units = 'mins')), 1), 'minutes.\n')
