# Scripts

The scripts are numbered in the order they run. Each one loads `00_Setup and package loading.R` itself, so please keep that file name as it is.

The easiest way to run everything is `run_all.R`. It checks that the raw files are in place, then runs scripts 02 to 18 in one R session.

| Script | What it does | Main input | Main output |
|---|---|---|---|
| `00_Setup and package loading.R` | loads packages, sets options, settles name clashes between packages | - | - |
| `01_Data_exploration.R` | optional. Searches the variable labels in the raw files for keywords | raw .DTA files | `women_column_list.txt`, `men_column_list.txt` |
| `02_Variable_import.R` | lists the variables to keep from each raw file | raw .DTA files | `women_vars`, `men_vars`, `household_vars` (in memory) |
| `03_Recode_women.R` | recodes women's data: survey weight, demographics, diabetes, tobacco, alcohol, BMI | raw women's file | `Clean_data/nfhs_women_clean.rds` |
| `04_Recode_men.R` | same for men (no BMI in the men's file) | raw men's file | `Clean_data/nfhs_men_clean.rds` |
| `05_Recode_household_TB.R` | builds the TB variables from the household member file | raw household file | `tb_roster.rds`, `hh_tb_status.rds`, `hh_person_agesex.rds` |
| `06_Missingness_checks.R` | optional. Frequency tables of raw codes and checks of the recodes | objects from 03-05 | console only |
| `07_Merge_women_men_TB.R` | adds TB status to each person, stacks women and men | clean files from 03-05 | `nfhs_merged_full.rds` |
| `08_Analytic_dataset.R` | makes the four TB x diabetes groups and drops people missing both TB and diabetes | merged file | `nfhs_analytic.rds` |
| `08b_Add_denormalised_weight.R` | adds a second weight, `wt_dn`, used only for the sensitivity analysis | analytic file, raw household file | `nfhs_analytic_wtdn.rds` |
| `09_Table1_sociodemographics.R` | Table 1 | analytic file | `Tables/TABLE1_FINAL.docx` |
| `10_Table2_prevalence_per100k.R` | Table 2, prevalence per 100,000 by subgroup | analytic file | `Tables/Table2_PER100K_UNR_publication.docx` |
| `11_State_prevalence_percent.R` | state prevalence in percent (the first two maps use it) | analytic file, raw women's file (state labels) | `Tables/table2_*_by_state.rds` |
| `11b_State_prevalence_per100k.R` | state prevalence per 100,000, unrounded | analytic file | `Tables/table2_*_by_state_PER100K_UNR.rds` |
| `11c_TableS1_state_table_with_CI.R` | Supplementary Table S1 | output of 11b | `Tables/TableS1_State_Level_PER100K_UNR_withCI.docx` |
| `12_Maps_TB_diabetes_state.R` | TB and diabetes maps (percent), then the co-occurrence maps | output of 11, boundary file | `Figures/Figure2_TB_Diabetes_combined.png`, `Figure_TBDM_cooccurrence_*.png` |
| `12b_Maps_per100k_and_cooccurrence.R` | maps per 100,000 and the three-panel Figure 2 | output of 11b and 12 | `Figures/Figure2_TB_DM_TBDM_3panel_PER100K_UNR.png` |
| `13_Table3_Table4_regression.R` | logistic models, Tables 3 and 4, supplementary model tables | analytic file | `Tables/Table3_FINAL_v4.docx`, `Table4_FINAL_v4.docx`, `Table3_Supplementary_FullModel_v4.docx` (Table S2), `TableS3_Model4_full_v4.docx` |
| `14_Table5_concentration_index.R` | concentration index, Table 5 | analytic file | `Tables/Table5_FINAL.docx` |
| `15_TableS4_S5_diabetes_by_wealth_residence.R` | does the diabetes-TB odds ratio differ by wealth or residence? | analytic file | `Tables/TableS4_*.docx`, `TableS5_*.docx` |
| `16_Figure1_sample_flow.R` | Figure 1, sample flow diagram | clean files | `Figures/Figure1_Sample_Flow_FINAL.png` |
| `18_Sensitivity_analyses.R` | weights, sex-stratified models, prevalence ratios, Fisher and Firth, Korn-Graubard intervals, RERI | `nfhs_analytic_wtdn.rds` | `Tables/Sensitivity_*.csv` |
| `19_Check_item_wording_and_Figure1_numbers.R` | optional. Prints the question wording stored in the Stata labels and the Figure 1 numbers | raw files, clean files | console only |
| `20_Consistency_checks_and_results_numbers.R` | optional. PASS or CHECK for internal consistency, and the numbers quoted in the Results | tables from 10, 11b, 13 | `Tables/Results_text_numbers_per100k.csv` |

There is no script 17 (the number was dropped when the co-occurrence map moved into script 12).

## Things to know

- Scripts 03, 04 and 05 need `women_vars`, `men_vars` and `household_vars` from script 02, and 12b needs the map objects made in 12. Both pairs must run in the same R session, which `run_all.R` does.
- The main analysis uses `wt`, which is `v005 / 1,000,000` taken from the file the person came from. The second weight, `wt_dn`, is used only in script 18. The reason for having two is explained in the main README.
- Every model and table uses the same design: `ids = ~psu`, `strata = ~sample_strata`, `nest = TRUE`, and `survey.lonely.psu = "adjust"`.
- Subgroups and states are taken with `design[idx, ]`, not `filter()` on the data, so that the variance still reflects the full design.
- Nothing in the pipeline is random, so no seed is set.
- Script 20 compares the output to the numbers from my own run (analytic N, 62 co-occurring cases, and so on). With other data, or the synthetic data, many lines will say CHECK. That only means the numbers differ from mine.
- In script 13 the diabetes odds ratio within each tobacco group (Table 4) is the diabetes coefficient plus the matching interaction term, taken with `svycontrast()`. Its 95% interval is the estimate plus or minus 1.96 standard errors, on the log odds scale. Scripts 15 and 18 do the same.
