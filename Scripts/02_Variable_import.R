# 02 - choose and import variables
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Lists the variables to keep from the women, men and household (PR) files and reads only those columns.
# reads : the three .DTA files in Raw_data_*
# writes: objects in memory only: women_vars, men_vars, household_vars (scripts 03-05 need these)
# So run this before 03, 04 and 05 in the same R session.

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))

# 2. women: variables to keep ----
women_vars <- c(
  "caseid","v001","v002","v003","v005", "v021", "v022", "v023",

  "s728a",  # currently has diabetes

  "v463a", "v463b", "v463c","v463d",  # tobacco items a-d
  "v463e", "v463f","v463g","v463h",  # items e-h
  "v463i", "v463j","v463k","v463l",  # items i-l (betel quid, gutka, khaini, paan)
  "v463x",  # smoked/smokeless tobacco
  "v463z",  # does not use tobacco
  "v463aa", "v463ab",
  "v464",
  "s707",
  "s708",  # currently smokes bidis
  "s709",
  "s710",
  "s711",
  "s712f",  # paan with tobacco
  "s713",  # how often smokeless tobacco is used
  "s714",
  "s716",

  "v012", "v013",  # age, age group
  "v101", "v024" , "v102", "v025",  # region, state, urban/rural
  "v106", "v107",  # education
  "v130", "v131",  # religion, caste/tribe
  "s116",  # SC/ST/OBC
  "v190", "v191",  # wealth quintile, wealth score
  "v190a",
  "v191a",
  "s190s",
  "s191s" ,
  "s190us" ,
  "s191us",
  "s190rs",
  "s191rs" ,
  "v445",  # bmi x 100
  "s720"  # alcohol
)

# read only these columns, the file is big
nfhs_women_raw <- read_dta(here('Raw_data_women','IAIR7EFL.DTA'),
                           col_select = all_of(women_vars))

# 3. men: variables to keep ----
# no BMI and no khaini/paan-specific item in the men's file
men_vars <- c(
  "mcaseid","mv001","mv002", "mv003","mv005", "mv021", "mv022", "mv023",

  "sm627a",  # currently has diabetes

  "mv463a", "mv463b", "mv463c","mv463d",  # tobacco items a-d
  "mv463e", "mv463f","mv463g",
  "mv463x",
  "mv463z",
  "mv463aa", "mv463ab",
  "mv463ac",
  "mv463ad",
  "mv464",
  "mv464a",  # daily amounts by type (not used later)
  "mv464b",
  "mv464c",
  "mv464d" ,
  "mv464e" ,
  "mv464f" ,
  "mv464g" ,
  "mv464h",
  "mv464i" ,
  "mv464j" ,
  "mv464k" ,
  "mv484a" ,  # weekly amounts by type (not used later)
  "mv484b" ,
  "mv484c" ,
  "mv484d" ,
  "mv484e" ,
  "mv484f" ,
  "mv484g",
  "mv484h" ,
  "mv484i" ,
  "mv484j" ,
  "mv484k" ,
  "sm604" ,
  "sm606" ,
  "sm607" ,  # currently smokes bidis
  "sm608" ,
  "sm609" ,
  "sm611f",  # paan with tobacco
  "sm613" ,

  "mv012", "mv013",  # age, age group
  "mv101", "mv024" , "mv102", "mv025",  # region, state, urban/rural
  "mv106", "mv107",
  "mv130", "mv131",
  "sm118",  # SC/ST/OBC
  "mv190", "mv191",  # wealth quintile, wealth score
  "mv190a",
  "mv191a",
  "sm190s",
  "sm191s" ,
  "sm190us" ,
  "sm191us",
  "sm190rs",
  "sm191rs" ,
  "sm619"  # alcohol
)

nfhs_men_raw <- read_dta(here('Raw_data_men','IAMR7EFL.DTA'),
                           col_select = all_of(men_vars))

# 4. household member file: TB variables ----
household_vars <- c(
  "hhid", "hv001","hv002","hv003", "hv005", "hv021", "hv022", "hv023",
  "hv104",  # sex
  "hv105",  # age
  "hvidx",  # line number
  "sh28" ,  # any usual resident has TB
  "sh29a" ,  # line number of person 1 with TB
  "sh29b" ,
  "sh29c" ,
  "sh29d",  # person 4
  "sh29"
  )

nfhs_household_raw <- read_dta(here('Raw_data_household','IAPR7EFL.DTA'),
                         col_select = all_of(household_vars))

# 5. quick structure check ----
glimpse(nfhs_women_raw)
ncol(nfhs_women_raw)
nrow(nfhs_women_raw)
var_label(nfhs_women_raw)
var_label(nfhs_men_raw)
val_labels(nfhs_women_raw$s190s)
val_labels(nfhs_women_raw$v463f)
val_labels(nfhs_women_raw$v024)
val_labels(nfhs_women_raw$v025)
val_labels(nfhs_women_raw$s728a)
val_labels(nfhs_household_raw$sh29c)
