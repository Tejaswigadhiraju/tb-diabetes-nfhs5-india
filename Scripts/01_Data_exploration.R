# 01 - data exploration (optional)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# One-time script I used to find the variable names in the raw files. Not part of the analysis chain, results do not depend on it.
# reads : Raw_data_women, Raw_data_men, Raw_data_household (.DTA)
# writes: women_column_list.txt, men_column_list.txt (project root)

# 1. setup ----
library(here)
source(here('Scripts','00_Setup and package loading.R'))

# 2. file paths and column names (reads only 10 rows so it is quick) ----
file_path_women <- here('Raw_data_women','IAIR7EFL.DTA')
women_colnames <- read_dta(file_path_women, n_max = 10) %>% names()

file_path_men <- here('Raw_data_men','IAMR7EFL.DTA')
men_colnames <- read_dta(file_path_men, n_max = 10) %>% names()

file_path_household <- here('Raw_data_household','IAPR7EFL.DTA')

cat("First 20 NFHS-5 women column names:\n")

# 3. first 30 column names ----
print(head(women_colnames, 30))

cat("First 20 NFHS-5 men column names:\n")
print(head(men_colnames, 30))

# 4. search variable labels by keyword ----
# attr(.,'label') gives the readable description of each column
women_labels <- sapply(read_dta(file_path_women, n_max = 1), attr, "label")
grep("smok|bidi|tobacco|snuff|pipe|chew|cigaret|paan|gutk|hooka", women_labels,ignore.case=TRUE, value = TRUE)
grep("Tb|tuberc", women_labels,ignore.case=TRUE, value = TRUE)
grep("wealth", women_labels, ignore.case=TRUE, value=TRUE)
grep("education",women_labels, ignore.case=TRUE, value=TRUE)
grep("residence|region|urban|rural", women_labels, ignore.case=TRUE, value=TRUE)
grep("caste|scheduled|tribe", women_labels, ignore.case=TRUE, value=TRUE)
grep("religion", women_labels, ignore.case=TRUE, value=TRUE)
grep("body mass|bmi", women_labels, ignore.case=TRUE, value=TRUE)
grep("alcohol", women_labels, ignore.case=TRUE, value=TRUE)
grep("diabet", women_labels, ignore.case=TRUE, value=TRUE)
grep("state", women_labels, ignore.case=TRUE, value=TRUE)
grep("line numbe", women_labels,ignore.case=TRUE, value = TRUE)
grep("height", women_labels,ignore.case=TRUE, value = TRUE)
grep("weight", women_labels,ignore.case=TRUE, value = TRUE)
grep("hiv", women_labels,ignore.case=TRUE, value = TRUE)
grep("gluco|sugar|hbA1", women_labels,ignore.case=TRUE, value = TRUE)

# men
men_labels <- sapply(read_dta(file_path_men, n_max = 1), attr, "label")
grep("Tb|tuberc", men_labels,ignore.case=TRUE, value = TRUE)
grep("smok|bidi|tobacco|snuff|pipe|chew|cigaret|paan|gutk|hooka", men_labels,ignore.case=TRUE, value = TRUE)
grep("diabet", men_labels,ignore.case=TRUE, value = TRUE)
grep("wealth", men_labels, ignore.case=TRUE, value=TRUE)
grep("education",men_labels, ignore.case=TRUE, value=TRUE)
grep("residence|region|urban|rural", men_labels, ignore.case=TRUE, value=TRUE)
grep("caste|scheduled|tribe", men_labels, ignore.case=TRUE, value=TRUE)
grep("religion", men_labels, ignore.case=TRUE, value=TRUE)
grep("body mass|bmi", men_labels, ignore.case=TRUE, value=TRUE)
grep("alcohol", men_labels, ignore.case=TRUE, value=TRUE)
grep("diabet", men_labels, ignore.case=TRUE, value=TRUE)
grep("state", men_labels, ignore.case=TRUE, value=TRUE)
grep("age", men_labels, ignore.case=TRUE, value=TRUE)
grep("line numbe", men_labels,ignore.case=TRUE, value = TRUE)
grep("height", men_labels,ignore.case=TRUE, value = TRUE)
grep("weight", men_labels,ignore.case=TRUE, value = TRUE)
grep("hiv", men_labels,ignore.case=TRUE, value = TRUE)
grep("gluco|sugar|hbA1", men_labels,ignore.case=TRUE, value = TRUE)

# household file
household_labels <- sapply(read_dta(file_path_household, n_max = 1), attr, "label")
household_colnames <- read_dta(file_path_household, n_max = 10) %>% names()
grep("Tb|tuberc", household_labels,ignore.case=TRUE, value = TRUE)
grep("line numbe", household_labels,ignore.case=TRUE, value = TRUE)
grep("age", household_labels,ignore.case=TRUE, value = TRUE)
grep("sex", household_labels,ignore.case=TRUE, value = TRUE)

# 5. save the column lists ----
writeLines(women_colnames, con = "women_column_list.txt")
writeLines(men_colnames, con = "men_column_list.txt")
