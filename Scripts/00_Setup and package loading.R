# 00 - setup and packages
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Loads the packages and sets global options. Every other script sources this file first, so there is no need to run it on its own.
# reads : nothing
# writes: nothing

# install once, then comment out
# install.packages(c('tidyverse', 'haven', 'labelled', 'survey', 'gtsummary', 'flextable',
#   'sf', 'patchwork', 'here', 'sessioninfo', 'conflicted', 'logistf'))
# logistf is not loaded here. Only script 18 uses it, and that script loads it itself.
#
# load packages (every session) ----
library(tidyverse)   # dplyr, tidyr, stringr, ggplot2, tibble, forcats
library(haven)       # read_dta()
library(labelled)    # val_labels(), var_label(), generate_dictionary()
library(survey)      # svydesign(), svyglm(), svyciprop()
library(gtsummary)   # Table 1, regression tables
library(flextable)   # Word tables
library(sf)          # maps
library(patchwork)   # combining plots
library(here)        # project-relative paths
library(sessioninfo) # session_info() at the end of run_all.R
library(conflicted)  # stops silently picking the wrong function when two packages share a name

# global options ----
options(scipen = 999)
options(digits = 4)

# a few NFHS-5 strata have only one PSU, 'adjust' stops the variance step from failing
options(survey.lonely.psu = "adjust")

# name clashes between packages: say which one is meant ----
conflicts_prefer(dplyr::filter, dplyr::select, dplyr::lag, dplyr::mutate)
conflicts_prefer(haven::as_factor, labelled::val_labels)

# check ----
cat('All packages loaded successfully \n')
cat('R version:', R.version$version.string, '\n')
