# Diabetes, tobacco use and household-reported tuberculosis in India (NFHS-5)

R code for my analysis of how diabetes and tobacco use are linked to tuberculosis (TB) among Indians aged 15 to 54 years, using the fifth National Family Health Survey (NFHS-5, 2019-21). The working title of the paper is *Independent and joint associations of diabetes and tobacco use with household-reported tuberculosis in India - Evidence from the National Family Health Survey - 5*. It is a manuscript in preparation.

The scripts are numbered in the order they run, so the analysis can be followed from the raw files to the final tables.

## The questions

India carries around 25% of the world's TB burden and also has a growing number of people with diabetes and tobacco use. Both diabetes and tobacco are established risk factors for TB. This analysis examines their independent and joint association, with tobacco split into smoking, smokeless and dual use, and addresses three questions:

1. What is the prevalence of TB, diabetes, and their co-occurrence among Indians?
2. What are the independent and joint relationships of diabetes and tobacco use with TB?
3. How do these associations vary across socioeconomic profiles?



**Objectives:**

1\. To estimate weighted national and state-level prevalence of TB, diabetes and their co-occurrence.

2\. To assess the independent associations of diabetes and tobacco use with TB using survey-weighted logistic regression.

3\. To evaluate the mutually adjusted and interaction effects of diabetes and tobacco use on TB.

4\. To determine socioeconomic inequalities using concentration indices and stratified analyses by wealth, education, residence, sex and state.



Additional analyses: Beyond these objectives, I also tested whether the diabetes-TB association differs by wealth and residence, calculated the odds ratio for diabetes within each tobacco category, and ran sensitivity analyses with rescaled weights, and the relative excess risk due to interaction (RERI).

## Data

NFHS-5 (2019-2021) is a nationally representative household survey conducted by the International Institute for Population Sciences, with the DHS Program as technical partner. The data are available on request, and the terms do not allow them to be shared. **No NFHS-5 data are in this repository.** [`data/README.md`](data/README.md) explains how to get them and where to put them.

Three files are used: the women's recode (women aged 15-49), the men's recode (men aged 15-54, interviewed in a subsample of households) and the household person recode.

* **TB** is reported by a household informant, who states whether any usual resident has TB and names those affected. It is therefore household-reported and not clinically confirmed, and it captures only TB known to the household. A person is classified as having TB if named, and as not having TB if the household answered the question and did not name them.
* **Diabetes** is the respondent's report of currently having diabetes. "Don't know" is treated as missing.
* **Tobacco use** is derived from individual items (smoking: cigarettes, bidis, pipe, cigars, hookah; smokeless: chewing tobacco, gutka, khaini, snuff, paan with tobacco) and grouped as non-user, smoking only, smokeless only or both.
* **Analytic sample -** Women and men were combined (825,954 adults: 724,115 women and 101,839 men). Only respondents missing both TB and diabetes were excluded. Regression models use complete cases (n = 816,611), because 9,343 respondents did not know their diabetes status.

## Methods in brief, and why

* **Survey design.** NFHS-5 is a two-stage stratified cluster sample, so every estimate uses the `survey` package with the primary sampling unit (`v021`), the stratum (`v022`) and the sampling weight (`v005 / 1,000,000`). Some strata have a single PSU, so `survey.lonely.psu` is set to `"adjust"`.
* **Prevalence.** Per 100,000 adults, with 95% confidence intervals from `svyciprop(method = "logit")`. The logit interval keeps the lower limit above zero for rare outcomes like TB. Values are kept unrounded until they are printed. Subgroups are estimated within the full design object (`design\\\[idx, ]`) as filtering the data could misstate the variance.
* **Regression.** Survey-weighted logistic regression (`svyglm`, quasibinomial). Model 1 has diabetes alone, model 2 has tobacco alone, model 3 has both plus age group, sex, residence, education and wealth quintile, and model 4 adds the diabetes by tobacco interaction. State is not in the models (36 categories, many with almost no TB), and state differences are shown in the maps and in Supplementary Table S1.
* **Interaction.** Tested with a design-based joint F test (`regTermTest`). The diabetes odds ratio within each tobacco group is the diabetes coefficient plus the matching interaction term, with its variance taken from the covariance matrix (`svycontrast`). Wealth and residence are examined the same way in script 15.
* **Inequality.** Concentration index by the convenient regression method (O'Donnell et al., 2008), with households ranked on the continuous wealth score and the Wagstaff correction for binary outcomes. A negative value means the outcome is concentrated among poorer households.
* **Sensitivity.** Script 18 repeats the main results with a second weight, within each sex, as prevalence ratios (quasi-Poisson), with Fisher and Firth methods for the sparse dual-use cell, with Korn-Graubard intervals, and adds a relative excess risk due to interaction (RERI) for diabetes with any tobacco.

## Limits I want to be upfront about

* **Weights.** In the DHS files the women's weights and the men's weights are each scaled to average one within their own file. When the two files are stacked, men count in proportion to their share of the interviews (about 12%), not their share of the population. The main analysis keeps this pooled weight (`wt`). Script 08b builds a rescaled version (`wt\\\_dn`), my own approximation of the DHS de-normalising step, and script 18 repeats the analysis with it. The prevalence estimates and the odds ratios move, so the main numbers should be read with this in mind.
* **Sparse cells.** Only 62 respondents have both TB and diabetes, and the diabetes odds ratio in dual tobacco users rests on very few cases. I treat those results as hypothesis-generating. Estimates by state for co-occurrence are descriptive only.
* **Measurement.** TB is reported by a household member, not by the person, and only captures TB the household knows about. Diabetes is self-reported, so undiagnosed diabetes is missed, probably more among poorer households.
* **Design.** The survey is cross-sectional, so the associations are not causal.

## Main results

These come from my run on the full data and are the numbers in the manuscript. The Word and CSV tables made by the scripts hold the rest.

* TB 177.6 per 100,000 adults (95% CI 164.3-192.0), diabetes 1,996.8 (1,941.5-2,053.6), and TB with diabetes 8.6 (6.0-12.4).
* Diabetes was associated with TB: crude odds ratio 2.49 (1.71-3.62), adjusted 2.15 (1.47-3.14). After mutual adjustment, tobacco use groups were not clearly associated with TB.
* The diabetes-TB association differed by tobacco use (interaction P = 0.0041), largest in dual users, but with a very wide interval (odds ratio 26.83, 95% CI 6.59-109.27).
* TB was concentrated among poorer households (concentration index -0.214, 95% CI -0.259 to -0.168) and diabetes among richer ones (0.194, 0.177 to 0.210).
* With the rescaled weights the adjusted diabetes odds ratio was 2.92 (1.62-5.25).

## How to run it

You need R (I tested the pipeline with 4.4.3) and RStudio is the easiest way to work.

1. Put the three raw files and the boundary file in place (see [`data/README.md`](data/README.md)).
2. Open `tb-diabetes-nfhs5-india.Rproj` in RStudio. The scripts use the `here` package, so paths are relative to this folder. Do not use `setwd()`.
3. Install the packages once:

```r
   install.packages(c('tidyverse', 'haven', 'labelled', 'survey', 'gtsummary', 'flextable',
                      'sf', 'patchwork', 'here', 'sessioninfo', 'conflicted', 'logistf'))
   ```

4. Run `Scripts/run\\\_all.R`. It stops with a clear message if a raw file is missing, then runs scripts 02 to 18 in order in one session and writes `session\\\_info.txt` with the exact package versions.

Script order, inputs and outputs are in [`Scripts/README.md`](Scripts/README.md).

### Trying it without the DHS data

`tests/make\\\_mock\\\_data.R` writes fake files with the same names and variables, so the code can be run end to end by someone who does not have access yet. The numbers mean nothing and many lines in script 20 will say CHECK. The script refuses to run if it finds real data files, and you should delete the `Raw\\\_data\\\_\\\*` folders afterwards.

```r
source('tests/make\\\_mock\\\_data.R')
source('Scripts/run\\\_all.R')
```

### If something goes wrong

* *"Missing input file(s)"*: a raw file is not where `run\\\_all.R` expects it. Check the folder and file names in `data/README.md`. The letter in `IAIR7EFL` can differ between DHS releases.
* *"object 'women\_vars' not found"*: script 02 was not run first in the same session. Use `run\\\_all.R`.
* *A package error mentioning `broom.helpers`, `cardx` or similar*: update `gtsummary` with its dependencies, `install.packages('gtsummary', dependencies = TRUE)`.
* *"\[conflicted] ... found in 2 packages"*: two packages share a function name. Add a line such as `conflicts\\\_prefer(dplyr::filter)` to script 00.
* *The maps stop with "unmatched"*: the state names in the boundary file do not match the NFHS state labels. Script 12 prints both lists, so the mismatch is easy to find.

## Folder structure

```
Scripts/     all R scripts in run order, run\\\_all.R, and a README
tests/       make\\\_mock\\\_data.R (fake data for a test run) and pre\\\_push\\\_check.R (checks before pushing)
data/        README on how to get the raw data
Raw\\\_data\\\_\\\*/  raw DHS files and boundary file (not uploaded, you add these)
Clean\\\_data/  cleaned files made by the scripts (not uploaded)
Tables/      tables made by the scripts (.docx, .csv)
Figures/     figures made by the scripts (.png, .pdf)
```

## Where each output comes from

|Output|Script|
|-|-|
|Table 1, sociodemographic profile by sex|09|
|Table 2, prevalence per 100,000 by subgroup|10|
|Supplementary Table S1, state-level prevalence|11b, 11c|
|Figure 1, sample flow|16|
|Figure 2, state maps of TB, diabetes and co-occurrence|12, 12b|
|Table 3, crude and adjusted odds ratios|13|
|Table 4, diabetes odds ratio by tobacco group|13|
|Table 5, concentration index|14|
|Supplementary Tables S2 and S3 (full models 3 and 4)|13|
|Supplementary Tables S4 and S5 (wealth and residence)|15|
|Sensitivity analyses|08b, 18|

## About

Gadhiraju Tejaswi, MPH, PHFI - Institute of Public Health Sciences (formerly known as Indian Institute of Public Health).

If you use the code, please cite this repository. The manuscript reference will go here once it is published.

## Data terms

The NFHS-5 data are not part of this repository and remain subject to the DHS Program terms of use.

