# 16 - Figure 1: sample flow diagram
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Draws the flow diagram. Every number is read from the saved data files, nothing is typed in. Includes the diabetes-missing step so the figure matches the regression sample.
# reads : Clean_data/nfhs_women_clean.rds, nfhs_men_clean.rds, nfhs_merged_full.rds, nfhs_analytic.rds
# writes: Figures/Figure1_Sample_Flow_FINAL.png/.pdf
# Does not source script 00, it loads its own packages.

# 1. numbers from the saved files ----
library(here); library(ggplot2); library(dplyr)

n_women_raw <- nrow(readRDS(here('Clean_data', 'nfhs_women_clean.rds')))
n_men_raw   <- nrow(readRDS(here('Clean_data', 'nfhs_men_clean.rds')))
n_merged    <- nrow(readRDS(here('Clean_data', 'nfhs_merged_full.rds')))
a           <- readRDS(here('Clean_data', 'nfhs_analytic.rds'))
n_analytic  <- nrow(a)
n_w <- sum(a$sex == 'Women'); n_m <- sum(a$sex == 'Men')
n_excl      <- n_merged - n_analytic
n_dm_miss   <- sum(is.na(a$diabetes))
n_dm_ok     <- n_analytic - n_dm_miss
cat('Women file:', n_women_raw, '| Men file:', n_men_raw, '| Merged:', n_merged, '| Analytic:', n_analytic,
    '| Excluded between merge and analytic:', n_excl, '| Diabetes missing:', n_dm_miss, '| With diabetes:', n_dm_ok, '\n')
stopifnot(n_excl >= 0)

# 2. boxes ----
f <- function(x) format(x, big.mark = ',')
boxes <- tibble::tribble(
  ~id, ~xmin, ~xmax, ~ymin, ~ymax, ~fill, ~label,
  'w',  0.0,  4.5, 20, 22, 'white',   paste0('NFHS-5 women\u2019s file\n(aged 15\u201349)\nN = ', f(n_women_raw)),
  'm',  5.5, 10.0, 20, 22, 'white',   paste0('NFHS-5 men\u2019s file\n(aged 15\u201354)\nN = ', f(n_men_raw)),
  'mg', 1.5,  8.5, 15.5, 18, 'white', paste0('Linked to household-member\nTB report\nN = ', f(n_merged)),
  'an', 1.0,  9.0, 11, 13.5, '#D9E8F5', paste0('Analytic sample\nN = ', f(n_analytic), '\n(women ', f(n_w), '; men ', f(n_m), ')'),
  'dm', 0.0,  4.5, 6.5, 9, '#D9E8F5',   paste0('Diabetes status available\n(models with diabetes)\nN = ', f(n_dm_ok)),
  'ms', 5.5, 10.0, 6.5, 9, 'white',     paste0('Diabetes status missing\nn = ', f(n_dm_miss), '\n(omitted from models with diabetes)')
)
if (n_excl > 0) boxes <- bind_rows(boxes, tibble::tibble(id = 'ex', xmin = 10.5, xmax = 15.5, ymin = 13.5, ymax = 16.5,
                    fill = 'white', label = paste0('Excluded\nn = ', f(n_excl))))
# arrows
ar <- function(x, xe, y, ye) annotate('segment', x = x, xend = xe, y = y, yend = ye,
        arrow = arrow(length = unit(0.2, 'cm'), type = 'closed'), linewidth = 0.4)

# 3. draw ----
fig1 <- ggplot() +
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = 'black', linewidth = 0.4, show.legend = FALSE) +
  scale_fill_identity() +
  geom_text(data = boxes, aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
            size = 3.2, lineheight = 0.95) +
  ar(2.25, 4.0, 20, 18) + ar(7.75, 6.0, 20, 18) +
  ar(5, 5, 15.5, 13.5) +
  ar(3.0, 2.25, 11, 9) + ar(7.0, 7.75, 11, 9) +
  { if (n_excl > 0) ar(8.5, 10.5, 16.75, 15) } +
  xlim(-0.5, 16) + ylim(6, 22.5) +
  theme_void() +
  labs(title = 'Figure 1. Sample selection flow diagram') +
  theme(plot.title = element_text(face = 'bold', size = 12, hjust = 0.5))
print(fig1)

# 4. save ----
dir.create(here('Figures'), showWarnings = FALSE)
ggsave(here('Figures', 'Figure1_Sample_Flow_FINAL.png'), fig1, width = 8, height = 9, dpi = 300)
ggsave(here('Figures', 'Figure1_Sample_Flow_FINAL.pdf'), fig1, width = 8, height = 9)
cat('\nDone. Saved Figures/Figure1_Sample_Flow_FINAL.png/.pdf. Check every box against Methods and Table 1.\n')
