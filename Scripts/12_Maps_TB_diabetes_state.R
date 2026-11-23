# 12 - state maps (Figure 2)
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Part 1 reads the state results from script 11, matches state names to the boundary file and draws the TB and diabetes maps (percent). Part 2 estimates TB + diabetes by state at full precision and draws the co-occurrence maps. The per 100,000 versions of the TB and diabetes maps are drawn in 12b.
# reads : Tables/table2_tb_by_state.rds, table2_dm_by_state.rds (script 11); Raw_data_household/india_states.geojson; Clean_data/nfhs_analytic.rds (part 2)
# writes: Figures/Figure2_TB_Diabetes_combined.png/.pdf; Tables/state_tbdm_per100k_fullprecision.rds; Figures/Figure_TBDM_cooccurrence_sqrt and _binned (.png/.pdf)
# Run before 12b and in the same session (12b uses the objects made here).

# part 1: TB and diabetes maps (percent) ----
# 1. load the state results from script 11
library(here)
library(sf)
library(ggplot2)
library(dplyr)
library(patchwork)

tb_by_state <- readRDS(here('Tables', 'table2_tb_by_state.rds'))
dm_by_state <- readRDS(here('Tables', 'table2_dm_by_state.rds'))
cat('Loaded tb_by_state (', nrow(tb_by_state), 'rows) and dm_by_state (',
    nrow(dm_by_state), 'rows)\n')

# 2. state boundaries ----
# downloaded once from geoBoundaries (ADM1), see data/README
india_map <- st_read(here('Raw_data_household', 'india_states.geojson'))

cat('\nShapefile loaded.', nrow(india_map), 'regions found.\n')
cat('Column names:\n')
print(names(india_map))

# change this if your file uses another column name
shapefile_name_col <- 'shapeName'

# 3. match state names between my data and the shapefile ----
cat('\n--- States in YOUR data ---\n')
print(sort(unique(tb_by_state$subgroup)))

cat('\n--- States in the SHAPEFILE ---\n')
print(sort(unique(india_map[[shapefile_name_col]])))

# helper: removes accents, makes lower case, & to and, drops punctuation
normalize_state <- function(x) {
  x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x <- tolower(x)
  x <- gsub('&', 'and', x)
  x <- gsub('[^a-z ]', '', x)
  x <- trimws(gsub('\\s+', ' ', x))
  x
}

# one manual fix
name_fixes <- c('nct of delhi' = 'delhi')

tb_by_state <- tb_by_state |>
  mutate(norm_name = normalize_state(subgroup),
         norm_name = if_else(norm_name %in% names(name_fixes), name_fixes[norm_name], norm_name))
dm_by_state <- dm_by_state |>
  mutate(norm_name = normalize_state(subgroup),
         norm_name = if_else(norm_name %in% names(name_fixes), name_fixes[norm_name], norm_name))

india_map <- india_map |>
  mutate(norm_name = normalize_state(.data[[shapefile_name_col]]))

# this must be empty
unmatched <- setdiff(tb_by_state$norm_name, india_map$norm_name)
print(unmatched)
stopifnot(length(unmatched) == 0)

# 4. TB map ----
# viridis palettes are colour blind friendly
map_data_tb <- india_map |> left_join(tb_by_state, by = 'norm_name')

map_data_tb <- st_transform(map_data_tb, crs = 24378)

fig2_tb <- ggplot(map_data_tb) +
  geom_sf(aes(fill = prevalence_pct), color = 'white', linewidth = 0.2) +
  scale_fill_viridis_c(
    option = 'magma', direction = -1, na.value = 'grey85',
    name = 'TB\nprevalence (%)'
  ) +
  labs(
    title = 'A. Tuberculosis',
    caption = 'Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries.'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0),
    legend.position = 'right'
  )

fig2_tb

# 5. diabetes map (different palette so the two maps are easy to tell apart) ----
map_data_dm <- india_map |> left_join(dm_by_state, by = 'norm_name')

map_data_dm <- st_transform(map_data_dm, crs = 24378)

fig2_dm <- ggplot(map_data_dm) +
  geom_sf(aes(fill = prevalence_pct), color = 'white', linewidth = 0.2) +
  scale_fill_viridis_c(
    option = 'viridis', direction = -1, na.value = 'grey85',
    name = 'Diabetes\nprevalence (%)'
  ) +
  labs(
    title = 'B. Diabetes',
    caption = 'Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries.'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0),
    legend.position = 'right'
  )

fig2_dm

dir.create(here('Figures'), showWarnings = FALSE)

# 6. combine and save ----
figure2_combined <- fig2_tb + fig2_dm + plot_layout(ncol = 2)
figure2_combined

ggsave(here('Figures', 'Figure2_TB_Diabetes_combined.png'), figure2_combined,
       width = 14, height = 8, dpi = 300)

ggsave(here('Figures', 'Figure2_TB_Diabetes_combined.pdf'), figure2_combined,
       width = 14, height = 8)

cat('\nDone. Figure 2 (percentage version) saved as PNG and PDF in the Figures folder.\n')

# part 2: co-occurrence map from full precision estimates ----
# 1. state estimates
library(here); library(survey); library(dplyr); library(sf); library(ggplot2)
source(here('Scripts','00_Setup and package loading.R'))
options(survey.lonely.psu = "adjust")
conflicts_prefer(dplyr::filter)
dir.create(here('Figures'), showWarnings = FALSE)
dir.create(here('Tables'),  showWarnings = FALSE)

nfhs_analytic <- readRDS(here('Clean_data','nfhs_analytic.rds'))
nfhs_design <- svydesign(ids = ~psu, strata = ~sample_strata, weights = ~wt,
                         data = nfhs_analytic, nest = TRUE)

nfhs_design$variables <- nfhs_design$variables |>
  mutate(tbdm = as.integer(tb_dm_cat == 'TB + Diabetes'))

states <- sort(unique(na.omit(nfhs_design$variables$state_name)))

# observed zero is not the same as a true zero, so zero cases is kept as its own class
state_est <- lapply(states, function(s) {
  sub    <- subset(nfhs_design, state_name == s)
  n      <- sum(!is.na(sub$variables$tbdm))
  cases  <- sum(sub$variables$tbdm == 1, na.rm = TRUE)
  if (cases == 0) {
    return(data.frame(state_name = s, n = n, cases = 0L,
                      per100k = 0, lo = NA_real_, hi = NA_real_))
  }
  est <- svyciprop(~I(tbdm == 1), sub, method = 'logit', na.rm = TRUE)
  ci  <- confint(est)
  data.frame(state_name = s, n = n, cases = cases,
             per100k = as.numeric(est) * 1e5,
             lo = ci[1] * 1e5, hi = ci[2] * 1e5)
}) |> bind_rows()

state_est <- state_est |>
  mutate(
# flag states with few cases
    sparse = cases < 10,
    class_note = case_when(cases == 0 ~ 'No case observed',
                           sparse     ~ 'Fewer than 10 cases',
                           TRUE       ~ 'Estimated')
  )

print(tibble::as_tibble(state_est |> arrange(desc(per100k))), n = Inf)
saveRDS(state_est, here('Tables','state_tbdm_per100k_fullprecision.rds'))

cat('\nStates with zero observed cases:', sum(state_est$cases == 0), '\n')
cat('States with <10 cases:           ', sum(state_est$sparse), '\n')

# 2. shapefile and name matching (same as part 1) ----
india_map <- st_read(here('Raw_data_household','india_states.geojson'), quiet = TRUE)
shapefile_name_col <- 'shapeName'

normalize_state <- function(x) {
  x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x <- tolower(x)
  x <- gsub('&', 'and', x)
  x <- gsub('[^a-z ]', '', x)
  x <- trimws(gsub('\\s+', ' ', x))
  x
}

name_fixes <- c('nct of delhi' = 'delhi')

state_est <- state_est |>
  mutate(norm_name = normalize_state(state_name),
         norm_name = if_else(norm_name %in% names(name_fixes),
                             unname(name_fixes[norm_name]), norm_name))
india_map <- india_map |>
  mutate(norm_name = normalize_state(.data[[shapefile_name_col]]))

unmatched <- setdiff(state_est$norm_name, india_map$norm_name)
cat('\nStates that will NOT match the map (must be empty):\n'); print(unmatched)
stopifnot(length(unmatched) == 0)

map_data <- india_map |>
  left_join(state_est, by = 'norm_name') |>
  st_transform(crs = 24378)

# 3. maps ----
cap <- paste0('Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries. ',
              'Survey-weighted TB + diabetes per 100,000 adults.\n',
              'Grey = no co-occurring case observed in the sample. Each estimate rests on fewer than 10 cases; descriptive only.')

base_theme <- theme_void() +
  theme(plot.title   = element_text(face = 'bold', size = 12, hjust = 0.5),
        plot.caption = element_text(size = 7),
        legend.position = 'right')

# version A: square root colour scale
map_a <- map_data |> mutate(fill_val = if_else(cases == 0, NA_real_, per100k))

fig_a <- ggplot(map_a) +
  geom_sf(aes(fill = fill_val), color = 'white', linewidth = 0.2) +
  scale_fill_viridis_c(option = 'magma', direction = -1, trans = 'sqrt',
                       breaks = c(5, 10, 25, 50, 100, 200),
                       na.value = 'grey85', name = 'TB + diabetes\nper 100,000\n(sqrt scale)') +
  labs(title = 'TB and diabetes co-occurrence by state/UT', caption = cap) +
  base_theme
fig_a

ggsave(here('Figures','Figure_TBDM_cooccurrence_sqrt.png'), fig_a, width = 8, height = 8, dpi = 300)
ggsave(here('Figures','Figure_TBDM_cooccurrence_sqrt.pdf'), fig_a, width = 8, height = 8)

# version B: binned scale, breaks chosen for small values
brks <- c(0, 5, 10, 20, 40, 100, Inf)
labs_b <- c('>0 to <5', '5 to <10', '10 to <20', '20 to <40', '40 to <100', '100 or more')

lvl_b <- c(labs_b, 'No case observed')

map_b <- map_data |>
  mutate(cls_chr = as.character(cut(per100k, breaks = brks, labels = labs_b,
                   include.lowest = FALSE, right = FALSE)),
         cls_chr = if_else(cases == 0, 'No case observed' , cls_chr),
         cls     = factor(cls_chr, levels = lvl_b))
pal_b <- setNames(c(viridisLite::magma(6, direction = -1), 'grey85'), lvl_b)

fig_b <- ggplot(map_b) +
  geom_sf(aes(fill = cls), color = 'white', linewidth = 0.2) +
  scale_fill_manual(values = pal_b, drop = FALSE, na.value = 'grey60',
                    name = 'TB + diabetes\nper 100,000') +
  labs(title = 'TB and diabetes co-occurrence by state/UT', caption = cap) +
  base_theme
fig_b

ggsave(here('Figures','Figure_TBDM_cooccurrence_binned.png'), fig_b, width = 8, height = 8, dpi = 300)
ggsave(here('Figures','Figure_TBDM_cooccurrence_binned.pdf'), fig_b, width = 8, height = 8)
