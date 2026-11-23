# 12b - Figure 2 per 100,000 and co-occurrence map
# TB-diabetes co-occurrence in India, NFHS-5 (2019-21)
#
# Redraws the TB and diabetes maps per 100,000 using the unrounded state values, then adds the co-occurrence map and makes the 3 panel figure.
# reads : Tables/table2_*_by_state_PER100K_UNR.rds (11b), Tables/state_tbdm_per100k_fullprecision.rds (script 12, part 2)
# writes: Figures/Figure2_TB_Diabetes_combined_PER100K_UNR.png/.pdf, Figure_TBDM_cooccurrence_binned_UNR.png/.pdf, Figure2_TB_DM_TBDM_3panel_PER100K_UNR.png/.pdf
# Run script 12 first, in the same session (needs map_data_tb, map_data_dm).

# 1. setup ----
library(here); library(sf); library(ggplot2); library(dplyr); library(patchwork)

# objects from script 12 must exist
stopifnot(exists('map_data_tb'), exists('map_data_dm'))

# 2. swap in the unrounded state values ----
# joined by state name (the column 'subgroup')
unr_tb <- readRDS(here('Tables', 'table2_tb_by_state_PER100K_UNR.rds')) |> select(subgroup, per100k)
unr_dm <- readRDS(here('Tables', 'table2_dm_by_state_PER100K_UNR.rds')) |> select(subgroup, per100k)
map_data_tb <- map_data_tb |> select(-any_of('per100k')) |> left_join(unr_tb, by = 'subgroup')
map_data_dm <- map_data_dm |> select(-any_of('per100k')) |> left_join(unr_dm, by = 'subgroup')
# check: both should print 0
cat('States with a value before but missing now - TB:',
    sum(is.na(map_data_tb$per100k) & !is.na(map_data_tb$prevalence_pct)),
    ' Diabetes:', sum(is.na(map_data_dm$per100k) & !is.na(map_data_dm$prevalence_pct)), '\n')

# 3. TB map per 100,000 ----
fig2_tb_100k <- ggplot(map_data_tb) +
  geom_sf(aes(fill = per100k), color = 'white', linewidth = 0.2) +
  scale_fill_viridis_c(
    option = 'magma', direction = -1, na.value = 'grey85',
    name = 'TB per\n100,000'
  ) +
  labs(
    title = 'A. Tuberculosis',
    caption = 'Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries. Values per 100,000 population.'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0),
    legend.position = 'right'
  )

fig2_tb_100k

# 4. diabetes map per 100,000 ----
fig2_dm_100k <- ggplot(map_data_dm) +
  geom_sf(aes(fill = per100k), color = 'white', linewidth = 0.2) +
  scale_fill_viridis_c(
    option = 'viridis', direction = -1, na.value = 'grey85',
    name = 'Diabetes per\n100,000'
  ) +
  labs(
    title = 'B. Diabetes',
    caption = 'Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries. Values per 100,000 population.'
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    plot.caption = element_text(size = 7, hjust = 0),
    legend.position = 'right'
  )

fig2_dm_100k

# 5. combine and save ----
dir.create(here('Figures'), showWarnings = FALSE)

ggsave(here('Figures', 'Figure_TB_per100000_UNR.png'), fig2_tb_100k, width = 8, height = 8, dpi = 300)
ggsave(here('Figures', 'Figure_Diabetes_per100000_UNR.png'), fig2_dm_100k, width = 8, height = 8, dpi = 300)

figure2_combined_100k <- fig2_tb_100k + fig2_dm_100k + plot_layout(ncol = 2)
figure2_combined_100k

ggsave(here('Figures', 'Figure2_TB_Diabetes_combined_PER100K_UNR.png'), figure2_combined_100k,
       width = 14, height = 8, dpi = 300)
ggsave(here('Figures', 'Figure2_TB_Diabetes_combined_PER100K_UNR.pdf'), figure2_combined_100k,
       width = 14, height = 8)

cat('\nDone. Per-100,000 Figure 2 saved to Figures/Figure2_TB_Diabetes_combined_PER100K_UNR.png/.pdf\n')

# part 2: co-occurrence map and 3 panel figure ----
stopifnot(exists('map_data_tb'), exists('fig2_tb_100k'), exists('fig2_dm_100k'))

# co-occurrence estimates (full precision)
cooc <- readRDS(here('Tables', 'state_tbdm_per100k_fullprecision.rds')) |>
  select(subgroup = state_name, cases, cooc100k = per100k)

map_cooc <- map_data_tb |> select(subgroup) |> left_join(cooc, by = 'subgroup')
cat('Map polygons with NO co-occurrence value (must be 0):', sum(is.na(map_cooc$cooc100k)), '\n')
if (sum(is.na(map_cooc$cooc100k)) > 0) print(map_cooc$subgroup[is.na(map_cooc$cooc100k)])

# class breaks. states with no case get their own grey class
brks   <- c(0, 5, 10, 20, 40, 100, Inf)
labs_b <- c('>0 to <5', '5 to <10', '10 to <20', '20 to <40', '40 to <100', '100 or more')
map_cooc <- map_cooc |>
  mutate(cls = as.character(cut(cooc100k, breaks = brks, labels = labs_b,
                                include.lowest = FALSE, right = FALSE)),
         cls = if_else(cases == 0, 'No case observed', cls),
         cls = factor(cls, levels = c(labs_b, 'No case observed')))

cap_c <- paste0('Source: NFHS-5, India (2019-21). State boundaries: geoBoundaries. Values per 100,000 adults.\n',
                'Grey = no co-occurring case observed in the sample. Estimates in states with <10 cases are imprecise.')
fig_c <- ggplot(map_cooc) +
  geom_sf(aes(fill = cls), color = 'white', linewidth = 0.2) +
  scale_fill_manual(values = c(setNames(viridisLite::magma(6, direction = -1), labs_b),
                               'No case observed' = 'grey75'),
                    drop = FALSE, name = 'TB + diabetes\nper 100,000') +
  labs(title = 'C. TB and diabetes co-occurrence', caption = cap_c) +
  theme_void() +
  theme(plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
        plot.caption = element_text(size = 7, hjust = 0), legend.position = 'right')
print(fig_c)

dir.create(here('Figures'), showWarnings = FALSE)
ggsave(here('Figures', 'Figure_TBDM_cooccurrence_binned_UNR.png'), fig_c, width = 8, height = 8, dpi = 300)
ggsave(here('Figures', 'Figure_TBDM_cooccurrence_binned_UNR.pdf'), fig_c, width = 8, height = 8)

# 3 panel figure
fig3 <- fig2_tb_100k + fig2_dm_100k + fig_c + plot_layout(ncol = 3)
print(fig3)
ggsave(here('Figures', 'Figure2_TB_DM_TBDM_3panel_PER100K_UNR.png'), fig3, width = 21, height = 8, dpi = 300)
ggsave(here('Figures', 'Figure2_TB_DM_TBDM_3panel_PER100K_UNR.pdf'), fig3, width = 21, height = 8)
cat('\nDone. Saved panel C and the 3-panel figure in Figures/.\n')
