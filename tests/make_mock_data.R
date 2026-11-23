# make_mock_data - synthetic stand-in for the NFHS-5 files
#
# The real NFHS-5 data cannot be shared, so this script writes FAKE files with the same
# file names, variable names and codes that scripts 02 to 18 expect. It lets anyone run
# the whole pipeline (Scripts/run_all.R) to see that the code works. The numbers that come
# out mean nothing: every value here is random, apart from a weak built-in link between
# diabetes, tobacco and TB so that the models have something to fit.
#
# writes: Raw_data_women/IAIR7EFL.DTA, Raw_data_men/IAMR7EFL.DTA,
#         Raw_data_household/IAPR7EFL.DTA and india_states.geojson (36 squares, not real boundaries)
# It refuses to run if it finds files that it did not make itself, so real data are never overwritten.
# After testing, delete the three Raw_data_* folders (and Clean_data, Tables, Figures outputs) before using real data.

library(here); library(haven); library(dplyr); library(sf)
set.seed(11)

dirs <- c('Raw_data_women', 'Raw_data_men', 'Raw_data_household')
targets <- c(here(dirs[1], 'IAIR7EFL.DTA'), here(dirs[2], 'IAMR7EFL.DTA'),
             here(dirs[3], 'IAPR7EFL.DTA'), here(dirs[3], 'india_states.geojson'))
marker <- here('Raw_data_household', 'MOCK_DATA_NOT_REAL.txt')

if (any(file.exists(targets)) && !file.exists(marker)) {
  stop('Found data files that were not made by this script. Nothing was changed.', call. = FALSE)
}
for (d in dirs) dir.create(here(d), showWarnings = FALSE)

# 1. clusters and households ----
states <- c('jammu and kashmir', 'himachal pradesh', 'punjab', 'chandigarh', 'uttarakhand', 'haryana',
            'nct of delhi', 'rajasthan', 'uttar pradesh', 'bihar', 'sikkim', 'arunachal pradesh',
            'nagaland', 'manipur', 'mizoram', 'tripura', 'meghalaya', 'assam', 'west bengal',
            'jharkhand', 'odisha', 'chhattisgarh', 'madhya pradesh', 'gujarat',
            'dadra and nagar haveli and daman and diu', 'maharashtra', 'andhra pradesh', 'karnataka',
            'goa', 'lakshadweep', 'kerala', 'tamil nadu', 'puducherry',
            'andaman and nicobar islands', 'telangana', 'ladakh')
n_state <- length(states)
n_clu <- 900

clu <- tibble(v001 = 1:n_clu,
              st   = sample(1:n_state, n_clu, replace = TRUE),
              res  = sample(1:2, n_clu, replace = TRUE, prob = c(.35, .65)))   # 1 urban, 2 rural
clu$strata <- clu$st * 10 + clu$res

hh <- tibble(v001 = rep(1:n_clu, each = 12), v002 = rep(1:12, n_clu)) |>
  left_join(clu, by = 'v001')
hh$wealth_score <- rnorm(nrow(hh)) + ifelse(hh$res == 1, .6, 0)
hh$wq <- as.integer(cut(hh$wealth_score, quantile(hh$wealth_score, 0:5 / 5),
                        include.lowest = TRUE, labels = 1:5))
hh$w <- round(runif(nrow(hh), .3, 3) * 1e6)          # v005 style weight, divided by 1e6 in script 03

# 2. household members, with diabetes, tobacco and TB ----
mem <- hh[rep(seq_len(nrow(hh)), each = 4), ] |>
  group_by(v001, v002) |> mutate(hvidx = row_number()) |> ungroup()
mem$hv104 <- sample(1:2, nrow(mem), replace = TRUE)               # 1 male, 2 female
mem$hv105 <- sample(1:80, nrow(mem), replace = TRUE)              # age
mem$hv103 <- sample(0:1, nrow(mem), replace = TRUE, prob = c(.05, .95))
age_scaled <- mem$hv105 / 80
mem$dm  <- rbinom(nrow(mem), 1, plogis(-4.2 + 3 * age_scaled + .2 * (mem$wq - 3)))
mem$tob <- rbinom(nrow(mem), 1, ifelse(mem$hv104 == 1, .35, .06))
mem$tb  <- rbinom(nrow(mem), 1, plogis(-5.2 + .9 * mem$dm + .5 * mem$tob - .25 * (mem$wq - 3) +
                                          1.2 * mem$dm * mem$tob))

# household TB question: sh28 = any usual resident has TB, sh29a-d = line numbers of those named
named <- mem |> filter(tb == 1) |> group_by(v001, v002) |>
  summarise(lines = list(head(hvidx, 4)), .groups = 'drop')
mem <- left_join(mem, named, by = c('v001', 'v002'))
mem$sh28 <- ifelse(vapply(mem$lines, Negate(is.null), logical(1)), 1, 0)
mem$sh28[sample(nrow(mem), 300)] <- 8                              # a few "don't know"
for (k in 1:4) {
  mem[[paste0('sh29', letters[k])]] <-
    vapply(mem$lines, function(x) if (length(x) >= k) as.numeric(x[k]) else NA_real_, numeric(1))
}
mem$sh29 <- NA_real_

pr <- mem |> transmute(hhid = paste(v001, v002), hv001 = v001, hv002 = v002, hv003 = v002,
                       hv005 = w, hv021 = v001, hv022 = strata, hv023 = strata,
                       hv103, hv104, hv105, hvidx, sh28, sh29a, sh29b, sh29c, sh29d, sh29)
pr$hv104 <- labelled(pr$hv104, c(Male = 1, Female = 2))
pr$sh28  <- labelled(pr$sh28, c(No = 0, Yes = 1, `Don't know` = 8))
for (v in c('sh29a', 'sh29b', 'sh29c', 'sh29d')) pr[[v]] <- labelled(pr[[v]], c(`Not applicable` = 0))
write_dta(pr, here('Raw_data_household', 'IAPR7EFL.DTA'))

# 3. individual files (women 15-49, a random half of men 15-54) ----
make_individual <- function(m, women) {
  n <- nrow(m)
  p <- function(x) paste0(if (women) '' else 'm', x)               # men's variables start with 'm'
  out <- tibble(.rows = n)
  out[[if (women) 'caseid' else 'mcaseid']] <- paste(m$v001, m$v002, m$hvidx)
  out[[p('v001')]] <- m$v001;   out[[p('v002')]] <- m$v002;   out[[p('v003')]] <- m$hvidx
  out[[p('v005')]] <- m$w;      out[[p('v021')]] <- m$v001
  out[[p('v022')]] <- m$strata; out[[p('v023')]] <- m$strata
  out[[p('v012')]] <- m$hv105
  out[[p('v013')]] <- pmin(7, (m$hv105 - 15) %/% 5 + 1)
  out[[p('v024')]] <- labelled(m$st, setNames(1:n_state, states))
  out[[p('v025')]] <- labelled(m$res, c(urban = 1, rural = 2))
  out[[p('v106')]] <- sample(0:3, n, replace = TRUE)
  out[[p('v107')]] <- sample(0:20, n, replace = TRUE)
  out[[p('v130')]] <- sample(c(1, 2, 3, 4, 5, 96), n, replace = TRUE, prob = c(.75, .14, .05, .02, .02, .02))
  out[[p('v131')]] <- sample(c(991, 992, 993, 998), n, replace = TRUE, prob = c(.5, .1, .35, .05))
  out[[p('v101')]] <- sample(1:6, n, replace = TRUE)
  out[[p('v102')]] <- m$res
  for (v in c('v190', 'v190a')) out[[p(v)]] <- m$wq
  for (v in c('v191', 'v191a')) out[[p(v)]] <- round(m$wealth_score * 1e5)

  dm <- m$dm
  dm[sample(n, ceiling(.012 * n))] <- 8                            # "don't know"
  use <- function(prob) ifelse(m$tob == 1, rbinom(n, 1, prob), 0)  # tobacco item, only for users

  if (women) {
    out$s116 <- sample(1:4, n, replace = TRUE)
    out$s190s <- m$wq; out$s191s <- round(m$wealth_score * 1e5)
    out$s190us <- m$wq; out$s191us <- 1; out$s190rs <- m$wq; out$s191rs <- 1
    out$s728a <- dm
    for (v in letters[1:12]) out[[paste0('v463', v)]] <- 0
    out$v463a <- use(.3); out$v463b <- use(.02); out$v463c <- use(.3); out$v463d <- use(.05)
    out$v463f <- use(.02); out$v463g <- use(.02)
    out$v463i <- use(.1); out$v463j <- use(.2); out$v463k <- use(.2); out$v463l <- use(.1)
    out$v463x <- 0; out$v463z <- as.numeric(m$tob == 0)
    for (v in c('v463aa', 'v463ab', 'v464', 's707', 's709', 's710', 's711', 's712f', 's713', 's714', 's716')) {
      out[[v]] <- NA_real_
    }
    out$s708 <- ifelse(m$tob == 1, sample(0:3, n, replace = TRUE, prob = c(.5, .2, .2, .1)), 3)
    out$v445 <- sample(c(1500:3500, 9998), n, replace = TRUE)       # BMI x 100, 9998 = missing
    out$s720 <- rbinom(n, 1, .05)
  } else {
    out$sm118 <- sample(1:4, n, replace = TRUE)
    out$sm190s <- m$wq; out$sm191s <- round(m$wealth_score * 1e5)
    out$sm190us <- m$wq; out$sm191us <- 1; out$sm190rs <- m$wq; out$sm191rs <- 1
    out$sm627a <- dm
    for (v in letters[1:7]) out[[paste0('mv463', v)]] <- 0
    out$mv463a <- use(.5); out$mv463b <- use(.03); out$mv463c <- use(.35); out$mv463d <- use(.03)
    out$mv463e <- use(.02); out$mv463f <- use(.02); out$mv463g <- use(.4)
    out$mv463x <- 0; out$mv463z <- as.numeric(m$tob == 0)
    for (v in c('mv463aa', 'mv463ab', 'mv463ac', 'mv463ad', 'mv464',
                paste0('mv464', letters[1:11]), paste0('mv484', letters[1:11]),
                'sm604', 'sm606', 'sm608', 'sm609', 'sm613')) out[[v]] <- NA_real_
    out$sm607 <- ifelse(m$tob == 1, sample(0:2, n, replace = TRUE, prob = c(.5, .3, .2)), 0)
    out$sm611f <- use(.1)
    out$sm619 <- rbinom(n, 1, .3)
  }
  out
}

women_pool <- mem |> filter(hv104 == 2, hv105 >= 15, hv105 <= 49)
men_pool   <- mem |> filter(hv104 == 1, hv105 >= 15, hv105 <= 54) |> slice_sample(prop = .5)
write_dta(make_individual(women_pool, TRUE),  here('Raw_data_women', 'IAIR7EFL.DTA'))
write_dta(make_individual(men_pool,  FALSE), here('Raw_data_men',   'IAMR7EFL.DTA'))

# 4. boundary file: 36 squares named like the geoBoundaries ADM1 file ----
shape_names <- tools::toTitleCase(states)
shape_names[states == 'nct of delhi'] <- 'Delhi'
squares <- lapply(1:n_state, function(i) {
  x <- 68 + ((i - 1) %% 6) * 4
  y <- 8  + ((i - 1) %/% 6) * 4
  st_polygon(list(rbind(c(x, y), c(x + 3.8, y), c(x + 3.8, y + 3.8), c(x, y + 3.8), c(x, y))))
})
boundaries <- st_sf(shapeName = shape_names, geometry = st_sfc(squares, crs = 4326))
geo_path <- here('Raw_data_household', 'india_states.geojson')
st_write(boundaries, geo_path, delete_dsn = file.exists(geo_path), quiet = TRUE)

writeLines('Synthetic files made by tests/make_mock_data.R. Not NFHS-5 data.', marker)
cat('Mock data written:', nrow(women_pool), 'women,', nrow(men_pool), 'men,', nrow(pr), 'household member rows.\n')
cat('Now run Scripts/run_all.R. Delete the Raw_data_* folders afterwards.\n')
