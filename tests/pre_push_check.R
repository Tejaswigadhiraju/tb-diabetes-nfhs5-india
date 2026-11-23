# pre_push_check - quick checks before pushing to GitHub. Takes a few seconds, needs no data.
#   source('tests/pre_push_check.R')
# 1. every script parses (no typing mistakes)
# 2. no placeholders left in README.md
# 3. nothing data-like sits where git would upload it

library(here)
problems <- 0
say <- function(ok, msg) { cat(if (ok) '[ok]     ' else '[PROBLEM] ', msg, '\n', sep = ''); if (!ok) problems <<- problems + 1 }

# 1. syntax
r_files <- c(list.files(here('Scripts'), pattern = '[.]R$', full.names = TRUE),
             list.files(here('tests'),   pattern = '[.]R$', full.names = TRUE))
for (f in r_files) {
  ok <- !inherits(try(parse(f), silent = TRUE), 'try-error')
  say(ok, paste('parses:', basename(f)))
}

# 2. placeholders
for (f in c('README.md')) {
  txt <- readLines(here(f), warn = FALSE)
  say(!any(grepl('\\[(Your|your)[^]]*\\]', txt)), paste('no [Your ...] placeholders in', f))
}

# 3. files git would upload: everything except the folders .gitignore blocks
all_files <- list.files(here(), recursive = TRUE, all.files = TRUE)
blocked   <- grepl('^(Raw_data_(women|men|household)/|\\.Rproj\\.user/)', all_files) |
             (grepl('^(Clean_data|Tables|Figures)/', all_files) & !grepl('(\\.gitkeep|README\\.md)$', all_files))
blocked   <- blocked | grepl('[.](rds|RData|Rhistory)$', all_files) | grepl('^(women|men)_column_list[.]txt$', all_files) | all_files == 'Rplots.pdf'
upload    <- all_files[!blocked]
risky     <- upload[grepl('[.](dta|rds|rdata|csv|xlsx?|docx|png|pdf|geojson|shp|sav|zip)$', upload, ignore.case = TRUE)]
say(length(risky) == 0, paste('no data or output files in the upload list', if (length(risky)) paste0('(', paste(risky, collapse = ', '), ')') else ''))
cat('\nFiles that would be uploaded:', length(upload), '(expected 39, 40 after a real run that adds session_info.txt)\n')
cat(if (problems == 0) 'All checks passed.\n' else paste(problems, 'problem(s) found. Fix them before you push.\n'))
