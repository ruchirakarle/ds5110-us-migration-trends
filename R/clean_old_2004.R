# scripts/clean_old_2004.R
# Clean IRS SOI state-to-state migration data, 2004-2005 (state0405.zip).
# Output follows the team format: original IRS column names (lowercase) + year,
# year = first year of the pair (2004-2005 -> 2004). All rows kept, incl. summary rows.
#
# Companion script: clean_old_2000_2003.R handles 2000-01 through 2003-04.
#
# How state0405.zip differs from the earlier ZIPs (checked by hand):
#   * ONE flat folder: files are named <StateName>0405in.xls / <StateName>0405out.xls
#     (no Inflow / Outflow subfolders). The ZIP also holds a Word doc and two .dat files.
#   * The title block is one row shorter: data start on row 8 (not row 9), and the
#     state the file is about ("TO: 01-ALABAMA" / "FROM: 01-ALABAMA") is in row 3.
#   * Wyoming0405out.xls has a 7th column (median AGI) that no other file has.
#     Only the first 6 columns are read, so it is ignored.
#   * There are no Excel files for the national totals (state code 00). They are only in
#     statein0405USA.dat / stateout0405USA.dat (4 lines each, the first line is
#     duplicated), so the 00 rows are taken from there (same approach as the 2005-2010
#     script, which does this for 2005-06). The extra last number on each .dat line is
#     an undocumented field and is dropped.
#   * Same as the earlier years: the state of the file is NOT a column; codes are stored
#     as numbers (1 instead of 01) and are padded to 2 digits; summary rows
#     (96, 97, 98, 57 "Foreign", same-state "Non-Migrants") are kept.

library(tidyverse)
library(readxl)

raw_dir <- if (exists("raw_dir")) raw_dir else "data/raw/old"
out_dir <- if (exists("out_dir")) out_dir else "data/clean"

my_year  <- 2004
zip_name <- "state0405.zip"

year_dir <- file.path(raw_dir, my_year)   # data/raw/old/2004/

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Get and unzip the file (skips anything already there) ----
zip_path  <- file.path(raw_dir, zip_name)
local_zip <- file.path(path.expand("~/Downloads"), zip_name)

if (!file.exists(zip_path)) {
  if (file.exists(local_zip)) {
    file.copy(local_zip, zip_path)
  } else {
    download.file(paste0("https://www.irs.gov/pub/irs-soi/", zip_name),
                  destfile = zip_path, mode = "wb")
  }
}
if (length(list.files(year_dir, pattern = "\\.xls$", ignore.case = TRUE)) == 0) {
  unzip(zip_path, exdir = year_dir)
}

# Final column order (agreed team format; `suppressed` as in the 2005-2010 and 1995-1999 scripts)
in_cols  <- c("y2_statefips", "y1_statefips", "y1_state", "y1_state_name",
              "n1", "n2", "agi", "year", "suppressed")
out_cols <- c("y1_statefips", "y2_statefips", "y2_state", "y2_state_name",
              "n1", "n2", "agi", "year", "suppressed")

# ---- 2. Read one state file (.xls) ----
read_xls_0405 <- function(path, type, year) {
  raw <- read_excel(path, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  
  # Data begin at the first row whose first cell is code 96 (total US & foreign).
  start <- which(str_trim(raw[[1]]) == "96")[1]
  if (is.na(start)) stop("Cannot find the first data row (code 96) in: ", path)
  
  # Everything above the data is the title block.
  header <- map_chr(seq_len(start - 1),
                    ~ paste(na.omit(unlist(raw[.x, ])), collapse = " "))
  
  # Safety check: the file must belong to the year we think it does.
  if (!str_detect(header[1], paste0(year, " - ", year + 1)))
    stop("Title does not say ", year, " - ", year + 1, " in: ", path)
  
  # Which state is this file about, and is it the right direction?
  hit <- str_match(paste(header, collapse = " "), "\\b(TO|FROM):\\s*(\\d{2})-")
  if (is.na(hit[1, 1])) stop("Cannot find the state code in the title of: ", path)
  expected_dir <- if (type == "inflow") "TO" else "FROM"
  if (hit[1, 2] != expected_dir)
    stop("Expected a ", expected_dir, " file but got ", hit[1, 2], ": ", path)
  this_state <- hit[1, 3]
  
  # First 6 columns only (drops Wyoming's extra median-AGI column) and only rows that
  # start with a state code (drops blank and footnote rows).
  body <- raw[start:nrow(raw), 1:6]
  names(body) <- c("other", "abbr", "name", "n1", "n2", "agi")
  body <- body |>
    filter(str_detect(str_trim(other), "^\\d+$")) |>
    mutate(abbr = str_trim(abbr), name = str_trim(name))
  
  if (type == "inflow") {
    body |> transmute(y2_statefips = this_state, y1_statefips = other,
                      y1_state = abbr, y1_state_name = name, n1, n2, agi)
  } else {
    body |> transmute(y1_statefips = this_state, y2_statefips = other,
                      y2_state = abbr, y2_state_name = name, n1, n2, agi)
  }
}

# ---- 3. Read the national totals (state code 00) from the .dat file ----
# Each line: state1 cty1 state2 cty2 abbr name returns exemptions agi extra
# Inflow:  first pair = destination (00), second pair = origin (96/97/98).
# Outflow: first pair = origin (00), second pair = destination (96/97/98).
read_dat <- function(path, type) {
  lines <- read_lines(path) |> str_remove("\r$") |> discard(~ .x == "")
  parts <- str_match(lines,
                     "^(\\d{2}) \\d{3} (\\d{2}) \\d{3} (\\S{2}) (.*?)\\s+(-?\\d+)\\s+(-?\\d+)\\s+(-?\\d+)\\s+(-?\\d+)\\s*$")
  stopifnot(!anyNA(parts[, 1]))   # every line must match the layout
  df <- tibble(a = parts[, 2], b = parts[, 3], abbr = parts[, 4],
               name = str_trim(parts[, 5]),
               n1 = parts[, 6], n2 = parts[, 7], agi = parts[, 8]) |>
    distinct()                    # the first line (code 96) appears twice in the file
  if (type == "inflow") {
    df |> transmute(y2_statefips = a, y1_statefips = b, y1_state = abbr,
                    y1_state_name = name, n1, n2, agi)
  } else {
    df |> transmute(y1_statefips = a, y2_statefips = b, y2_state = abbr,
                    y2_state_name = name, n1, n2, agi)
  }
}

# ---- 4. Shared formatting ----
# 2-digit text codes, numeric counts.
# Suppressed flows (IRS hides small counts) show up as "d" or -1. Team rule: set n1, n2, agi
# to NA, keep the row, flag suppressed = TRUE. (None were found in this year, but the rule
# is kept so all cleaned files behave the same way.)
tidy_types <- function(df) {
  df |>
    mutate(across(c(y1_statefips, y2_statefips),
                  ~ str_pad(as.character(as.integer(.x)), 2, pad = "0")),
           suppressed = str_to_lower(str_trim(n1)) %in% c("d", "-1"),
           across(c(n1, n2, agi),
                  ~ if_else(suppressed, NA_real_,
                            suppressWarnings(as.numeric(str_remove_all(str_trim(.x), ","))))))
}

# ---- 5. Read one direction ----
clean_year <- function(year, type) {
  pattern <- if (type == "inflow") "0405in\\.xls$" else "0405out\\.xls$"
  files <- list.files(year_dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)
  stopifnot(length(files) == 51)   # 50 states + DC
  
  states  <- map_dfr(files, ~ read_xls_0405(.x, type, year))
  dat     <- if (type == "inflow") "statein0405USA.dat" else "stateout0405USA.dat"
  us_rows <- read_dat(file.path(year_dir, dat), type)
  stopifnot(nrow(us_rows) == 3)
  
  cols <- if (type == "inflow") in_cols else out_cols
  bind_rows(us_rows, states) |>
    tidy_types() |>
    mutate(year = as.integer(year)) |>
    select(all_of(cols))
}

inflow  <- clean_year(my_year, "inflow")
outflow <- clean_year(my_year, "outflow")

# ---- 6. Checks ----
print(count(inflow, year))      # 2808 = 51 states x 55 rows + 3 national rows
print(count(outflow, year))

# 51 states + the 00 national rows
print(n_distinct(inflow$y2_statefips))    # 52
print(n_distinct(outflow$y1_statefips))   # 52

# No duplicated flows
stopifnot(!anyDuplicated(inflow  |> select(y2_statefips, y1_statefips)),
          !anyDuplicated(outflow |> select(y1_statefips, y2_statefips)))

# Only suppressed flows may be NA
stopifnot(identical(is.na(inflow$n1),  inflow$suppressed),
          identical(is.na(outflow$n1), outflow$suppressed))
print(count(inflow,  suppressed))
print(count(outflow, suppressed))
print(summary(inflow$n1))
print(summary(outflow$n1))

# The 51 states' "Total Mig - US" (code 97) rows must add up to the United States (00) row
check_us <- function(df, home_col, other_col) {
  df |>
    filter(.data[[other_col]] == "97") |>
    group_by(is_us = .data[[home_col]] == "00") |>
    summarise(n1 = sum(n1), n2 = sum(n2), agi = sum(agi), .groups = "drop")
}
print(check_us(inflow,  "y2_statefips", "y1_statefips"))   # TRUE row == FALSE row
print(check_us(outflow, "y1_statefips", "y2_statefips"))

# Check: Florida (12) total migration rows
print(inflow |> filter(y2_statefips == "12", y1_statefips %in% c("96", "97", "98")))

# ---- 7. Save ----
write_csv(inflow,  file.path(out_dir, "state_inflow_2004.csv"))
write_csv(outflow, file.path(out_dir, "state_outflow_2004.csv"))