# scripts/clean_old_2000_2003.R
# Clean IRS SOI state-to-state migration data, 2000-2001 through 2003-2004.
# Output follows the team format: original IRS column names (lowercase) + year,
# year = first year of the pair (2000-2001 -> 2000). All rows kept, incl. summary rows.
#
# Companion script: clean_old_2004.R handles state0405.zip (2004-2005), which has a
# different folder structure and a slightly different Excel layout.
#
# Source layout (checked by hand against the ZIPs):
#   * Each ZIP has an Inflow folder and an Outflow folder, one .xls file per state.
#   * Every file: title rows, then the data. Data columns are
#       1 = code of the OTHER state, 2 = its abbreviation, 3 = its name,
#       4 = returns (n1), 5 = exemptions (n2), 6 = AGI (in thousands of dollars)
#   * The state the file is ABOUT is NOT a column. It only appears in a title cell,
#     e.g. "TO: 01-ALABAMA" (inflow) or "FROM: 01-ALABAMA" (outflow). Its position
#     varies (sometimes split over two cells, e.g. "FROM:" | | "19-IOWA"), so the
#     script searches the whole title block instead of reading one fixed cell.
#   * Summary rows (96 = total US & foreign, 97 = total US, 98 = foreign,
#     same-state "Non-Migrants", "Foreign" code 57) are KEPT.
#   * 2000-01, 2001-02 and 2002-03 also contain a "UNITED STATES" file (state code 00)
#     with the national 96/97/98 rows. It is KEPT (team decision). 2003-04 has no
#     such file, so it has no 00 rows.
#   * File names are irregular (e.g. oeg01inr.xls, vrg01otr.xls, nrbt04ot.xls),
#     so every .xls in the folder is read rather than building names by state.
#   * Some codes are stored as numbers (e.g. 1 instead of 01) -> padded to 2 digits.

library(tidyverse)
library(readxl)

raw_dir <- if (exists("raw_dir")) raw_dir else "data/raw/old"
out_dir <- if (exists("out_dir")) out_dir else "data/clean"

my_years  <- 2000:2003
zip_names <- c("2000to2001statemigration.zip",
               "2001to2002statemigration.zip",
               "2002to2003statemigration.zip",
               "2003to2004statemigration.zip")

# Each ZIP is unzipped into its own folder named by the first year, e.g. data/raw/old/2000/
year_dir <- function(y) file.path(raw_dir, y)

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Get and unzip the files (skips anything already there) ----
for (i in seq_along(zip_names)) {
  zip_path  <- file.path(raw_dir, zip_names[i])
  local_zip <- file.path(path.expand("~/Downloads"), zip_names[i])
  
  if (!file.exists(zip_path)) {
    if (file.exists(local_zip)) {
      file.copy(local_zip, zip_path)
    } else {
      download.file(paste0("https://www.irs.gov/pub/irs-soi/", zip_names[i]),
                    destfile = zip_path, mode = "wb")
    }
  }
  
  already <- list.files(year_dir(my_years[i]), pattern = "\\.xls$",
                        recursive = TRUE, ignore.case = TRUE)
  if (length(already) == 0) unzip(zip_path, exdir = year_dir(my_years[i]))
}

# Final column order (agreed team format; `suppressed` as in the 2005-2010 and 1995-1999 scripts)
in_cols  <- c("y2_statefips", "y1_statefips", "y1_state", "y1_state_name",
              "n1", "n2", "agi", "year", "suppressed")
out_cols <- c("y1_statefips", "y2_statefips", "y2_state", "y2_state_name",
              "n1", "n2", "agi", "year", "suppressed")

# ---- 2. Read one state file ----
read_old_file <- function(path, type, year) {
  raw <- read_excel(path, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  
  # Data begin at the first row whose first cell is code 96 (total US & foreign).
  start <- which(str_trim(raw[[1]]) == "96")[1]
  if (is.na(start)) stop("Cannot find the first data row (code 96) in: ", path)
  
  # Title block = everything above the data. Paste each row's cells together so that
  # titles split over several cells ("FROM:" | | "19-IOWA") are still found.
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
  
  # Keep only the first 6 columns, and only rows that start with a state code
  # (this drops blank and footnote rows).
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

# ---- 3. Shared formatting ----
# 2-digit text codes, numeric counts.
# Suppressed flows (IRS hides small counts) show up as "d" or -1. Team rule: set n1, n2, agi
# to NA, keep the row, flag suppressed = TRUE. (None were found in these years, but the rule
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

# ---- 4. Read one year, one direction ----
flow_folder <- function(year, type) {
  dirs <- list.dirs(year_dir(year), recursive = TRUE, full.names = TRUE)
  hit  <- dirs[str_detect(basename(dirs), regex(type, ignore_case = TRUE))]
  stopifnot(length(hit) == 1)
  hit
}

clean_year <- function(year, type) {
  files <- list.files(flow_folder(year, type), pattern = "\\.xls$",
                      full.names = TRUE, ignore.case = TRUE)
  # 2000-2002 have 52 files per direction (50 states + DC + United States);
  # 2003 has 51 (no United States file).
  stopifnot(length(files) == if (year == 2003) 51 else 52)
  
  df <- map_dfr(files, ~ read_old_file(.x, type, year))
  cols <- if (type == "inflow") in_cols else out_cols
  df |> tidy_types() |> mutate(year = as.integer(year)) |> select(all_of(cols))
}

inflow  <- map_dfr(my_years, ~ clean_year(.x, "inflow"))
outflow <- map_dfr(my_years, ~ clean_year(.x, "outflow"))

# ---- 5. Checks ----
print(count(inflow, year))
print(count(outflow, year))

# Expected rows per year: a complete year is 51 states x 55 rows (+3 national rows if the
# United States file exists) = 2808 (2002) or 2805 (2003, no 00 file).
# 2000 and 2001 are 4 rows short (2804) on purpose: the IRS file omits a few tiny flows
# between small states (2000: North/South Dakota <-> New Hampshire/Vermont; 2001: North/South
# Dakota <-> Rhode Island/Vermont). The same flows are missing on both the inflow and the
# outflow side, and none are marked "d", so these are IRS omissions, not reading errors.
expected_rows <- c("2000" = 2804, "2001" = 2804, "2002" = 2808, "2003" = 2805)
stopifnot(identical(as.numeric(count(inflow,  year)$n), unname(expected_rows)),
          identical(as.numeric(count(outflow, year)$n), unname(expected_rows)))

# Home-state files per year (51 states, +1 for the 00 file in 2000-2002)
print(inflow  |> group_by(year) |> summarise(home_states = n_distinct(y2_statefips)))
print(outflow |> group_by(year) |> summarise(home_states = n_distinct(y1_statefips)))

# No duplicated flows
stopifnot(!anyDuplicated(inflow  |> select(y2_statefips, y1_statefips, year)),
          !anyDuplicated(outflow |> select(y1_statefips, y2_statefips, year)))

# Only suppressed flows may be NA
stopifnot(identical(is.na(inflow$n1),  inflow$suppressed),
          identical(is.na(outflow$n1), outflow$suppressed))
print(count(inflow,  suppressed))
print(count(outflow, suppressed))
print(summary(inflow$n1))
print(summary(outflow$n1))

# The 51 states' "Total Mig - US" (code 97) rows must add up to the United States (00) row.
# 2003 has no 00 rows, so it is skipped in this check.
check_us <- function(df, home_col, other_col) {
  df |>
    filter(.data[[other_col]] == "97") |>
    group_by(year, is_us = .data[[home_col]] == "00") |>
    summarise(n1 = sum(n1), .groups = "drop") |>
    pivot_wider(names_from = is_us, values_from = n1, names_prefix = "us_") |>
    mutate(difference = us_TRUE - us_FALSE)
}
print(check_us(inflow,  "y2_statefips", "y1_statefips"))
print(check_us(outflow, "y1_statefips", "y2_statefips"))

# Check on one state: Florida (12) inflow should change gradually across years
print(inflow |> filter(y2_statefips == "12", y1_statefips == "96") |> select(year, n1, n2, agi))

# ---- 6. Save ----
write_csv(inflow,  file.path(out_dir, "state_inflow_2000_2003.csv"))
write_csv(outflow, file.path(out_dir, "state_outflow_2000_2003.csv"))

