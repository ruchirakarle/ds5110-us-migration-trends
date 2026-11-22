# R/clean_old_2005_2010.R
# Clean IRS SOI state-to-state migration data, 2005-2006 through 2010-2011.
# Output follows the team format: original IRS column names (lowercase) + year,
# year = first year of the pair (2005-2006 -> 2005). All rows kept, incl. summary rows.
#
# Source layout differs by year (checked by hand; notes kept locally in data/raw/old/NOTES_weijen.md):
#   2005-06: one Excel file per state (the combined .dat in the ZIP is truncated, 3 rows)
#   2006-07, 2007-08: combined fixed-width .dat file (all states)
#   2008-09 to 2010-11: combined .csv file (all states)

library(tidyverse)
library(readxl)

raw_dir <- if (exists("raw_dir")) raw_dir else "data/raw/old"
out_dir <- if (exists("out_dir")) out_dir else "data/clean"

my_years  <- 2005:2010
zip_names <- c("state0506.zip", "state0607.zip", "state0708.zip",
               "state0809.zip", "state0910.zip", "state1011.zip")
codes     <- c("0506", "0607", "0708", "0809", "0910", "1011")

# Each ZIP is unzipped into its own folder, e.g. data/raw/old/state0506/
year_dir <- function(code) file.path(raw_dir, paste0("state", code))

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Download and unzip (skips files already downloaded) ----
for (i in seq_along(zip_names)) {
  zip_path <- file.path(raw_dir, zip_names[i])
  if (!file.exists(zip_path)) {
    download.file(paste0("https://www.irs.gov/pub/irs-soi/", zip_names[i]),
                  destfile = zip_path, mode = "wb")
  }
  unzip(zip_path, exdir = year_dir(codes[i]))
}

# Final column order (agreed team format)
in_cols  <- c("y2_statefips", "y1_statefips", "y1_state", "y1_state_name",
              "n1", "n2", "agi", "year", "suppressed")
out_cols <- c("y1_statefips", "y2_statefips", "y2_state", "y2_state_name",
              "n1", "n2", "agi", "year", "suppressed")

# Shared formatting: 2-digit text codes, numeric counts
tidy_types <- function(df) {
  df |>
    mutate(across(c(y1_statefips, y2_statefips),
                  ~ str_pad(as.character(as.integer(.x)), 2, pad = "0")),
           # Suppressed flows (IRS hides small counts): "d" in Excel, -1 in csv.
           # Team rule: set n1, n2, agi to NA, keep the row, flag suppressed = TRUE.
           # (Negative AGI with positive returns is real, so only n1 decides.)
           suppressed = n1 %in% c("d", "-1"),
           across(c(n1, n2, agi),
                  ~ if_else(suppressed, NA_real_,
                            suppressWarnings(as.numeric(str_remove_all(as.character(.x), ","))))))
}

# ---- 2. 2005-06: one Excel file per state ----
# Rows 1-7 are titles; row 3 holds the state this file is about ("TO: 01-ALABAMA").
# Data columns: code of the other state, abbreviation, name, returns, exemptions, AGI.
read_xls_0506 <- function(path, type) {
  raw <- read_excel(path, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  this_state <- str_extract(raw[[1]][3], "\\d{2}")
  body <- raw[-(1:7), 1:6]
  names(body) <- c("other", "abbr", "name", "n1", "n2", "agi")
  body <- body |> filter(!is.na(n1), !is.na(abbr))   # drops footnote rows

  if (type == "inflow") {
    body |> transmute(y2_statefips = this_state, y1_statefips = other,
                      y1_state = abbr, y1_state_name = name, n1, n2, agi)
  } else {
    body |> transmute(y1_statefips = this_state, y2_statefips = other,
                      y2_state = abbr, y2_state_name = name, n1, n2, agi)
  }
}

read_0506 <- function(type) {
  pattern <- if (type == "inflow") "0506in\\.xls$" else "0506out\\.xls$"
  files <- list.files(year_dir("0506"), pattern = pattern,
                      full.names = TRUE)
  stopifnot(length(files) == 51)
  states <- map_dfr(files, ~ read_xls_0506(.x, type))

  # The Excel files have no U.S. total rows (state code 00). The truncated
  # combined .dat holds exactly those 3 rows, so take them from there.
  dat <- if (type == "inflow") "statein0506.dat" else "stateout0506.dat"
  us_rows <- read_dat(file.path(year_dir("0506"), dat), type)
  bind_rows(us_rows, states)
}

# ---- 3. 2006-07 and 2007-08: combined fixed-width .dat ----
# Each line: state1 cty1 state2 cty2 abbr name ... returns exemptions agi extra
# Inflow:  first pair = destination, second pair = origin.
# Outflow: first pair = origin, second pair = destination.
# The last number is an undocumented extra field (not in later years), so it is dropped.
read_dat <- function(path, type) {
  lines <- read_lines(path) |> str_remove("\r$") |> discard(~ .x == "")
  parts <- str_match(lines,
    "^(\\d{2}) \\d{3} (\\d{2}) \\d{3} (\\S{2}) (.*?)\\s+(-?\\d+)\\s+(-?\\d+)\\s+(-?\\d+)\\s+(-?\\d+)\\s*$")
  stopifnot(!anyNA(parts[, 1]))   # every line must match the layout
  df <- tibble(a = parts[, 2], b = parts[, 3], abbr = parts[, 4],
               name = str_trim(parts[, 5]),
               n1 = parts[, 6], n2 = parts[, 7], agi = parts[, 8])
  if (type == "inflow") {
    df |> transmute(y2_statefips = a, y1_statefips = b, y1_state = abbr,
                    y1_state_name = name, n1, n2, agi)
  } else {
    df |> transmute(y1_statefips = a, y2_statefips = b, y2_state = abbr,
                    y2_state_name = name, n1, n2, agi)
  }
}

dat_files <- list(
  "0607" = c(inflow = "statein0607.dat", outflow = "stateout0607.dat"),
  "0708" = c(inflow = "si0708us.dat",    outflow = "so0708us.dat")
)

# ---- 4. 2008-09 to 2010-11: combined .csv ----
# Column names here are e.g. State_Code_Dest / State_Code_Origin / Return_Num;
# renamed to the IRS names used from 2011 on.
read_csv_old <- function(path, type) {
  df <- read_csv(path, col_types = cols(.default = "c")) |>
    rename(n1 = Return_Num, n2 = Exmpt_Num, agi = Aggr_AGI)
  if (type == "inflow") {
    df |> transmute(y2_statefips = State_Code_Dest, y1_statefips = State_Code_Origin,
                    y1_state = State_Abbrv, y1_state_name = State_Name, n1, n2, agi)
  } else {
    df |> transmute(y1_statefips = State_Code_Origin, y2_statefips = State_Code_Dest,
                    y2_state = State_Abbrv, y2_state_name = State_Name, n1, n2, agi)
  }
}

# ---- 5. Read one year ----
clean_year <- function(code, year, type) {
  df <- if (code == "0506") {
    read_0506(type)
  } else if (code %in% names(dat_files)) {
    read_dat(file.path(year_dir(code), dat_files[[code]][[type]]), type)
  } else {
    read_csv_old(file.path(year_dir(code), paste0("state", type, code, ".csv")), type)
  }
  cols <- if (type == "inflow") in_cols else out_cols
  df |> tidy_types() |> mutate(year = year) |> select(all_of(cols))
}

inflow  <- map2_dfr(codes, my_years, ~ clean_year(.x, .y, "inflow"))
outflow <- map2_dfr(codes, my_years, ~ clean_year(.x, .y, "outflow"))

# ---- 6. Checks ----
print(count(inflow, year))      # 2808 rows every year
print(count(outflow, year))

# Sum of the 51 states' "Total Mig - US" (code 97) must equal the U.S. row
check_us <- function(df, state_col, other_col) {
  df |>
    filter(.data[[other_col]] == "97") |>
    group_by(year, us = .data[[state_col]] == "00") |>
    summarise(n1 = sum(n1), .groups = "drop") |>
    pivot_wider(names_from = us, values_from = n1, names_prefix = "us_")
}
print(check_us(inflow,  "y2_statefips", "y1_statefips"))   # us_TRUE == us_FALSE
print(check_us(outflow, "y1_statefips", "y2_statefips"))

# Only suppressed flows should be NA (expect 4 per file in 2005 and 2010)
stopifnot(identical(is.na(inflow$n1), inflow$suppressed),
          identical(is.na(outflow$n1), outflow$suppressed))
print(inflow  |> filter(is.na(n1)) |> count(year))
print(outflow |> filter(is.na(n1)) |> count(year))

write_csv(inflow,  file.path(out_dir, "state_inflow_2005_2010.csv"))
write_csv(outflow, file.path(out_dir, "state_outflow_2005_2010.csv"))
