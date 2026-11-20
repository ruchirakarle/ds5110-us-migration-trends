# clean_old_1990_1994.R
# DS 5110 project - IRS state to state migration data
# Ruchira Karle
#
# cleans the old IRS excel files for 1990-1994 (inflow + outflow)
# so they match the newer csv files (same column names + year + suppressed)


# ---- step 1: packages and settings ----

library(tidyverse)
library(readxl)

years <- 1990:1994
zips <- c("1990to1991statemigration.zip",
          "1991to1992statemigration.zip",
          "1992to1993statemigration.zip",
          "1993to1994statemigration.zip",
          "1994to1995statemigration.zip")

out_inflow  <- "data/clean/state_inflow_1990_1994.csv"
out_outflow <- "data/clean/state_outflow_1990_1994.csv"

# state fips codes (old files only show the state name in the title)
states <- tibble(
  state_name = c(state.name, "District of Columbia"),
  state      = c(state.abb, "DC")
) |>
  mutate(statefips = recode(state,
                            AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10",
                            DC="11", FL="12", GA="13", HI="15", ID="16", IL="17", IN="18", IA="19",
                            KS="20", KY="21", LA="22", ME="23", MD="24", MA="25", MI="26", MN="27",
                            MS="28", MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34", NM="35",
                            NY="36", NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44",
                            SC="45", SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53",
                            WV="54", WI="55", WY="56"))


# ---- step 2: download and unzip ----
# only downloads if the zip isn't there already

dir.create("data/raw/old", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(zips)) {
  zip_path <- file.path("data/raw/old", zips[i])
  if (!file.exists(zip_path)) {
    download.file(paste0("https://www.irs.gov/pub/irs-soi/", zips[i]),
                  destfile = zip_path, mode = "wb")
  }
  unzip(zip_path, exdir = file.path("data/raw/old", years[i]))
}


# ---- step 3: helper functions ----

# finds the inflow / outflow folder for a year
find_folder <- function(year, type) {
  dirs <- list.dirs(file.path("data/raw/old", year), recursive = TRUE)
  dirs[grepl(type, basename(dirs), ignore.case = TRUE)][1]
}

# column positions change by year:
#   inflow 1990-91:  code, abbr, name, returns, %, exemptions, %
#   outflow 1990-91: code, abbr, abbr, name, returns, %, exemptions, %
#   1992-94 (both):  code, abbr, name, returns, exemptions, income
# (no income column before 1992)
get_layout <- function(year, type) {
  if (year <= 1991 & type == "inflow") {
    list(code = 1, abbr = 2, name = 3, n1 = 4, n2 = 6, agi = NA)
  } else if (year <= 1991 & type == "outflow") {
    list(code = 1, abbr = 2, name = 4, n1 = 5, n2 = 7, agi = NA)
  } else {
    list(code = 1, abbr = 2, name = 3, n1 = 4, n2 = 5, agi = 6)
  }
}

# gets the file's own state from the "TO:" / "FROM:" line at the top
# compares letters only because 1992 writes names like "NewHampshire"
get_own_state <- function(path) {
  top <- suppressMessages(read_excel(path, col_names = FALSE, n_max = 8,
                                     col_types = "text"))
  lines <- apply(top, 1, function(r) paste(na.omit(r), collapse = " "))
  title <- lines[str_detect(lines, regex("^(TO|FROM):\\s*\\S", ignore_case = TRUE))][1]
  if (is.na(title)) return(NA)
  
  letters_only <- function(x) str_remove_all(toupper(x), "[^A-Z]")
  match <- states |>
    filter(str_detect(letters_only(title), fixed(letters_only(state_name)))) |>
    arrange(desc(nchar(state_name)))   # so Arkansas doesn't match Kansas
  if (nrow(match) == 0) NA else match$statefips[1]
}

# reads one excel file and returns it in the IRS csv format
read_old_file <- function(path, type, year) {
  own <- get_own_state(path)
  if (is.na(own)) return(NULL)   # skips the Foreign file
  
  cols <- get_layout(year, type)
  raw <- suppressMessages(read_excel(path, skip = 8, col_names = FALSE,
                                     col_types = "text"))
  
  df <- tibble(
    code = str_trim(raw[[cols$code]]),
    abbr = raw[[cols$abbr]],
    name = raw[[cols$name]],
    n1   = raw[[cols$n1]],
    n2   = raw[[cols$n2]],
    agi  = if (is.na(cols$agi)) NA_character_ else raw[[cols$agi]]
  ) |>
    filter(!is.na(code), !is.na(n1))   # drop blank and footnote rows
  
  df <- df |>
    mutate(
      # total row has a different label every year -> use 96 like the new files
      # non-migrant row (people who stayed) -> use the state's own code
      code = case_when(
        str_detect(name, regex("total", ignore_case = TRUE)) ~ "96",
        code %in% c("96", "63") ~ "96",
        str_detect(name, regex("non.?migrant", ignore_case = TRUE)) ~ own,
        TRUE ~ str_pad(code, 2, pad = "0")
      ),
      abbr = if_else(code == own, NA_character_, abbr),
      name = if_else(code == own, "State Non-Migrant", name),
      
      # team rule: hidden values ("d" or -1) become NA and get flagged
      suppressed = str_trim(n1) %in% c("d", "-1"),
      across(c(n1, n2, agi), ~ str_trim(str_remove_all(.x, ","))),
      across(c(n1, n2, agi), ~ if_else(.x %in% c("d", "-1"), NA_character_, .x)),
      across(c(n1, n2, agi), ~ suppressWarnings(as.numeric(.x))),
      year = year
    )
  
  if (type == "inflow") {
    df |> transmute(y2_statefips = own, y1_statefips = code,
                    y1_state = abbr, y1_state_name = name,
                    n1, n2, agi, year, suppressed)
  } else {
    df |> transmute(y1_statefips = own, y2_statefips = code,
                    y2_state = abbr, y2_state_name = name,
                    n1, n2, agi, year, suppressed)
  }
}

# reads every state file for one year
clean_year <- function(year, type) {
  files <- list.files(find_folder(year, type), pattern = "\\.xls",
                      full.names = TRUE, ignore.case = TRUE)
  map_dfr(files, ~ read_old_file(.x, type, year))
}


# ---- step 4: clean all 5 years ----

inflow  <- map_dfr(years, ~ clean_year(.x, "inflow"))
outflow <- map_dfr(years, ~ clean_year(.x, "outflow"))


# ---- step 5: quick checks ----

# should be 51 states every year
print(inflow |> distinct(year, y2_statefips) |> count(year))
print(outflow |> distinct(year, y1_statefips) |> count(year))

# rows per year should be close to each other (~2700)
print(count(inflow, year))

# income is missing for 1990-91 only
print(inflow |> group_by(year) |> summarise(missing_agi = mean(is.na(agi))))

# florida total inflow should change slowly year to year
print(inflow |> filter(y2_statefips == "12", y1_statefips == "96") |>
        select(year, n1, n2, agi))

# should both be empty (no weird codes)
print(inflow |> filter(!y1_statefips %in% c(states$statefips, "96", "57")))
print(outflow |> filter(!y2_statefips %in% c(states$statefips, "96", "57")))


# ---- step 6: save ----

write_csv(inflow, out_inflow)
write_csv(outflow, out_outflow)