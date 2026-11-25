
# ============================================================
# DS 5110 Project
# Clean State Inflow, State Outflow, and Gross Migration Data
# Years: 2011-2022
# ============================================================

library(tidyverse)


# ------------------------------------------------------------
# 1. Define year codes and starting years
# ------------------------------------------------------------

years <- tibble(
  code = c(
    "1112", "1213", "1314", "1415",
    "1516", "1617", "1718", "1819",
    "1920", "2021", "2122", "2223"
  ),
  year = 2011:2022
)


# ------------------------------------------------------------
# 2. Function to read and clean one state CSV file
# ------------------------------------------------------------

read_one <- function(type, code, year) {
  
  file_path <- paste0(
    "data/raw/new/state",
    type,
    code,
    ".csv"
  )
  
  data <- read_csv(
    file_path,
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower)
  
  # Check that required columns exist
  required_columns <- c("n1", "n2", "agi")
  
  if (!all(required_columns %in% names(data))) {
    stop(paste("Required columns missing in:", file_path))
  }
  
  # Identify suppressed values before replacing them
  data <- data |>
    mutate(
      suppressed = if_any(
        all_of(required_columns),
        ~ str_to_lower(str_trim(.x)) %in% c("-1", "d")
      )
    ) |>
    mutate(
      across(
        all_of(required_columns),
        ~ if_else(
          str_to_lower(str_trim(.x)) %in% c("-1", "d"),
          NA_character_,
          .x
        )
      ),
      year = year
    )
  
  return(data)
}


# ------------------------------------------------------------
# 3. Combine all state inflow files
# ------------------------------------------------------------

inflow <- map2_dfr(
  years$code,
  years$year,
  ~ read_one("inflow", .x, .y)
)


# ------------------------------------------------------------
# 4. Combine all state outflow files
# ------------------------------------------------------------

outflow <- map2_dfr(
  years$code,
  years$year,
  ~ read_one("outflow", .x, .y)
)


# ------------------------------------------------------------
# 5. Check inflow and outflow year counts
# ------------------------------------------------------------

count(inflow, year)
count(outflow, year)


# ------------------------------------------------------------
# 6. Check the column structures
# ------------------------------------------------------------

glimpse(inflow)
glimpse(outflow)


# ------------------------------------------------------------
# 7. Count suppressed rows
# ------------------------------------------------------------

inflow |> count(suppressed)
outflow |> count(suppressed)


# ------------------------------------------------------------
# 8. Save cleaned state datasets
# ------------------------------------------------------------

write_csv(
  inflow,
  "data/clean/state_inflow_2011_2022.csv"
)

write_csv(
  outflow,
  "data/clean/state_outflow_2011_2022.csv"
)


# ============================================================
# D4: Combine Gross Migration Files
# ============================================================


# ------------------------------------------------------------
# 9. Define gross migration year codes
# ------------------------------------------------------------

gross_years <- tibble(
  code = c(
    "1112", "1213", "1314", "1415",
    "1516", "1617", "1718", "1819",
    "1920", "2021", "2122", "2223"
  ),
  year = 2011:2022
)


# ------------------------------------------------------------
# 10. Function to read one gross migration file
# ------------------------------------------------------------

read_gross <- function(code, year) {
  
  file_path <- paste0(
    "data/raw/new/",
    code,
    "inmigall.csv"
  )
  
  data <- read_csv(
    file_path,
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower)
  
  # Check expected file dimensions
  if (nrow(data) != 408 || ncol(data) != 144) {
    stop(paste("Unexpected dimensions in:", file_path))
  }
  
  # Add year while preserving original data values
  data <- data |>
    mutate(year = year)
  
  return(data)
}


# ------------------------------------------------------------
# 11. Combine all 12 gross migration files
# ------------------------------------------------------------

gross_migration <- map2_dfr(
  gross_years$code,
  gross_years$year,
  read_gross
)


# ------------------------------------------------------------
# 12. Check combined gross migration dataset
# ------------------------------------------------------------

dim(gross_migration)

count(gross_migration, year)

glimpse(gross_migration)


# ------------------------------------------------------------
# 13. Save cleaned gross migration dataset
# ------------------------------------------------------------

write_csv(
  gross_migration,
  "data/clean/gross_migration_2011_2022.csv"
)


# ============================================================
# END OF SCRIPT
# ============================================================
