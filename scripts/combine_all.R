
# ============================================================
# DS 5110 Project
# Combine State Inflow and State Outflow Datasets
# Coverage: 1990-2022
# ============================================================

library(tidyverse)

# 1. Define input files
# ------------------------------------------------------------

inflow_files <- c(
  "data/clean/state_inflow_1990_1994.csv",
  "data/clean/state_inflow_1995_1999.csv",
  "data/clean/state_inflow_2000_2003.csv",
  "data/clean/state_inflow_2004.csv",
  "data/clean/state_inflow_2005_2010.csv",
  "data/clean/state_inflow_2011_2022.csv"
)

outflow_files <- c(
  "data/clean/state_outflow_1990_1994.csv",
  "data/clean/state_outflow_1995_1999.csv",
  "data/clean/state_outflow_2000_2003.csv",
  "data/clean/state_outflow_2004.csv",
  "data/clean/state_outflow_2005_2010.csv",
  "data/clean/state_outflow_2011_2022.csv"
)

# 2. Define required columns
# ------------------------------------------------------------

inflow_required <- c(
  "y2_statefips", "y1_statefips",
  "y1_state", "y1_state_name",
  "n1", "n2", "agi", "year", "suppressed"
)

outflow_required <- c(
  "y1_statefips", "y2_statefips",
  "y2_state", "y2_state_name",
  "n1", "n2", "agi", "year", "suppressed"
)

# 3. Read and standardize one file
# ------------------------------------------------------------

read_clean_file <- function(file, required_columns) {
  
  if (!file.exists(file)) {
    stop(paste("Missing input file:", file))
  }
  
  data <- read_csv(
    file,
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    rename_with(tolower)
  
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(paste(
      "Missing columns in", file, ":",
      paste(missing_columns, collapse = ", ")
    ))
  }
  
  # Standardize suppression markers
  marker_columns <- c("n1", "n2", "agi")
  
  data <- data |>
    mutate(
      across(
        all_of(marker_columns),
        ~ str_to_lower(str_trim(.x))
      )
    )
  
  # Preserve existing suppression information and
  # identify rows containing suppression markers
  data <- data |>
    mutate(
      suppressed = (
        str_to_lower(str_trim(suppressed)) %in%
          c("true", "t", "1", "yes")
      ) |
        if_any(
          all_of(marker_columns),
          ~ .x %in% c("-1", "d")
        )
    )
  
  # Replace suppression markers with missing values
  data <- data |>
    mutate(
      across(
        all_of(marker_columns),
        ~ if_else(.x %in% c("-1", "d"), NA_character_, .x)
      )
    )
  
  # Convert numeric fields
  for (column in marker_columns) {
    
    original_values <- data[[column]]
    
    converted_values <- parse_number(original_values)
    
    invalid_values <- !is.na(original_values) &
      is.na(converted_values)
    
    if (any(invalid_values)) {
      stop(paste(
        "Unexpected non-numeric values in",
        column, "of", file
      ))
    }
    
    data[[column]] <- converted_values
  }
  
  # Convert year
  data$year <- as.integer(data$year)
  
  if (any(is.na(data$year))) {
    stop(paste("Missing or invalid year values in:", file))
  }
  
  # Keep FIPS codes as character
  data <- data |>
    mutate(
      across(
        any_of(c("y1_statefips", "y2_statefips")),
        as.character
      )
    )
  
  # Standardize column order
  data <- data |>
    select(all_of(required_columns))
  
  return(data)
}

# 4. Combine and validate files
# ------------------------------------------------------------

combine_files <- function(files, required_columns, flow_type) {
  
  combined <- map_dfr(
    files,
    read_clean_file,
    required_columns = required_columns
  )
  
  # Check year coverage
  expected_years <- 1990:2022
  actual_years <- sort(unique(combined$year))
  
  missing_years <- setdiff(expected_years, actual_years)
  extra_years <- setdiff(actual_years, expected_years)
  
  if (length(missing_years) > 0) {
    stop(paste(
      "Missing years in", flow_type, ":",
      paste(missing_years, collapse = ", ")
    ))
  }
  
  if (length(extra_years) > 0) {
    stop(paste(
      "Unexpected years in", flow_type, ":",
      paste(extra_years, collapse = ", ")
    ))
  }
  
  # Check duplicate state-pair/category/year keys
  # Include the descriptive name because some FIPS
  # combinations have multiple legitimate categories.
  
  if (flow_type == "inflow") {
    key_columns <- c(
      "y2_statefips",
      "y1_statefips",
      "y1_state_name",
      "year"
    )
  } else {
    key_columns <- c(
      "y1_statefips",
      "y2_statefips",
      "y2_state_name",
      "year"
    )
  }
  
  duplicates <- combined |>
    count(across(all_of(key_columns))) |>
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    cat("\nDuplicate keys found in", flow_type, ":\n")
    print(duplicates)
    
    stop(paste(
      "Please inspect duplicate keys in",
      flow_type,
      "before saving."
    ))
  }
  
  # Sort records by year
  combined <- combined |>
    arrange(year)
  
  return(combined)
}

# 5. Combine inflow and outflow
# ------------------------------------------------------------

inflow_all <- combine_files(
  inflow_files,
  inflow_required,
  "inflow"
)

outflow_all <- combine_files(
  outflow_files,
  outflow_required,
  "outflow"
)

# 6. Final validation summaries
# ------------------------------------------------------------

cat("\n========== INFLOW DATASET ==========\n")
print(dim(inflow_all))
print(count(inflow_all, year))
print(table(inflow_all$suppressed, useNA = "ifany"))

cat("\n========== OUTFLOW DATASET ==========\n")
print(dim(outflow_all))
print(count(outflow_all, year))
print(table(outflow_all$suppressed, useNA = "ifany"))

cat("\n========== DATA TYPE CHECK ==========\n")
print(sapply(inflow_all, typeof))
print(sapply(outflow_all, typeof))

# 7. Save combined datasets
# ------------------------------------------------------------

write_csv(
  inflow_all,
  "data/clean/state_inflow_all.csv"
)

write_csv(
  outflow_all,
  "data/clean/state_outflow_all.csv"
)

cat("\nBoth combined datasets were created successfully!\n")
