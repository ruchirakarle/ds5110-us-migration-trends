# R/clean_old_1995_1999.R

library(tidyverse)
library(readxl)

my_years <- c("1995-1996", "1996-1997", "1997-1998",
              "1998-1999", "1999-2000")

zip_names <- c("1995to1996statemigration.zip",
                              "1996to1997statemigration.zip",
                              "1997to1998statemigration.zip",
                              "1998to1999statemigration.zip",
                              "1999to2000statemigration.zip")

dir.create("data/raw/old", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw/new", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(zip_names)) {
  
  zip_path <- file.path("data/raw/old", zip_names[i])
  local_zip <- file.path(path.expand("~/Downloads"), zip_names[i])
  
  if (!file.exists(zip_path)) {
    
    if (file.exists(local_zip)) {
      file.copy(local_zip, zip_path)
      
    } else {
      download.file(
        paste0("https://www.irs.gov/pub/irs-soi/", zip_names[i]),
        destfile = zip_path,
        mode = "wb")
    }
  }
  
  unzip(zip_path, exdir = file.path("data/raw/old", my_years[i]))
}
read_old_file <- function(path, type, year) {
  
  raw <-suppressMessages (read_excel(path, skip = 8, col_names = FALSE, col_types = "text"))
  
  non_migrant_row <- which(
    str_detect(raw[[3]], "Non-Migrants")
  )[1]
  
  if (is.na(non_migrant_row)) {
    stop("Cannot identify state code in: ", path)
  }
  
  state_code <- raw[[1]][non_migrant_row]
  
  if (type == "inflow") {
    out <- raw |> select(y1_statefips = 1,
                         y1_state = 2,
                         y1_state_name = 3,
                         n1 = 4, n2 = 5, agi = 6) |>
      mutate(y2_statefips = state_code)
    
  } else {
    out <- raw |> select(y2_statefips = 1,
                         y2_state = 2,
                         y2_state_name = 3,
                         n1 = 4, n2 = 5, agi = 6) |>
      mutate(
        y1_statefips = state_code,
        y2_statefips = if_else(
          str_detect(coalesce(y2_state_name, ""),
                     "Total Mig - US & For"),
          "96",
          y2_statefips
        )
      )
  }
  
  out |>
    filter(if_any(c(n1, n2, agi),
                  ~ !is.na(.x) & str_trim(.x) != "")) |>
    mutate(
      across(c(y1_statefips, y2_statefips),
             ~ str_pad(str_trim(as.character(.x)), 2, pad = "0"))
    ) |>
    filter(
      str_detect(y1_statefips, "^[0-9]{2}$"),
      str_detect(y2_statefips, "^[0-9]{2}$")
    )|>
    
    mutate(
      suppressed = if_any(c(n1, n2, agi),
                          ~ str_to_lower(str_trim(.x)) %in% c("d", "-1")),
      across(c(n1, n2, agi),
             ~ as.numeric(
               na_if(
                 na_if(str_to_lower(str_remove_all(str_trim(.x), ",")), "d"),
                 "-1"
               )
             )),
      year = as.integer(substr(as.character(year), 1, 4))
    )
}

inflow_folder <- function(y) {
  folders <- list.dirs(
    file.path("data/raw/old", y),
    recursive = TRUE,
    full.names = TRUE
  )
  
  folders[grepl("inflow", basename(folders), ignore.case = TRUE)][1]
}

outflow_folder <- function(y) {
  folders <- list.dirs(
    file.path("data/raw/old", y),
    recursive = TRUE,
    full.names = TRUE
  )
  
  folders[grepl("outflow", basename(folders), ignore.case = TRUE)][1]
}


clean_year <- function(year, type) {
  folder <- if (type == "inflow") inflow_folder(year) else outflow_folder(year)
  files <- list.files(folder, pattern = "\\.xlsx?$", full.names = TRUE)
  
  files <- files[!grepl("us(ir|or)\\.xls$|^unit",
                        basename(files), ignore.case = TRUE)]
  
  map_dfr(files, ~ read_old_file(.x, type, year))
}

inflow <- map_dfr(my_years, ~ clean_year(.x, "inflow"))
outflow <- map_dfr(my_years, ~ clean_year(.x, "outflow"))

inflow_us <- inflow |>
  filter(y1_statefips %in% c("96", "97", "98")) |>
  group_by(year, y1_statefips) |>
  summarise(across(c(n1, n2, agi), sum), .groups = "drop") |>
  mutate(
    y2_statefips = "00",
    y1_state = "US",
    y1_state_name = case_when(
      y1_statefips == "96" ~ "US Total Mig - US & For",
      y1_statefips == "97" ~ "US Total Mig - US",
      y1_statefips == "98" ~ "US Total Mig - Foreign"
    ),
    suppressed = FALSE
  )


outflow_us <- outflow |>
  filter(y2_statefips %in% c("96", "97", "98")) |>
  group_by(year, y2_statefips) |>
  summarise(across(c(n1, n2, agi), sum), .groups = "drop") |>
  mutate(
    y1_statefips = "00",
    y2_state = "US",
    y2_state_name = case_when(
      y2_statefips == "96" ~ "US Total Mig - US & For",
      y2_statefips == "97" ~ "US Total Mig - US",
      y2_statefips == "98" ~ "US Total Mig - Foreign"
    ),
    suppressed = FALSE
  )


inflow <- bind_rows(inflow_us, inflow) |>
  select(y2_statefips, y1_statefips, y1_state,
         y1_state_name, n1, n2, agi, year, suppressed) |>
  arrange(year, as.integer(y2_statefips))

outflow <- bind_rows(outflow_us, outflow) |>
  select(y1_statefips, y2_statefips, y2_state,
         y2_state_name, n1, n2, agi, year, suppressed) |>
  arrange(year, as.integer(y1_statefips))



count(inflow, year)
count(outflow, year)

n_distinct(inflow$y2_statefips)
n_distinct(outflow$y1_statefips)

summary(inflow$n1)
summary(outflow$n1)

count(inflow, suppressed)
count(outflow, suppressed)

out_inflow  <- "data/clean/state_inflow_1995_1999.csv"
out_outflow <- "data/clean/state_outflow_1995_1999.csv"

if (nrow(inflow) != 14028 || nrow(outflow) != 14028) {
  stop("Unexpected row counts. Check the cleaning results before saving.")
}

write_csv(inflow,  out_inflow)
write_csv(outflow, out_outflow)

