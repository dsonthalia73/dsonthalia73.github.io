################################################################################
# Script Name: data_pull.R
# Author: Dhriti Sonthalia
# GitHub: <your-gh-username>
# Date Created: 2026-10-05
#
# Purpose: Pulls College Scorecard data (2018-19, 2021-22, 2024-25) for
# bachelor's-granting public and private nonprofit institutions via the
# {rscorecard} API wrapper, and saves the untouched result as a raw .Rds file.
#
#                       IMPORTANT
# Run this script MANUALLY, once. It is NOT source()'d by any page, so the
# site never calls the API on render. It will not overwrite an existing raw
# file; delete pages/project/data/scorecard_raw.Rds to force a fresh pull.
# API key is read from ~/.Renviron (SCORECARD_KEY); never hardcode it.
#
################################################################################
# Load necessary libraries
library(rscorecard)
library(dplyr)
library(purrr)
library(here)

sc_key(Sys.getenv("SCORECARD_KEY"))

raw_path <- here("pages", "project", "data", "scorecard_raw.Rds")

################################################################################
# Check field names and value labels before pulling.
# Read admcon7's codes carefully: data_cleaning.R's recode depends on them.
# Also confirm in ?sc_year that 2018 maps to the 2018-19 file.

sc_dict("admcon7")
sc_dict("adm_rate")
sc_dict("sat_avg")
sc_dict("pctpell")

################################################################################
# Pull

years <- c(2018, 2021, 2024)

vars <- c(
  # identifiers
  "unitid", "instnm", "stabbr", "control", "preddeg", "ccbasic",
  # admissions / test policy
  "admcon7", "adm_rate", "sat_avg",
  "satvr25", "satvr75", "satmt25", "satmt75", "actcm25", "actcm75",
  # who enrolls
  "ugds", "pctpell",
  "ugds_white", "ugds_black", "ugds_hisp", "ugds_asian"
)

pull_year <- function(yr) {
  message("Pulling ", yr, " ...")
  out <- sc_init() |>
    sc_filter(preddeg == 3, control == c(1, 2)) |>  # bachelor's; public + private nonprofit
    sc_select_(vars) |>
    sc_year(yr) |>
    sc_get()
  Sys.sleep(2)                                      # be polite to the API
  mutate(out, year = yr)                            # explicit year column
}

################################################################################
# Save raw data (never overwrite)

if (!file.exists(raw_path)) {
  scorecard_raw <- map_dfr(years, pull_year)
  saveRDS(object = scorecard_raw, file = raw_path)
} else {
  message("Raw file already exists; not overwriting: ", raw_path)
  scorecard_raw <- readRDS(raw_path)
}

################################################################################
# Sanity checks

count(scorecard_raw, year)               # rows per year should be similar
count(scorecard_raw, year, admcon7)      # policy distribution by year
scorecard_raw |>
  group_by(year) |>
  summarise(across(c(admcon7, adm_rate, sat_avg, pctpell),
                   ~ mean(is.na(.x)), .names = "na_{.col}"))

################################################################################
# End of script
