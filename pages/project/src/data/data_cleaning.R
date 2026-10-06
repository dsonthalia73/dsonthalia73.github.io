################################################################################
# Script Name: data_cleaning.R
# Author: Dhriti Sonthalia
# GitHub: <your-gh-username>
# Date Created: 2026-10-05
#
# Purpose: Cleans the raw College Scorecard pull (from data_pull.R) for the
# test-optional admissions project: labels sector and test policy, adds an
# academic-year label, and flags institutions present in all three years.
# Saves pages/project/data/cleaned/scorecard_cleaned.Rds.
#
#                       IMPORTANT
# Do not overwrite any data files. Raw files should always remain raw and
# untouched. This script only reads scorecard_raw.Rds and writes a separate
# _cleaned file. It does not call the API.
#
################################################################################
# Load necessary libraries/source any functions
library(dplyr)
library(here)

################################################################################
# read raw data with {here} for path management
scorecard_raw <- readRDS(here("pages", "project", "data", "scorecard_raw.Rds"))

n_years_pulled <- n_distinct(scorecard_raw$year)

################################################################################
# clean data

scorecard_cleaned <-
  scorecard_raw |>
  mutate(
    # "2018" -> "2018-19"
    academic_year = paste0(year, "-", substr(year + 1, 3, 4)),
    # control codes: 1 = public, 2 = private nonprofit (for-profits filtered in pull)
    sector = recode(control, `1` = "Public", `2` = "Private nonprofit"),
    # admcon7 test-score requirement codes.
    # VERIFY against sc_dict("admcon7") output before relying on this.
    test_policy = case_when(
      admcon7 == 1          ~ "Required",
      admcon7 %in% c(2, 5)  ~ "Optional",          # recommended + considered-not-required; IPEDS dropped "recommended" after 2021-22
      admcon7 == 3          ~ "Test-blind",
      TRUE                  ~ NA_character_
    ),
    test_policy = factor(test_policy, levels = c("Required", "Optional", "Test-blind"))
  ) |>
  group_by(unitid) |>
  mutate(policy_all_years = sum(!is.na(test_policy)) == n_years_pulled) |>
  ungroup()

scorecard_cleaned <- scorecard_cleaned |>
  group_by(unitid) |>
  mutate(
    policy_path = if_else(
      policy_all_years,
      paste(test_policy[order(year)], collapse = " → "),
      NA_character_
    ),
    p18 = first(test_policy[year == 2018]),
    p21 = first(test_policy[year == 2021]),
    p24 = first(test_policy[year == 2024]),
    policy_group = case_when(
      !policy_all_years                                         ~ NA_character_,
      p18 == "Required" & p21 == "Required" & p24 == "Required" ~ "Always required",
      p18 == "Required" & p24 == "Required"                     ~ "Reinstated",
      p18 == "Required"                                         ~ "Dropped requirement",
      p24 != "Required"                                         ~ "Test-free by 2018",
      TRUE                                                      ~ "Adopted requirement"
    )
  ) |>
  select(-p18, -p21, -p24) |>
  ungroup()

################################################################################
# save cleaned data as .Rds file with {here} for path management

dir.create(here("pages", "project", "data", "cleaned"),
           recursive = TRUE, showWarnings = FALSE)

saveRDS(
  object = scorecard_cleaned,
  file = here("pages", "project", "data", "cleaned", "scorecard_cleaned.Rds")
)

################################################################################
# End of script
