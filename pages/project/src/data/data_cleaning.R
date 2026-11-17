################################################################################
# Script Name: data_cleaning.R
# Author: Dhriti Sonthalia
# GitHub: dsonthalia73
# Date Created: 2026-10-05
#
# Purpose: Cleans the raw College Scorecard pull (from data_pull.R) for the
# test-optional admissions project: labels sector and test policy, adds an
# academic-year label, flags institutions present in all three years, and adds
# analysis flags (consistent SAT reporters, policy status, small and
# special-focus institutions, analysis sample). No rows are dropped.
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
# analysis flags
# No rows are dropped here. Charts should filter on these flags so every plot
# uses the same institutions:
#   analysis_sample             -> default sample for all charts
#   analysis_sample & sat_all_years -> any chart of SAT/ACT scores

min_ugds <- 200  # enrollment floor: below this, rates such as ret_ft4 swing on a handful of students

scorecard_cleaned <-
  scorecard_cleaned |>
  group_by(unitid) |>
  mutate(
    # 1. Test-blind schools report no scores and optional schools report
    #    submitters only, so the set of schools behind sat_avg shrinks after
    #    2018-19. TRUE = sat_avg reported in every year (a fixed set to compare).
    sat_all_years = sum(!is.na(sat_avg)) == n_years_pulled,
    # 2. Say why an institution has no policy_group instead of leaving a bare NA.
    #    "No admissions data" = neither admcon7 nor adm_rate in any year
    #    (most likely open admission; openadmp was not pulled, so not confirmed).
    policy_status = case_when(
      policy_all_years                      ~ "Policy reported all years",
      all(is.na(admcon7) & is.na(adm_rate)) ~ "No admissions data",
      TRUE                                  ~ "Policy missing some years"
    ),
    # 4. Small or specialised institutions add noise.
    #    small_inst: under min_ugds undergraduates (or missing) in any year.
    #    special_focus: Carnegie basic 24-32 (seminaries, art, health, etc.) or
    #    not classified (-2). ccbasic is the institution's current classification.
    small_inst    = !all(!is.na(ugds) & ugds >= min_ugds),
    special_focus = any(ccbasic %in% 24:32 | ccbasic < 0)
  ) |>
  ungroup() |>
  mutate(
    # 3. "Adopted requirement" has only 2 institutions, too few to chart as a
    #    group, so it is kept as a label but left out of the analysis sample.
    #    "Reinstated" is also small: show those schools as named cases, not as
    #    a group average.
    analysis_sample = policy_all_years & !small_inst & !special_focus &
      !(policy_group %in% "Adopted requirement"),
    # fixed order for legends and facets
    policy_group = factor(
      policy_group,
      levels = c("Always required", "Dropped requirement", "Reinstated",
                 "Test-free by 2018", "Adopted requirement")
    )
  )

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
