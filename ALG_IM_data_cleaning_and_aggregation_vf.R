# ============================================================
# Algeria IM data cleaning and aggregation
# Final updated version with full improvements and QC
# ============================================================

# ---- Working directory ----
setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_raw/")

# ---- Libraries ----
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)
library(ggplot2)
library(sf)
library(flextable)

# ============================================================
# Helper: robust mixed date parser
# ============================================================
parse_mixed_dates <- function(x) {
  x <- as.character(x)
  
  suppressWarnings(
    coalesce(
      ymd(x),
      dmy(x),
      mdy(x),
      ymd_hms(x),
      dmy_hms(x),
      mdy_hms(x)
    )
  )
}

# ============================================================
# Helper: safe row sum
# ============================================================
safe_row_sum <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (length(cols) == 0) {
    return(rep(0, nrow(data)))
  }
  rowSums(data[, cols, drop = FALSE], na.rm = TRUE)
}

# ============================================================
# Helper: safe numeric cleaning
# ============================================================
clean_numeric <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("n/a", "NA", "", " ", "null", "NULL")] <- "0"
  suppressWarnings(as.numeric(x))
}

# ============================================================
# Input / output
# ============================================================
input_file  <- "ALG_SIA.csv"
output_file <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/ALG_IM.csv"

# Optional QC export
qc_output_file <- "C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/IM_level/ALG_IM_QC_flags.csv"

# ============================================================
# Read raw data
# ============================================================
DF <- read_csv(input_file, show_col_types = FALSE)

# Keep records where TotalFM is not missing
DF <- DF %>%
  filter(!is.na(TotalFM))

# ============================================================
# Core filter
# ============================================================
GF <- DF %>%
  filter(
    Type_Monitoring == "EndProcess",
    !is.na(Response),
    Response != "",
    Response != "n/a",
    Response != "NA",
    !is.na(roundNumber),
    roundNumber != "",
    roundNumber != "n/a",
    !is.na(Total_U5_Present),
    Total_U5_Present != "n/a"
  )

# ============================================================
# Detect relevant columns dynamically
# ============================================================

# All HH-level columns to clean numerically
hh_cols <- grep("^HH\\[", names(GF), value = TRUE)

# Main HH columns
u5_present_cols <- grep("^HH\\[[0-9]+\\]/Total_U6_Present_HH$", names(GF), value = TRUE)
u5_fm_cols      <- grep("^HH\\[[0-9]+\\]/U6_Vac_FM_HH$", names(GF), value = TRUE)

abs_main_cols      <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Absent_HH$", names(GF), value = TRUE)
nc_main_cols       <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_NC_HH$", names(GF), value = TRUE)
noparent_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Noparent_HH$", names(GF), value = TRUE)
distance_main_cols <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_distance$", names(GF), value = TRUE)
travel_main_cols   <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_travel_HH$", names(GF), value = TRUE)
other_main_cols    <- grep("^HH\\[[0-9]+\\]/group1/Tot_child_Others_HH$", names(GF), value = TRUE)

# Detailed absence HH columns
abs_sick_cols         <- grep("^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Sick$", names(GF), value = TRUE)
abs_school_cols       <- grep("^HH\\[[0-9]+\\]/group2/Tot_child_Abs_School$", names(GF), value = TRUE)
abs_play_cols         <- grep("^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Play_areas$", names(GF), value = TRUE)
abs_social_cols       <- grep("^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Social_event$", names(GF), value = TRUE)
abs_travelling_cols   <- grep("^HH\\[[0-9]+\\]/group2/Tot_child_Abs_Travelling$", names(GF), value = TRUE)
abs_other_detail_cols <- grep("^HH\\[[0-9]+\\]/group2/Other_Reason_Absent$", names(GF), value = TRUE)

# Detailed NC HH columns
nc_child_sick_cols   <- grep("^HH\\[[0-9]+\\]/group4/Tot_child_NC_Child_was_sick$", names(GF), value = TRUE)
nc_not_decide_cols   <- grep("^HH\\[[0-9]+\\]/group4/Tot_child_NC_pas_decide$", names(GF), value = TRUE)
nc_poliofree_cols    <- grep("^HH\\[[0-9]+\\]/group4/Tot_child_NC_PolioFree$", names(GF), value = TRUE)
nc_nopv_cols         <- grep("^HH\\[[0-9]+\\]/group4/Tot_child_NC_nOPV$", names(GF), value = TRUE)
nc_other_detail_cols <- grep("^HH\\[[0-9]+\\]/group4/Tot_child_NC_Other$", names(GF), value = TRUE)

# Form-level total validation columns
abs_other_total_form_col   <- intersect("Tot_child_Abs_Other_T", names(GF))
abs_sick_total_form_col    <- intersect("Tot_child_Abs_Sick_T", names(GF))
abs_school_total_form_col  <- intersect("Tot_child_Abs_School_T", names(GF))
abs_play_total_form_col    <- intersect("Tot_child_Abs_Play_areas_T", names(GF))
abs_social_total_form_col  <- intersect("Tot_child_Abs_SocialEvent", names(GF))
abs_travel_total_form_col  <- intersect("Sum_child_Abs_Travelling", names(GF))

nc_poliofree_total_form_col   <- intersect("Tot_child_NC_PolioFREE_T", names(GF))
nc_sideeffects_total_form_col <- intersect("Tot_child_NC_sideEffects", names(GF))
nc_notdecide_total_form_col   <- intersect("Tot_child_NC_NotDecide_T", names(GF))
nc_childsick_total_form_col   <- intersect("Tot_child_NC_ChildSick_T", names(GF))
nc_nopv_total_form_col        <- intersect("Tot_child_NC_nOPV_T", names(GF))
nc_other_total_form_col       <- intersect("Tot_child_NC_Others_T", names(GF))

# ============================================================
# Clean numeric columns
# ============================================================
GF <- GF %>%
  mutate(
    across(all_of(hh_cols), clean_numeric)
  )

form_total_cols <- unique(c(
  abs_other_total_form_col,
  abs_sick_total_form_col,
  abs_school_total_form_col,
  abs_play_total_form_col,
  abs_social_total_form_col,
  abs_travel_total_form_col,
  nc_poliofree_total_form_col,
  nc_sideeffects_total_form_col,
  nc_notdecide_total_form_col,
  nc_childsick_total_form_col,
  nc_nopv_total_form_col,
  nc_other_total_form_col
))

if (length(form_total_cols) > 0) {
  GF <- GF %>%
    mutate(
      across(all_of(form_total_cols), clean_numeric)
    )
}

GF <- GF %>%
  mutate(
    date_monitored = parse_mixed_dates(date_monitored)
  ) %>%
  filter(!is.na(date_monitored))

# ============================================================
# Reconstruct row-level IM indicators
# ============================================================
GH <- GF %>%
  mutate(
    # Core U5 indicators
    u5_present = safe_row_sum(., u5_present_cols),
    u5_FM_raw  = safe_row_sum(., u5_fm_cols),
    u5_FM      = pmin(u5_FM_raw, u5_present),
    missed_child = pmax(0, u5_present - u5_FM),
    
    # Top-level reasons
    r_non_FM_Absent      = safe_row_sum(., abs_main_cols),
    r_non_FM_NC          = safe_row_sum(., nc_main_cols),
    r_non_FM_hh_Noparent = safe_row_sum(., noparent_main_cols),
    r_non_FM_hh_distance = safe_row_sum(., distance_main_cols),
    r_non_FM_travel      = safe_row_sum(., travel_main_cols),
    r_non_FM_other       = safe_row_sum(., other_main_cols),
    
    # Detailed absence reasons
    r_abs_sick         = safe_row_sum(., abs_sick_cols),
    r_abs_school       = safe_row_sum(., abs_school_cols),
    r_abs_play_areas   = safe_row_sum(., abs_play_cols),
    r_abs_social_event = safe_row_sum(., abs_social_cols),
    r_abs_travelling   = safe_row_sum(., abs_travelling_cols),
    r_abs_other_detail = safe_row_sum(., abs_other_detail_cols),
    
    # Detailed NC reasons
    r_nc_child_sick   = safe_row_sum(., nc_child_sick_cols),
    r_nc_not_decided  = safe_row_sum(., nc_not_decide_cols),
    r_nc_polio_free   = safe_row_sum(., nc_poliofree_cols),
    r_nc_nopv         = safe_row_sum(., nc_nopv_cols),
    r_nc_other_detail = safe_row_sum(., nc_other_detail_cols),
    
    # Form-level total validation fields
    abs_other_total_form   = if (length(abs_other_total_form_col) > 0) .data[[abs_other_total_form_col]] else 0,
    abs_sick_total_form    = if (length(abs_sick_total_form_col) > 0) .data[[abs_sick_total_form_col]] else 0,
    abs_school_total_form  = if (length(abs_school_total_form_col) > 0) .data[[abs_school_total_form_col]] else 0,
    abs_play_total_form    = if (length(abs_play_total_form_col) > 0) .data[[abs_play_total_form_col]] else 0,
    abs_social_total_form  = if (length(abs_social_total_form_col) > 0) .data[[abs_social_total_form_col]] else 0,
    abs_travel_total_form  = if (length(abs_travel_total_form_col) > 0) .data[[abs_travel_total_form_col]] else 0,
    
    nc_poliofree_total_form   = if (length(nc_poliofree_total_form_col) > 0) .data[[nc_poliofree_total_form_col]] else 0,
    nc_sideeffects_total_form = if (length(nc_sideeffects_total_form_col) > 0) .data[[nc_sideeffects_total_form_col]] else 0,
    nc_notdecide_total_form   = if (length(nc_notdecide_total_form_col) > 0) .data[[nc_notdecide_total_form_col]] else 0,
    nc_childsick_total_form   = if (length(nc_childsick_total_form_col) > 0) .data[[nc_childsick_total_form_col]] else 0,
    nc_nopv_total_form        = if (length(nc_nopv_total_form_col) > 0) .data[[nc_nopv_total_form_col]] else 0,
    nc_other_total_form       = if (length(nc_other_total_form_col) > 0) .data[[nc_other_total_form_col]] else 0
  ) %>%
  mutate(
    # Detailed totals from HH-level counts
    abs_detail_total = r_abs_sick + r_abs_school + r_abs_play_areas +
      r_abs_social_event + r_abs_travelling + r_abs_other_detail,
    
    nc_detail_total = r_nc_child_sick + r_nc_not_decided + r_nc_polio_free +
      r_nc_nopv + r_nc_other_detail
  )

# ============================================================
# Aggregate daily level first
# ============================================================
daily_im <- GH %>%
  select(
    Country, Region, District, Response, roundNumber, date_monitored,
    u5_present, u5_FM, missed_child,
    
    r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_Noparent,
    r_non_FM_hh_distance, r_non_FM_travel, r_non_FM_other,
    
    r_abs_sick, r_abs_school, r_abs_play_areas,
    r_abs_social_event, r_abs_travelling, r_abs_other_detail,
    
    r_nc_child_sick, r_nc_not_decided, r_nc_polio_free,
    r_nc_nopv, r_nc_other_detail,
    
    abs_detail_total, nc_detail_total,
    
    abs_other_total_form, abs_sick_total_form, abs_school_total_form,
    abs_play_total_form, abs_social_total_form, abs_travel_total_form,
    
    nc_poliofree_total_form, nc_sideeffects_total_form,
    nc_notdecide_total_form, nc_childsick_total_form,
    nc_nopv_total_form, nc_other_total_form
  ) %>%
  group_by(Country, Region, District, Response, roundNumber, date_monitored) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ============================================================
# Build periods of consecutive monitoring days
# ============================================================
period_im <- daily_im %>%
  group_by(Country, Region, District, Response, roundNumber) %>%
  arrange(date_monitored, .by_group = TRUE) %>%
  mutate(
    gap_days = as.integer(date_monitored - lag(date_monitored)),
    new_period = if_else(is.na(gap_days) | gap_days != 1, 1L, 0L),
    period = cumsum(new_period)
  ) %>%
  ungroup()

# ============================================================
# Aggregate by monitoring period
# ============================================================
final_im <- period_im %>%
  group_by(Country, Region, District, Response, roundNumber, period) %>%
  summarise(
    start_date = min(date_monitored, na.rm = TRUE),
    end_date   = max(date_monitored, na.rm = TRUE),
    
    u5_present = sum(u5_present, na.rm = TRUE),
    u5_FM = sum(u5_FM, na.rm = TRUE),
    missed_child = sum(missed_child, na.rm = TRUE),
    
    r_non_FM_Absent = sum(r_non_FM_Absent, na.rm = TRUE),
    r_non_FM_NC = sum(r_non_FM_NC, na.rm = TRUE),
    r_non_FM_hh_Noparent = sum(r_non_FM_hh_Noparent, na.rm = TRUE),
    r_non_FM_hh_distance = sum(r_non_FM_hh_distance, na.rm = TRUE),
    r_non_FM_travel = sum(r_non_FM_travel, na.rm = TRUE),
    r_non_FM_other = sum(r_non_FM_other, na.rm = TRUE),
    
    r_abs_sick = sum(r_abs_sick, na.rm = TRUE),
    r_abs_school = sum(r_abs_school, na.rm = TRUE),
    r_abs_play_areas = sum(r_abs_play_areas, na.rm = TRUE),
    r_abs_social_event = sum(r_abs_social_event, na.rm = TRUE),
    r_abs_travelling = sum(r_abs_travelling, na.rm = TRUE),
    r_abs_other_detail = sum(r_abs_other_detail, na.rm = TRUE),
    
    r_nc_child_sick = sum(r_nc_child_sick, na.rm = TRUE),
    r_nc_not_decided = sum(r_nc_not_decided, na.rm = TRUE),
    r_nc_polio_free = sum(r_nc_polio_free, na.rm = TRUE),
    r_nc_nopv = sum(r_nc_nopv, na.rm = TRUE),
    r_nc_other_detail = sum(r_nc_other_detail, na.rm = TRUE),
    
    abs_detail_total = sum(abs_detail_total, na.rm = TRUE),
    nc_detail_total = sum(nc_detail_total, na.rm = TRUE),
    
    abs_other_total_form = sum(abs_other_total_form, na.rm = TRUE),
    abs_sick_total_form = sum(abs_sick_total_form, na.rm = TRUE),
    abs_school_total_form = sum(abs_school_total_form, na.rm = TRUE),
    abs_play_total_form = sum(abs_play_total_form, na.rm = TRUE),
    abs_social_total_form = sum(abs_social_total_form, na.rm = TRUE),
    abs_travel_total_form = sum(abs_travel_total_form, na.rm = TRUE),
    
    nc_poliofree_total_form = sum(nc_poliofree_total_form, na.rm = TRUE),
    nc_sideeffects_total_form = sum(nc_sideeffects_total_form, na.rm = TRUE),
    nc_notdecide_total_form = sum(nc_notdecide_total_form, na.rm = TRUE),
    nc_childsick_total_form = sum(nc_childsick_total_form, na.rm = TRUE),
    nc_nopv_total_form = sum(nc_nopv_total_form, na.rm = TRUE),
    nc_other_total_form = sum(nc_other_total_form, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cv = ifelse(u5_present > 0, round(u5_FM / u5_present, 2), NA_real_),
    year = year(start_date),
    
    # Top-level reconciliation
    reasons_total = r_non_FM_Absent + r_non_FM_NC + r_non_FM_hh_Noparent +
      r_non_FM_hh_distance + r_non_FM_travel + r_non_FM_other,
    
    check_missed = missed_child - reasons_total,
    
    # Key QC metrics
    unexplained_missed   = pmax(check_missed, 0),
    overreported_reasons = pmax(-check_missed, 0),
    
    explained_ratio = ifelse(missed_child > 0, round(reasons_total / missed_child, 3), NA_real_),
    unexplained_ratio = ifelse(missed_child > 0, round(unexplained_missed / missed_child, 3), NA_real_),
    
    # Detailed reconciliation against top-level
    check_abs_detail = r_non_FM_Absent - abs_detail_total,
    check_nc_detail  = r_non_FM_NC - nc_detail_total,
    
    # HH detailed totals versus form totals
    check_abs_other_total  = r_abs_other_detail - abs_other_total_form,
    check_abs_sick_total   = r_abs_sick - abs_sick_total_form,
    check_abs_school_total = r_abs_school - abs_school_total_form,
    check_abs_play_total   = r_abs_play_areas - abs_play_total_form,
    check_abs_social_total = r_abs_social_event - abs_social_total_form,
    check_abs_travel_total = r_abs_travelling - abs_travel_total_form,
    
    check_nc_childsick_total = r_nc_child_sick - nc_childsick_total_form,
    check_nc_notdecide_total = r_nc_not_decided - nc_notdecide_total_form,
    check_nc_poliofree_total = r_nc_polio_free - nc_poliofree_total_form,
    check_nc_nopv_total      = r_nc_nopv - nc_nopv_total_form,
    check_nc_other_total     = r_nc_other_detail - nc_other_total_form,
    
    reconciliation_flag = case_when(
      check_missed == 0 ~ "Consistent",
      check_missed > 0 & reasons_total == 0 ~ "No reasons recorded",
      check_missed > 0 ~ "Partial reasons recorded",
      check_missed < 0 ~ "Overlapping reasons",
      TRUE ~ "Unknown"
    ),
    
    abs_detail_flag = case_when(
      check_abs_detail == 0 ~ "Abs detail consistent",
      check_abs_detail > 0 ~ "Abs detail incomplete",
      check_abs_detail < 0 ~ "Abs detail overlapping",
      TRUE ~ "Unknown"
    ),
    
    nc_detail_flag = case_when(
      check_nc_detail == 0 ~ "NC detail consistent",
      check_nc_detail > 0 ~ "NC detail incomplete",
      check_nc_detail < 0 ~ "NC detail overlapping",
      TRUE ~ "Unknown"
    ),
    
    qc_flag = case_when(
      check_missed == 0 & check_abs_detail == 0 & check_nc_detail == 0 ~ "OK",
      TRUE ~ "Needs review"
    )
  ) %>%
  filter(!is.na(start_date), start_date >= as.Date("2019-10-01")) %>%
  arrange(start_date)

# ============================================================
# Optional vaccine type derivation
# ============================================================
final_im <- final_im %>%
  mutate(
    Vaccine_type = case_when(
      str_detect(Response, regex("nOPV|VPOn|nVPO", ignore_case = TRUE)) ~ "nOPV2",
      str_detect(Response, regex("bOPV|WPV1|VPOb|OPV", ignore_case = TRUE)) ~ "bOPV",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# Final output
# ============================================================
ALG_IM <- final_im %>%
  select(
    Country, Region, District, Response, roundNumber, Vaccine_type,
    start_date, end_date, year,
    u5_present, u5_FM, missed_child, cv,
    
    # Top-level reasons
    r_non_FM_Absent, r_non_FM_NC, r_non_FM_hh_Noparent,
    r_non_FM_hh_distance, r_non_FM_travel, r_non_FM_other,
    
    # Detailed absence reasons
    r_abs_sick, r_abs_school, r_abs_play_areas,
    r_abs_social_event, r_abs_travelling, r_abs_other_detail,
    
    # Detailed NC reasons
    r_nc_child_sick, r_nc_not_decided, r_nc_polio_free,
    r_nc_nopv, r_nc_other_detail,
    
    # Totals and ratios
    abs_detail_total, nc_detail_total, reasons_total,
    explained_ratio, unexplained_ratio,
    unexplained_missed, overreported_reasons,
    
    # Form-level validation totals
    abs_other_total_form, abs_sick_total_form, abs_school_total_form,
    abs_play_total_form, abs_social_total_form, abs_travel_total_form,
    nc_childsick_total_form, nc_notdecide_total_form,
    nc_poliofree_total_form, nc_nopv_total_form, nc_other_total_form,
    nc_sideeffects_total_form,
    
    # QC checks
    check_missed, check_abs_detail, check_nc_detail,
    check_abs_other_total, check_abs_sick_total, check_abs_school_total,
    check_abs_play_total, check_abs_social_total, check_abs_travel_total,
    check_nc_childsick_total, check_nc_notdecide_total,
    check_nc_poliofree_total, check_nc_nopv_total, check_nc_other_total,
    
    # QC flags
    reconciliation_flag, abs_detail_flag, nc_detail_flag, qc_flag
  )

# ============================================================
# QC extract
# ============================================================
ALG_IM_QC <- ALG_IM %>%
  filter(
    qc_flag == "Needs review" |
      abs(check_missed) >= 5 |
      abs(check_abs_detail) >= 3 |
      abs(check_nc_detail) >= 3
  ) %>%
  arrange(desc(abs(check_missed)), desc(abs(check_abs_detail)), desc(abs(check_nc_detail)))

# ============================================================
# Export
# ============================================================
write_csv(ALG_IM, output_file)
write_csv(ALG_IM_QC, qc_output_file)

cat("Final updated Algeria IM file saved to:\n", output_file, "\n")
cat("QC review file saved to:\n", qc_output_file, "\n")

# ============================================================
# Optional quick summaries
# ============================================================
cat("\n--- QC Summary ---\n")
print(ALG_IM %>% count(reconciliation_flag))
print(ALG_IM %>% count(abs_detail_flag))
print(ALG_IM %>% count(nc_detail_flag))
print(ALG_IM %>% count(qc_flag))