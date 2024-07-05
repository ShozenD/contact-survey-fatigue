#' Preprocess the COVIMOD data for the GAM model
#'
#' @param w wave
#' @param part participant data (data.table)
#' @param hh household contacts (data.table)
#' @param nhh non-household contacts (data.table)
#' @param nuts NUTS regions (data.table)
#'
#' @return processed contact data (data.table)
#' @export
preproc_gam_data <- function(w, part, hh, nhh, nuts) {
  # Count the number of previous participations for each participant
  setkeyv(part, cols = c("new_id", "wave"))
  part[, rep := seq_len(.N) - 1, by = .(new_id)]

  # Fiter data by wave
  part <- part[wave == w]
  hh <- hh[wave == w]
  nhh <- nhh[wave == w]

  # Sort part by new_id
  part <- part[order(new_id)]

  # Remove participants with missing values for age and gender
  part <- part[!is.na(age_strata)]
  part <- part[!is.na(gender)]
  part <- part[age_strata != "85+"]

  # Impute missing ages for children
  # Preprocess age_strata and job variables
  part <- fill_missing_child_ages(part, seed = 123)
  part <- preproc_age_job(part)

  # Preprocess household size variable
  part[, hh_size := fifelse(hh_p_incl_0 >= 5, "5+", as.character(hh_p_incl_0))]
  part[, hh_p_incl_0 := NULL]
  part[, hh_size := factor(hh_size, levels = c("3", "1", "2", "4", "5+"))]

  # Calculate day of week
  part[, dow := lubridate::wday(date, label = TRUE)]
  part[, dow := ifelse(dow %in% c("Sat", "Sun"), "weekend", "weekday")]

  # Merge NUTS info
  nuts <- as.data.table(nuts)
  nuts <- nuts[LEVL_CODE == 3 & CNTR_CODE == "DE", .(NUTS_NAME, URBN_TYPE)]
  part <- merge(part, nuts, by = "NUTS_NAME", all.x = TRUE)
  part[, urbn_type := case_when(URBN_TYPE == "1" ~ "urban",
                                URBN_TYPE == "2" ~ "intermediate",
                                URBN_TYPE == "3" ~ "rural")]
  part[, URBN_TYPE := NULL]

  # ===== Prepare contact count vector Y =====
  # For nhh Count the number of rows (contacts) by participant and alter_age_strata
  nhh_sum <- nhh[, .(y_nhh = .N), by = new_id]

  # For hh sum the number of rows (contacts) by participant and alter_age_strata
  hh_sum <- hh[, .(y_hh = sum(hh_met_this_day)), by = new_id]

  # In part, sum the values in columns Q75_u18_work to Q75_o64_else and save it as y_grp
  SDcols <- colnames(part)[str_detect(colnames(part), "Q")]
  part[, y_grp := rowSums(.SD, na.rm = TRUE), .SDcols = SDcols]
  grp_sum <- part[, .(new_id, y_grp)]

  # Merge the three data.tables
  cnt_sum <- merge(grp_sum, hh_sum, by = "new_id", all.x = TRUE)
  cnt_sum <- merge(cnt_sum, nhh_sum, by = "new_id", all.x = TRUE)

  # Replace missing values with 0
  cnt_sum[is.na(y_grp), y_grp := 0]
  cnt_sum[is.na(y_hh), y_hh := 0]
  cnt_sum[is.na(y_nhh), y_nhh := 0]

  # Sum the three columns to get the total number of contacts
  cnt_sum[, y := y_grp + y_hh + y_nhh]

  # Merge the y vector with the participant data
  part <- merge(part, cnt_sum, by = "new_id")

  # Remove extreme outliers
  final <- part[y < quantile(part$y, 0.95)]

  return(final)
}
