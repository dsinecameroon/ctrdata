#' Reconcile and Standardize REDCap Timestamp Fields
#'
#' This function identifies all timestamp-related columns in a REDCap dataset, attempts to reconcile
#' missing or alternative values using archival versions, and creates a unified `date_survey` field
#' representing the most relevant date per record.
#'
#' @param data A data frame returned from a REDCap API call, containing timestamp fields.
#'
#' @return A data frame with:
#' - All original columns (with timestamps coerced to `POSIXct`)
#' - Reconciled timestamp fields using `_arch` versions where applicable
#' - A new column `date_survey` representing the latest non-missing timestamp
#' - Columns reordered with `record_id` and `date_survey` first
#'
#' @details
#' - Timestamp columns are detected by matching names that end with `"timestamp"`.
#' - For each such field, if the value is `NA` or blank, the function checks for an archival version
#'   named like `"fieldname_timestamp_arch"` and uses that instead.
#' - The new `date_survey` column holds the latest (`max`) timestamp value across all primary and archival
#'   timestamp fields per row.
#'
#' Assumes timestamp format is `%Y-%m-%d %H:%M:%S`.
#'
#' @examples
#' \dontrun{
#' redcap_data <- rc_export_records()
#' reconciled <- ctr_dates_reconciliation(redcap_data)
#' }
#'
ctr_dates_reconciliation <- function(data) {
  # Step 1: Identify main timestamp fields
  pattern <- "(?<=timestamp$)"
  main_date_var <- names(data)[grepl(pattern, names(data), perl = TRUE)]

  # Step 2: Convert all timestamp fields to POSIXct
  data <- data %>%
    dplyr::mutate(across(dplyr::matches("timestamp"), ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))

  # Step 3: Reconcile each timestamp with its archival version (if NA or blank)
  for (i in seq_along(main_date_var)) {
    original <- sym(main_date_var[i])
    fallback <- sym(paste(main_date_var[i], "arch", sep = "_"))

    data <- data %>%
      dplyr::mutate(
        !!original := dplyr::if_else(
          !is.na(!!original) & as.character(!!original) != "",
          !!original,
          !!fallback
        )
      )

  }

  # Step 4: Create `date_survey` as the max non-NA timestamp across all relevant columns
  combined_fields <- c(main_date_var, paste(main_date_var, "arch", sep = "_"))
  data[["date_survey"]] <- apply(data[, combined_fields], 1, function(x) suppressWarnings(max(x, na.rm = TRUE)))

  # Step 5: Reorder columns: record_id, date_survey, then the rest
  data <- data[, c("record_id", "date_survey", setdiff(names(data), c("record_id", "date_survey")))]

  data <- data[,names(data)[!grepl("_arch", names(data))]]

  return(data)
}
