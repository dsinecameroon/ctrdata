#' Export Multiple REDCap Records and Group Checkbox Variables
#'
#' Retrieves multiple REDCap records matching one or more values of a given field,
#' and automatically groups checkbox dummy columns (with `___` separators) into their
#' corresponding main variable columns. Grouped variables are relocated to appear
#' immediately before their related dummy columns.
#'
#' @param field_name Character string. The field name to search (e.g., `'unique_ctr_id'`).
#' @param field_value Character vector. One or more values to match in `field_name`.
#'
#' @return A data frame containing the full REDCap records for all matching IDs,
#'   with additional grouped columns for checkbox variables.
#'
#' @export
rc_export_multiple_record_with_groups <- function(field_name, field_value) {
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  if (is.na(api_url) || is.na(api_token) || api_url == "" || api_token == "") {
    stop("REDCap API configuration missing. Use rc_config(api_url, api_token) first.")
  }

  # ---- Step 1: Get all records with the target field ----
  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         exportSurveyFields = 'true',
                         fields = paste(c('record_id', field_name), collapse = ",")
                       ),
                       encode = "form"
  )
  data <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))

  # Match all values in field_value (vector)
  matching_record <- data[data[[field_name]] %in% field_value, , drop = FALSE]
  if (nrow(matching_record) == 0) {
    warning("No records found with the specified field values.")
    return(NULL)
  }

  record_ids <- matching_record$record_id
  message(paste("Starting the extraction of", length(record_ids), "records"))

  # ---- Step 2: Retrieve full records ----
  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         exportSurveyFields = 'true',
                         records = paste(record_ids, collapse = ","),
                         rawOrLabel = "raw",
                         rawOrLabelHeaders = "raw",
                         exportCheckboxLabel = "false"
                       ),
                       encode = "form"
  )

  full_record <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))
  full_record <- ctr_dates_reconciliation(full_record)
  full_record <- full_record[, names(full_record)[!grepl("prectr|ctr_login|redcap", names(full_record))]]

  # ---- Step 3: Detect and group checkbox dummies ----
  base_fields <- unique(sub("___.*$", "", grep("___", names(full_record), value = TRUE)))

  for (field in base_fields) {
    chk_cols <- grep(paste0("^", field, "___"), names(full_record), value = TRUE)
    if (length(chk_cols) > 0) {
      # Create grouped column with selected codes per row
      full_record[[field]] <- apply(full_record[chk_cols], 1, function(row) {
        selected <- sub(paste0("^", field, "___"), "", chk_cols[row == 1])
        paste(selected, collapse = ",")
      })

      # Relocate grouped column immediately BEFORE the first dummy
      col_order <- names(full_record)
      idx <- which(col_order %in% chk_cols)
      first_idx <- min(idx)
      col_order <- setdiff(col_order, field)
      new_order <- append(col_order, values = field, after = first_idx - 1)
      full_record <- full_record[, new_order]
    }
  }

  return(full_record)
}
