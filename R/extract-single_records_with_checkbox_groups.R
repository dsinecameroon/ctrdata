#' Export a Single REDCap Record and Group Checkbox Variables
#'
#' Retrieves a single REDCap record matching a specified field/value pair,
#' and automatically groups checkbox dummy columns (with `___` separators)
#' into their corresponding main variable columns. The grouped variables
#' are relocated to appear immediately before their related dummy columns.
#'
#' Uses environment variables `RC_API_URL` and `RC_API_TOKEN` set via
#' `rc_config()`.
#'
#' @param field_name Character string. The field name to search for
#'   (e.g., `'unique_ctr_id'`).
#' @param field_value Character string or pattern to match within `field_name`.
#'
#' @return A data frame containing the full REDCap record for the matching
#'   record ID, with an additional grouped column for each checkbox field.
#'   The grouped column contains the selected raw codes joined by commas,
#'   and is placed immediately before the corresponding dummy columns.
#'
#' @details
#' This function performs the following steps:
#' 1. Queries REDCap for all records containing `field_name`.
#' 2. Identifies the record whose `field_name` value matches `field_value`.
#' 3. Retrieves the complete record for the matching record ID.
#' 4. Detects checkbox fields by searching for columns with `___` in their names.
#' 5. Combines each set of dummy columns into a single grouped column
#'    with raw codes (e.g., `"0,2,5"`).
#' 6. Relocates the grouped column immediately before its corresponding
#'    dummy variables.
#'
#' Environment variables `RC_API_URL` and `RC_API_TOKEN` must be set prior to
#' calling this function. Use `rc_config(api_url, api_token)` to configure.
#'
#' @examples
#' \dontrun{
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' df <- rc_export_single_record_with_groups(field_name = "unique_ctr_id",
#'                                           field_value = "HRL001174")
#' head(df)
#' }
#'
#' @export

rc_export_single_record_with_groups <- function(field_name, field_value) {
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

  matching_record <- data[grepl(field_value, data[[field_name]]), , drop = FALSE]
  if (nrow(matching_record) == 0) {
    warning("No record found with the specified field value.")
    return(NULL)
  }

  record_id <- matching_record$record_id
  message(paste("Starting the extraction of record", record_id))

  # ---- Step 2: Retrieve full record ----
  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         exportSurveyFields = 'true',
                         records = paste(record_id, collapse = ","),
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
      # Create grouped column with selected codes
      full_record[[field]] <- apply(full_record[chk_cols], 1, function(row) {
        selected <- sub(paste0("^", field, "___"), "", chk_cols[row == 1])
        paste(selected, collapse = ",")
      })

      # Relocate grouped column immediately BEFORE the first dummy
      col_order <- names(full_record)
      idx <- which(col_order %in% chk_cols)
      first_idx <- min(idx)

      # Remove grouped column from current position if exists
      col_order <- setdiff(col_order, field)

      # Insert grouped field at correct position
      new_order <- append(col_order, values = field, after = first_idx - 1)
      full_record <- full_record[, new_order]
    }
  }

  return(full_record)
}
