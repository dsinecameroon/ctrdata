#' Export a Single REDCap Record by Matching Field Value
#'
#' Searches REDCap records for a value in a specific field and returns the full record that matches.
#' This function uses the API URL and token from environment variables set via `rc_config()`.
#'
#' @param field_name A character string specifying the name of the field to search in (e.g., `"email"` or `"participant_id"`).
#' @param field_value A character string specifying the value to search for in the given field.
#'
#' @return A data frame containing the full record that matches the given field value.
#' Returns `NULL` if no match is found.
#'
#' @details
#' The function performs the following steps:
#' 1. Retrieves only the `record_id` and the specified `field_name` from all records.
#' 2. Filters to find a record where the field matches `field_value` using a partial match (`grepl`).
#' 3. Extracts and returns the full record for the matching `record_id`.
#'
#' The REDCap API URL and token must be set beforehand using `rc_config(api_url, api_token)`.
#'
#' @examples
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' rc_export_single_record(field_name = "email", field_value = "john.doe@example.com")
#'
#' @export
rc_export_single_record <- function(field_name, field_value) {
  # Retrieve API configuration
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  if (is.na(api_url) || is.na(api_token) || api_url == "" || api_token == "") {
    stop("REDCap API configuration missing. Use rc_config(api_url, api_token) first.")
  }

  # Step 1: Get all records with the target field
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

  # Parse the response
  data <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))

  # Step 2: Find the matching record ID
  matching_record <- data[grepl(field_value, data[[field_name]]), , drop = FALSE]

  print(matching_record)

  if (nrow(matching_record) == 0) {
    warning("No record found with the specified field value.")
    return(NULL)
  }

  record_id <- matching_record$record_id

  print(paste("Starting the extraction of record", record_id))

  # Step 3: Retrieve full record using the record_id
  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         exportSurveyFields = 'true',
                         records = paste(record_id, collapse = ",")
                       ),
                       encode = "form"
  )

  full_record <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))

  full_record <- ctr_dates_reconciliation(full_record)

  full_record <- full_record[, names(full_record)[!grepl("prectr|ctr_login|redcap",names(full_record))]]

  return(full_record)
}
