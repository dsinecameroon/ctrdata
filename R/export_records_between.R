#' Export REDCap Records Within a Date Range
#'
#' Extracts all REDCap records whose survey timestamp falls between the specified start and end dates.
#' Uses environment variables `RC_API_URL` and `RC_API_TOKEN` set via `rc_config()`. It relies on a helper
#' function `rc_timestamp_var()` to detect timestamp fields and assumes a `date_survey` column is created
#' or standardized via `ctr_dates_reconciliation()`.
#'
#' @param record_id A string indicating the name of the record ID field. Default is `'record_id'`.
#' @param start_date The start date (inclusive) in `"YYYY-MM-DD"` format.
#' @param end_date The end date (inclusive) in `"YYYY-MM-DD"` format.
#'
#' @return A data frame of all REDCap records whose survey timestamps fall within the given date range.
#'
#' @details
#' This function performs three steps:
#' 1. Retrieves record IDs and timestamp fields using `rc_timestamp_var()`.
#' 2. Filters records between `start_date` and `end_date` based on `date_survey`.
#' 3. Fetches the full record data for only the matching records.
#'
#' Environment variables `RC_API_URL` and `RC_API_TOKEN` must be set before calling this function.
#' Timestamps must be parsable as `Date` objects.
#'
#' @examples
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' rc_export_records_between(start_date = "2024-01-01", end_date = "2024-03-31")
#'
#' @export
rc_export_records_between <- function(record_id = 'record_id', start_date, end_date) {
  # Retrieve API configuration
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  if (is.na(api_url) || is.na(api_token) || api_url == "" || api_token == "") {
    stop("REDCap API configuration missing. Use rc_config(api_url, api_token) first.")
  }

  # Step 1: Extract timestamp fields
  date_fields <- rc_timestamp_var(rec_id = 1)

  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         exportSurveyFields = 'true',
                         fields = paste(c(record_id, paste(date_fields, "arch", sep = "_")), collapse = ',')
                       ),
                       encode = "form"
  )

  data <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))

  # Step 2: Filter records between dates
  date_fields <- names(data)[grepl("timestamp", names(data))]
  for (d in date_fields) {
    data[[d]] <- as.Date(data[[d]], format = "%Y-%m-%d")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  filtered_data <- subset(data, data[["date_survey"]] >= start_date & data[["date_survey"]] <= end_date)

  # Step 3: Extract full records for those filtered
  result <- httr::POST(api_url,
                       body = list(
                         token = api_token,
                         content = 'record',
                         format = 'json',
                         type = 'flat',
                         records = paste(filtered_data$record_id, collapse = ","),
                         exportSurveyFields = 'true'
                       ),
                       encode = "form"
  )

  data <- jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8"))
  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  data <- data[, names(data)[!grepl("prectr|ctr_login|redcap",names(data))]]

  return(data)
}
