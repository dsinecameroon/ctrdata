
#' Extract Timestamp Field Names from a REDCap Project
#'
#' Identifies all field names that end in `"timestamp"` in a REDCap project by inspecting a sample record.
#' This is useful for dynamically selecting timestamp fields without hardcoding them.
#' Requires that the REDCap API environment variables are set via `rc_config()`.
#'
#' @param rec_id A single record ID (usually a number or string) used to pull one record from REDCap.
#' Default is `1`. The record must exist; otherwise, the function may fail.
#'
#' @return A character vector of field names that end with `"timestamp"`.
#'
#' @details
#' - The function sends a REDCap API request to export a single record using the given `rec_id`.
#' - It inspects the field names in the response and returns those that match the pattern `"timestamp$"`.
#' - The function relies on environment variables `RC_API_URL` and `RC_API_TOKEN` set using `rc_config()`.
#'
#' @examples
#' \dontrun{
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' rc_timestamp_var()
#' }
#'
#' @export
rc_timestamp_var <- function(rec_id = 1) {
  # Read environment variables
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  # Validate configuration
  if (is.na(api_url) || api_url == "" || is.na(api_token) || api_token == "") {
    stop("REDCap API configuration is missing. Please run rc_config(api_url, api_token) first.")
  }

  # Pull sample data (single record)
  sample_data <- httr::POST(api_url,
                            body = list(
                              token = api_token,
                              content = 'record',
                              format = 'json',
                              type = 'flat',
                              records = rec_id,
                              exportSurveyFields = 'true'
                            ),
                            encode = "form"
  ) %>% httr::content("parsed")

  # Extract field names ending with "timestamp"
  timestamp_fields <- names(sample_data[[1]])[grepl("timestamp$", names(sample_data[[1]]))]

  return(timestamp_fields)
}
