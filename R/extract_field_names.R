#' Export REDCap Field Names Matching a Pattern
#'
#' Extracts field names from REDCap metadata that match a given pattern.
#' Uses the REDCap API URL and token from environment variables set by `rc_config()`.
#'
#' @param pattern A string pattern to match field names (e.g., "timestamp").
#'
#' @return A character vector of matching field names.
#'
#' @examples
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' rc_export_fields_name("timestamp")
#'
#' @export
rc_export_fields_name <- function(pattern) {
  # Read environment variables
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  # Validate configuration
  if (is.na(api_url) || api_url == "" || is.na(api_token) || api_token == "") {
    stop("REDCap API configuration is missing. Please run rc_config(api_url, api_token) first.")
  }

  # Metadata extraction
  meta <- httr::POST(api_url,
                     body = list(
                       token = api_token,
                       content = 'metadata',
                       format = 'json'
                     ),
                     encode = "form"
  ) %>%
    httr::content("parsed")

  # Match field names using the pattern
  matched <- sapply(meta, function(x) grepl(pattern, x$field_name))
  field_names <- sapply(meta[matched], function(x) x$field_name)

  return(field_names)
}
