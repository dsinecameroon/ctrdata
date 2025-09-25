#' Export REDCap Project Logs
#'
#' Retrieves project log events from the REDCap API.
#'
#' This function calls the REDCap API with the `content = 'log'` parameter
#' and returns the project logs in JSON format as an R object.
#' The function requires that the API URL and token be stored in environment
#' variables `RC_API_URL` and `RC_API_TOKEN`.
#'
#' @param log_type Optional character string specifying the type of log events to export.
#'   Valid values include `"record"`, `"manage"`, `"file"`, and others supported by REDCap.
#'   Defaults to `""` (all log types).
#' @param begin_time Optional character string specifying the start date/time for filtering logs.
#'   Must be in the format `"YYYY-MM-DD HH:MM:SS"`. Defaults to `""` (no lower bound).
#' @param end_time Optional character string specifying the end date/time for filtering logs.
#'   Must be in the format `"YYYY-MM-DD HH:MM:SS"`. Defaults to `""` (no upper bound).
#'
#' @return A data frame (or list) of log entries returned by the REDCap API, parsed from JSON.
#'
#' @details
#' The function requires that two environment variables be set:
#' \itemize{
#'   \item \code{RC_API_URL}: the base URL of the REDCap API endpoint.
#'   \item \code{RC_API_TOKEN}: a valid API token for the project.
#' }
#'
#' If the API request fails, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Export all logs
#' logs <- rc_export_logs()
#'
#' # Export only record logs within a date range
#' logs <- rc_export_logs(
#'   log_type   = "record",
#'   begin_time = "2025-01-01 00:00:00",
#'   end_time   = "2025-01-31 23:59:59"
#' )
#' }
#'
#' @seealso \href{https://redcap.vanderbilt.edu/api/}{REDCap API Documentation}
#' @export
rc_export_logs <- function(log_type = "", begin_time = "", end_time = "") {
  # Retrieve API configuration
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

  response <- httr::POST(
    url = api_url,
    body = list(
      token = api_token,
      content = 'log',
      format = 'json',
      logtype = log_type,     # Optional: 'record', 'manage', 'file', etc.
      beginTime = begin_time, # Format: 'YYYY-MM-DD HH:MM:SS'
      endTime = end_time      # Format: 'YYYY-MM-DD HH:MM:SS'
    ),
    encode = "form"
  )

  if (httr::http_error(response)) {
    stop("Failed to export logs: ", httr::content(response, as = "text"))
  }

  logs <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(logs)
}
