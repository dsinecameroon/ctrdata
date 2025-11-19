#' Export REDCap Records Within a Date Range
#'
#' Extracts all REDCap records whose survey timestamp falls between the specified start and end dates.
#' Uses environment variables `RC_API_URL` and `RC_API_TOKEN` set via `rc_config()`. It relies on a helper
#' function `rc_timestamp_var()` to detect timestamp fields and assumes a `date_survey` column is created
#' or standardized via `ctr_dates_reconciliation()`.
#'
#' If no parameters are provided, the function will export **all records in the REDCap project**.
#' This may take a long time depending on your internet speed and computer performance.
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
#' If `start_date` and `end_date` are omitted, **the entire REDCap database is downloaded**.
#' This feature is helpful for full project extraction but may require several minutes for large databases.
#'
#' Environment variables `RC_API_URL` and `RC_API_TOKEN` must be set before calling this function.
#' Timestamps must be parsable as `Date` objects.
#'
#' @examples
#' # Configure REDCap connection
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#'
#' # Export records between two dates
#' rc_export_records_between(start_date = "2024-01-01", end_date = "2024-03-31")
#'
#' # Export the full REDCap database (may be slow)
#' data <- rc_export_records_between()

#' @export

rc_export_records_between <- function(record_id = 'record_id', start_date=NULL, end_date=NULL) {
  # Retrieve API configuration
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)
  origin <- "2022-06-09"

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

  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  # Dealing with date entries
  if(is.null(start_date))
    start_date <- origin

  if(is.null(end_date))
    end_date <- lubridate::today()

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if(end_date < start_date)
    stop("Error: The end date is before the start date.")

  filtered_data <- subset(data, data[["date_survey"]] >= start_date & data[["date_survey"]] <= end_date)

  # Step 3: Extract full records for those filtered

  record_ids <- sort(as.numeric(filtered_data$record_id))
  block_size <- 5000
  record_list <- split(record_ids, ceiling(seq_along(record_ids) / block_size))

  result_list <- lapply(record_list, function(recs){

                        print(paste("Extracting records between", min(recs), "and", max(recs)))
                        httr::POST(api_url,
                                   body = list(
                                     token = api_token,
                                     content = 'record',
                                     format = 'json',
                                     type = 'flat',
                                     records = paste(recs, collapse = ","),
                                     exportSurveyFields = 'true',
                                     rawOrLabel = "raw",
                                     rawOrLabelHeaders = "raw",
                                     exportCheckboxLabel = "false"
                                   ),
                                   encode = "form"
                        )

                        # print(paste("Records", min(recs),  "to", max(recs), "extracted."))
                        }

                        )

  data_list <- lapply(result_list,\(result)
                 jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8")))
  # Merge all
  data <- data.table::rbindlist(data_list, fill = T)

  data <- as.data.frame(data)

  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  data <- subset(data, select = names(data)[!grepl("prectr|ctr_login|redcap",names(data))])


  return(data)
}



#' Export REDCap Records Between Two Dates (With Grouped Retrieval)
#'
#' This function extracts REDCap records whose survey timestamps fall between
#' a specified start and end date. It first performs a lightweight extraction
#' to identify eligible `record_id` values, then downloads the full records in
#' grouped batches using `rc_export_multiple_record_with_groups()`.
#'
#' The function requires REDCap API configuration variables to be set via
#' environment variables (`RC_API_URL`, `RC_API_TOKEN`). If these are missing,
#' an error is raised.
#'
#' @param record_id Character. The name of the record ID field in the REDCap
#'   project. Defaults to `"record_id"`.
#' @param start_date Character or `Date`. The start date used to filter records.
#'   If `NULL` (default), the origin date `"2022-06-01"` is used.
#' @param end_date Character or `Date`. The end date used to filter records.
#'   If `NULL`, defaults to today's date (`lubridate::today()`).
#'
#' @details
#' The function performs the following steps:
#'
#' \enumerate{
#'   \item Retrieves timestamp-related variables using `rc_timestamp_var()`.
#'   \item Performs a lightweight `record` export to obtain only record IDs and
#'         timestamp fields.
#'   \item Converts timestamp fields to `Date` format and harmonizes them using
#'         `ctr_dates_reconciliation()`.
#'   \item Filters records whose `date_survey` falls between `start_date` and
#'         `end_date`.
#'   \item Splits matching record IDs into blocks of 5,000 and extracts full
#'         records using `rc_export_multiple_record_with_groups()`.
#'   \item Combines all returned data into a single data frame.
#' }
#'
#' All date comparisons are performed in R using standard `Date` objects.
#'
#' @return A `data.frame` containing the fully extracted and merged REDCap
#'   records that fall within the specified date range.
#'
#' @examples
#' \dontrun{
#' # Export all records from 2023
#' rc_export_records_between_with_groups(
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31"
#' )
#'
#' # Using a custom record_id field
#' rc_export_records_between_with_groups(
#'   record_id = "study_id",
#'   start_date = "2022-01-01"
#' )
#' }
#'
#' @seealso
#'   \code{\link{rc_export_multiple_record_with_groups}},
#'   \code{\link{rc_timestamp_var}},
#'   \code{\link{ctr_dates_reconciliation}}
#'
#' @export

rc_export_records_between_with_groups <- function(record_id = 'record_id', start_date=NULL, end_date=NULL) {

  # Retrieve API configuration
  api_url <- Sys.getenv("RC_API_URL", unset = NA)
  api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)
  origin <- "2022-06-01" # setting origin

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

  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  # Dealing with date entries
  if(is.null(start_date))
    start_date <- origin

  if(is.null(end_date))
    end_date <- lubridate::today()

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if(end_date < start_date)
    stop("Error: The end date is before the start date.")

  filtered_data <- subset(data, data[["date_survey"]] >= start_date & data[["date_survey"]] <= end_date)

  # Step 3: Extract full records for those filtered

  record_ids <- sort(as.numeric(filtered_data$record_id))
  block_size <- 5000
  record_list <- split(record_ids, ceiling(seq_along(record_ids) / block_size))

  result_list <- lapply(record_list, function(recs){

    print(paste("Extracting records between", min(recs), "and", max(recs)))

    ## Extracting records with groups
    rc_export_multiple_record_with_groups(field_name = "record_id", field_value = recs)

  }

  )

  # Merge all
  data <- data.table::rbindlist(result_list, fill = T)

  data <- as.data.frame(data)

  return(data)
}
