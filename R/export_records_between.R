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
rc_export_records_between_with_groups <- function(record_id = 'record_id', start_date=NULL, end_date=NULL) {
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

    ## Extracting records with groups
    rc_export_multiple_record_with_groups(field_name = "record_id", field_value = recs)

    # print(paste("Records", min(recs),  "to", max(recs), "extracted."))

  }

  )

  # Merge all
  data <- data.table::rbindlist(result_list, fill = T)

  data <- as.data.frame(data)

  return(data)
}



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
#' The second API call uses:
#' - `rawOrLabel = "both"`: Returns both raw coded values and labels combined (e.g., `"1, Male"`).
#'   * Possible values: `"raw"`, `"label"`, `"both"`.
#' - `rawOrLabelHeaders = "both"`: Returns both raw variable names and field labels in headers.
#'   * Possible values: `"raw"`, `"label"`, `"both"`.
#' - `exportCheckboxLabel = "true"`: Exports checkbox fields as labels instead of 0/1 codes.
#'   * Possible values: `"true"`, `"false"`.
#'
#' Environment variables `RC_API_URL` and `RC_API_TOKEN` must be set before calling this function.
#' Timestamps must be parsable as `Date` objects.
#'
#' @examples
#' \dontrun{
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' rc_export_records_between_label(start_date = "2024-01-01", end_date = "2024-03-31")
#' }
#'
#' @export
rc_export_records_between_label <- function(record_id = 'record_id', start_date, end_date) {
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

  record_ids <- sort(as.numeric(filtered_data$record_id))
  block_size <- 5000
  record_list <- split(record_ids, ceiling(seq_along(record_ids) / block_size))


  # Step 3: Extract full records for those filtered
  result_list <- lapply(record_list, function(recs){

    httr::POST(api_url,
               body = list(
               token = api_token,
               content = 'record',
               format = 'json',
               type = 'flat',
               records = paste(recs, collapse = ","),
               exportSurveyFields = 'true',
               rawOrLabel = "label",          # <-- this returns both raw and labels
               # rawOrLabelHeaders = "raw",   # <-- includes both raw and label headers
               exportCheckboxLabel = "false"  # <-- ensures checkbox/dummies have labels
               ),
               encode = "form")
    })


  data_list <- lapply(result_list,\(result)
                      jsonlite::fromJSON(httr::content(result, as = "text", encoding = "UTF-8")))
  # Merge all
  data <- data.table::rbindlist(data_list, fill = T)

  data <- suppressWarnings(ctr_dates_reconciliation(data = data))

  data <- subset(data, select = names(data)[!grepl("prectr|ctr_login|redcap",names(data))])

  return(data)
}
