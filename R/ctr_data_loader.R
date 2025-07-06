#### Extracting data between 2 dates
#' Load CTR Data from Google Box Folder Between Two Dates
#'
#' Loads CTR data stored in a Google Box folder hierarchy structured as `DMAC/Data Anonymized/YYYY/MM/`.
#' If no dates are provided, it loads all available data files. If a date range is provided, it filters
#' based on folder paths and further restricts rows based on the `date_survey` column.
#'
#' @param box_root A string representing the root directory where the Google Box folder is mounted or synced locally.
#' For example, `"~/Box"` or `"C:/Users/username/Box"`. The function will look inside
#' `box_root/DMAC/Data Anonymized/` for year/month folders containing `data_anon.xlsx` files.
#' @param start_date Optional start date (`"yyyy-mm-dd"`). Only records on or after this date will be included.
#' @param end_date Optional end date (`"yyyy-mm-dd"`). Only records on or before this date will be included.
#'
#' @return A data frame combining and filtering records across all relevant months within the given date range.
#' If no date range is provided, all records found in subfolders are returned.
#'
#' @details
#' The function assumes the following folder structure within the Box folder:
#' `box_root/DMAC/Data Anonymized/YYYY/MM/data_anon.xlsx`
#'
#' - If `start_date` and `end_date` are not provided, the function loads all files matching `"data_anon.xlsx"` recursively.
#' - If dates are provided, only files in matching year/month folders will be considered.
#' - After loading, rows are filtered by the `date_survey` column.
#'
#' Requires the helper function `temp_loader()` which must read a file path and return a data frame.
#'
#' @examples
#' \dontrun{
#' ctr_data_loader(box_root = "~/Box", start_date = "2023-01-01", end_date = "2023-03-31")
#' ctr_data_loader(box_root = "C:/Users/myuser/Box", start_date = "2024-01-01", end_date = "2024-06-30")
#' ctr_data_loader(box_root = "~/Box")  # Load all data
#' }
#'
#' @export
ctr_data_loader <- function(box_root = ".", start_date = NULL, end_date = NULL) {

  path = paste0(box_root, "/DMAC/Data Anonymized/")

  if (is.null(start_date) & is.null(end_date)) {
    folder_list <- list.files(
      path = path,
      pattern = "data_anon.xlsx",
      recursive = TRUE,
      full.names = TRUE
    )

    df_list <- lapply(folder_list, temp_loader)

  } else {

    if (
      stringr::str_detect(start_date, "\\d{4}(-|:|/)\\d{2}(-|:|/)\\d{2}") &
      stringr::str_detect(end_date, "\\d{4}(-|:|/)\\d{2}(-|:|/)\\d{2}")
    ) {

      # Convert dates
      start_date <- as.Date(start_date)
      end_date <- as.Date(end_date)

      if (start_date > end_date)
        stop("Start date cannot be greater than end date.")

      # Build year/month folder list
      start_year <- lubridate::year(start_date)
      end_year <- lubridate::year(end_date)

      all_months <- lapply(start_year:end_year, function(y) {
        months <- sprintf("%02d", 1:12)
        paste0(y, "/", months)
      })
      folder_list <- unlist(all_months)

      # Determine folder range
      start_mon <- sprintf("%02d", lubridate::month(start_date))
      end_mon <- sprintf("%02d", lubridate::month(end_date))

      path_start <- paste0(start_year, "/", start_mon)
      path_end <- paste0(end_year, "/", end_mon)

      index_start <- which(folder_list == path_start)
      index_end <- which(folder_list == path_end)

      folder_list <- folder_list[index_start:index_end]

      # Load from folders
      df_list <- lapply(folder_list, function(x) {
        temp_loader(file.path(path, x, "data_anon.xlsx"))
      })

    } else {
      stop("Wrong date format. Use 'yyyy-mm-dd'.")
    }
  }

  # Combine and filter final dataset
  df_final <- data.table::rbindlist(df_list, fill = TRUE)

  if (!is.null(start_date) && !is.null(end_date)) {
    df_final <- dplyr::filter(df_final, dplyr::between(as.Date(date_survey), start_date, end_date))
  }

  return(df_final)
}
