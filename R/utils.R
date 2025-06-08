# -------------------------- Utility Functions ---------------------------

#' Load Excel file through a temporary path to handle permissions or long paths.
#'
#' @param path Path to Excel file.
#' @param ... Additional arguments to pass to read_excel.
#' @return A data.frame.
temp_loader <- function(path, ...){
  temp_path <- tempfile(fileext = ".xlsx")
  file.copy(path, temp_path, overwrite = TRUE)
  print(paste("Reading", path))
  data <- readxl::read_excel(temp_path, ...)
  unlink(temp_path)
  return(data)
}

#' Upload a data.frame to a specified path using a temporary file.
#'
#' @param df The data frame to save.
#' @param path Destination file path.
temp_uploader <- function(df, path){
  temp_path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(df, path = temp_path)
  file.copy(temp_path, path, overwrite = TRUE)
  unlink(temp_path)
}

#' Create directory path recursively if it doesn't exist.
#'
#' @param full_path Full path to create.
create_path <- function(full_path){
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
  }
}

#' Split data by a month column and save anonymized data.
#'
#' @param df Data frame to split and save.
#' @param month_var Column name representing year-month.
#' @param path Base path to save data.
split_save <- function(df, month_var='year_mon', path=box_data_path){
  mon = unique(df[[month_var]])
  if(length(mon)!=1) stop("Month must be unique")
  path_month <- paste0(path, mon, "/")
  create_path(path_month)
  df %>% select(!matches(month_var)) %>% temp_uploader(., path = paste0(path_month, "data_anon.xlsx"))
  print(paste("Anonymized data saved for ", mon))
}
