#' Load CTR data between two dates.
#'
#'
#' @export
#' @param path Root path where data is stored.
#' @param start_date Start date in yyyy-mm-dd format.
#' @param end_date End date in yyyy-mm-dd format.
#' @return A data frame of records between specified dates.
ctr_data_loader <- function(path=".", start_date=NULL, end_date=NULL){
  if(is.null(start_date) & is.null(end_date)){
    folder_list <- list.files(paste0(path, "/"), pattern = "data_anon.xlsx", recursive = T,  full.names = T)
    df_list <- lapply(folder_list, temp_loader)
  } else {
    if((stringr::str_detect(start_date, "\\d{4}(-|:|/)\\d{2}(-|:|/)\\d{2}") & stringr::str_detect(end_date, "\\d{4}(-|:|/)\\d{2}(-|:|/)\\d{2}"))){
      start_year = lubridate::year(start_date)
      end_year =  lubridate::year(end_date)
      if(start_date>end_date) stop("Start Date Cannot be greater than end Date.")
      folder_list <- unlist(lapply(start_year:end_year, \(x) paste(x, c(paste0(0, 1:9), 10:12), sep="/")))
      start_mon = sprintf("%02d", lubridate::month(start_date))
      end_mon = sprintf("%02d", lubridate::month(end_date))
      index_start = which(folder_list == paste(start_year, start_mon, sep = "/"))
      index_end = which(folder_list == paste(end_year, end_mon, sep = "/"))
      folder_list <- folder_list[index_start:index_end]
      df_list <- lapply(folder_list, function(x){temp_loader(paste(path, x, "data_anon.xlsx", sep = "/"))})
    } else {
      print("Wrong Date Format. Date format should be yyyy-mm-dd or yyyy:mm:dd")
    }
  }
  df_final <- data.table::rbindlist(df_list, fill = T)
  df_final <- df_final %>% dplyr::filter(dplyr::between(as.Date(date_survey), as.Date(start_date), as.Date(end_date)))
  return(df_final)
}
