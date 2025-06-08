#' Reconcile multiple timestamp fields to create a unified date_survey column.
#'
#' @export
#' @param data Data frame with timestamp fields.
#' @param date_field Main date field for reference.
#' @return Data frame with unified date_survey column.
ctr_dates_reconciliation <- function(data, date_field){
  pattern = "(?=.*timestamp$)"
  main_date_var <- names(data)[grepl(pattern, names(data), perl = TRUE)]
  pattern2 = paste0("(?=.*timestamp)","(?!.*timestamp$)")
  other_date_var <- names(data)[grepl(pattern2, names(data), perl = TRUE)]

  data <- data %>% mutate(across(matches("timestamp"),  ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))

  for(i in seq_along(main_date_var)){
    data <- data %>%
      mutate(!!rlang::sym(main_date_var[i]) := if_else(!is.na(!!rlang::sym(main_date_var[i])),
                                                       !!rlang::sym(main_date_var[i]),
                                                       !!rlang::sym(other_date_var[i])))
  }

  data[['date_survey']] <- apply(data[, c(main_date_var, other_date_var)], 1, \(x) max(x, na.rm=TRUE))
  data <- data[, c("record_id", "date_survey", setdiff(names(data), c("record_id", "date_survey")))]

  return(data)
}
