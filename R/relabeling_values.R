# ----------------------------- CTR ----------------------------------
#' Relabel values in CTR dataframe using a list of value-label pairs.
#'
#' @export
#' @param df A data.frame.
#' @param choices_list A named list of value-label pair data.frames.
#' @param lang Language column to use for relabeling. Default is both French and English.
#' @return Data frame with relabeled values.
relabeling_values_ctr_df <- function(df, choices_list, lang=c("french", "english")){
  print("filtering complete")

  for(nam in names(choices_list)){
    if(nam %in% names(df)){
      print(paste("starting on", nam))

      choices_df <- choices_list[[nam]] %>% as_tibble()
      choices_df <- choices_df %>% select(matches(paste0("value|", lang)))

      input_values <- paste0("\\b", choices_df[["value"]], "\\b")
      input_labels <- paste0(choices_df[[lang]], ";")

      print(paste("Labelling values of :", nam))

      df[,nam] <- stringi::stri_replace_all_regex(str= tolower(df[[nam]]), pattern =tolower(input_values), replacement = input_labels, vectorize_all = FALSE)
    }
  }
  return(df)
}
