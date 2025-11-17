# ----------------------------- CTR ----------------------------------
#' Relabel values in CTR dataframe using a list of value-label pairs.
#'
#' @export
#' @param df A data.frame.
#' @param choices_list A named list of value-label pair data.frames.
#' @param lang Language column to use for relabeling. Default is both French and English.
#' @return Data frame with relabeled values.
label_data <- function(df, choices_list, lang = c("french", "english")) {

  lang <- match.arg(lang)

  message("Filtering complete")

  # ─────────────────────────────
  # 1. Apply choice labels
  # ─────────────────────────────
  for (nam in names(choices_list)) {
    if (nam %in% names(df)) {

      message("Labelling values of: ", nam)

      choices_df <- as.data.frame(choices_list[[nam]])
      choices_df <- choices_df[, grepl(paste0("value|", lang), names(choices_df)), drop = FALSE]

      input_values  <- paste0("\\b", choices_df[["value"]], "\\b")
      input_labels  <- paste0(choices_df[[lang]], ";")

      df[[nam]] <- stringi::stri_replace_all_regex(
        str       = tolower(df[[nam]]),
        pattern   = tolower(input_values),
        replacement = input_labels,
        vectorize_all = FALSE
      )

      df[[nam]] <- sub(";$", "", df[[nam]])   # remove trailing ;
    }
  }

  # ─────────────────────────────
  # 2. Detect safe binary variables
  # ─────────────────────────────
  bin_var <- names(df)[sapply(df, function(x) {
    ux <- unique(na.omit(x))          # remove NA before testing
    all(ux %in% c("0", "1", 0, 1))    # allow numeric or character
  })]

  # Exclude REDCap multi-choice vars
  bin_var <- setdiff(bin_var, names(choices_list))

  # ─────────────────────────────
  # 3. Build language map
  # ─────────────────────────────
  map <- if (lang == "english") {
    c("0" = "No",  "1" = "Yes")
  } else {
    c("0" = "Non", "1" = "Oui")
  }

  # ─────────────────────────────
  # 4. Apply replacement SAFELY
  # ─────────────────────────────
  if (length(bin_var) > 0) {
    # message("Binary fields converted: ", paste(bin_var, collapse = ", "))
    for (var in bin_var) {
      df[[var]] <- map[as.character(df[[var]])]   # preserves NA
    }
  }

  return(df)
}

