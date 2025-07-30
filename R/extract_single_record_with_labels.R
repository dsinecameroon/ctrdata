#' Export and Label a Single REDCap Record
#'
#' Retrieves a single REDCap record matching a specified field/value pair and
#' applies value labels to radio and checkbox variables. Labels can be built
#' using live REDCap metadata via the API ("online" mode) or an offline data
#' dictionary file ("offline" mode).
#'
#' @param field_name Character string. The field name to search for
#'   (e.g., `'unique_ctr_id'`).
#' @param field_value Character string or pattern to match within `field_name`.
#' @param meta Character, either `"online"` or `"offline"`.
#'   * `"online"`: Fetches metadata directly from REDCap and applies labels immediately.
#'   * `"offline"`: Loads metadata from a local data dictionary file.
#' @param path Character. Path to the offline data dictionary file (required if `meta = "offline"`).
#' @param lang Character. Language code used when applying labels from a multilingual offline dictionary (required if `meta = "offline"`).
#' @param sheet Integer. Sheet index to load from the offline dictionary file (default = `3`).
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A data frame containing the full REDCap record for the matching record ID,
#'   with value labels applied to all radio and checkbox variables.
#'
#' @details
#' **Online mode**:
#' * Fetches metadata from REDCap using `rc_export_metadata()`.
#' * Detects radio and checkbox fields and parses their choice codes and labels.
#' * For radio fields, replaces raw codes with the corresponding labels.
#' * For checkbox fields:
#'   - Combines dummy variables (`field___code`) into a single grouped variable (`field`)
#'     containing selected codes.
#'   - Replaces codes with the corresponding labels.
#'   - Relocates the grouped variable to appear immediately before the dummy columns.
#'
#' **Offline mode**:
#' * Loads a local data dictionary via `temp_loader()`.
#' * Builds a `choices_list` and applies labels using `ctrdata::label_data()`.
#'
#' Environment variables `RC_API_URL` and `RC_API_TOKEN` must be set to use online mode.
#' Use `rc_config(api_url, api_token)` to configure API access.
#'
#' @examples
#' \dontrun{
#' # Online labeling
#' rc_config("https://redcap.your.org/api/", "XYZ123TOKEN")
#' df <- rc_export_single_record_lab(field_name = "unique_ctr_id",
#'                                   field_value = "HRL001174",
#'                                   meta = "online")
#'
#' # Offline labeling
#' df <- rc_export_single_record_lab(field_name = "unique_ctr_id",
#'                                   field_value = "HRL001174",
#'                                   meta = "offline",
#'                                   path = "dictionary.xlsx",
#'                                   lang = "en",
#'                                   sheet = 3)
#' }
#'
#' @export

rc_export_single_record_lab <- function(field_name, field_value, meta = c("online", "offline"),
                                        path = NULL, lang = NULL, sheet = 3, ...) {

  meta <- match.arg(meta)

  ## --- Online metadata and labeling ---
  if (meta == "online") {
    # Fetch metadata from REDCap
    metadata <- rc_export_metadata()

    # Keep only radio and checkbox fields and exclude login/ID/timestamp fields
    metadata <- metadata %>%
      dplyr::filter(field_type %in% c("radio", "checkbox"),
                    !stringr::str_detect(field_name, "login|id|timestamp"))

    # Build a named list of choices for radios and checkboxes
    parse_choices <- function(choice_string) {
      if (is.na(choice_string) || choice_string == "") return(NULL)
      opts <- unlist(strsplit(choice_string, "\\|"))
      opts <- trimws(opts)
      choices <- sapply(opts, function(opt) {
        parts <- sub("^([^,]+),\\s*(.*)$", "\\1;;\\2", opt) # split only on first comma
        kv <- strsplit(parts, ";;")[[1]]
        setNames(kv[2], kv[1])
      })
      return(choices)
    }

    choices_list <- lapply(metadata$select_choices_or_calculations, parse_choices)
    names(choices_list) <- metadata$field_name

    # Export the single record
    df <- ctrdata::rc_export_single_record(field_name = field_name,
                                           field_value = field_value)

    # Apply labels for radio and checkbox fields
    for (fld in names(choices_list)) {
      if (is.null(choices_list[[fld]])) next

      if (fld %in% names(df)) {
        # Radio/select-one: replace raw codes with labels
        df[[fld]] <- as.character(choices_list[[fld]][as.character(df[[fld]])])
      }

      # Checkbox/select-multiple: combine dummy columns
      chk_cols <- grep(paste0("^", fld, "___"), names(df), value = TRUE)
      if (length(chk_cols) > 0) {
        # Create combined field with codes
        df[[fld]] <- apply(df[chk_cols], 1, function(row) {
          selected <- sub(paste0("^", fld, "___"), "", chk_cols[row == 1])
          paste(selected, collapse = ",")
        })
        # Replace with labels if any selected
        df[[fld]] <- sapply(strsplit(df[[fld]], ","), function(vals) {
          vals <- trimws(vals)
          lbls <- choices_list[[fld]][vals]
          paste(na.omit(lbls), collapse = ", ")
        })
        # Reorder: move grouped field before its dummies
        col_order <- names(df)
        idx <- which(col_order %in% chk_cols)
        first_idx <- min(idx)
        col_order <- setdiff(col_order, fld)
        new_order <- append(col_order, fld, after = first_idx - 1)
        df <- df[, new_order]
      }
    }

    return(df)
  }

  ## --- Offline metadata and labeling ---
  if (meta == "offline") {
    if (is.null(path) | is.null(lang))
      stop("Please set the data dictionary path and language when using offline mode")

    metadata <- temp_loader(path = path, sheet = sheet)
    choices_list <- split(metadata, metadata$variable)

    df <- ctrdata::rc_export_single_record(field_name = field_name,
                                           field_value = field_value)

    df <- ctrdata::label_data(df = df, choices_list = choices_list, lang = lang)
    return(df)
  }
}
