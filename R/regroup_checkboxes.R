#' Regroup Checkbox Dummies in a Full REDCap Dataset
#'
#' Scans a REDCap dataset for checkbox dummy variables (columns containing `___`)
#' and creates a grouped column for each checkbox field. The grouped column contains
#' the selected raw codes joined by commas and is relocated to appear immediately
#' before the related dummy columns.
#'
#' @param data A data frame exported from REDCap (flat format).
#'
#' @return The same data frame with additional grouped columns for each checkbox
#'   field, placed before their corresponding dummy columns.
#'
#' @details
#' - Checkbox dummies are detected by the `___` separator in column names.
#' - For each unique base field, the function combines selected codes into a single
#'   comma-separated string (e.g., `"0,2,5"`).
#' - The grouped column is inserted immediately before the dummy variables in the
#'   output dataset.
#'
#' @examples
#' \dontrun{
#' full_data <- rc_export_full_dataset()
#' regrouped <- rc_regroup_checkboxes(full_data)
#' }
#'
#' @export
rc_regroup_checkboxes <- function(data) {
  # Identify all checkbox dummy columns
  base_fields <- unique(sub("___.*$", "", grep("___", names(data), value = TRUE)))

  for (field in base_fields) {
    chk_cols <- grep(paste0("^", field, "___"), names(data), value = TRUE)
    if (length(chk_cols) > 0) {
      # Create grouped column with selected codes
      data[[field]] <- apply(data[chk_cols], 1, function(row) {
        selected <- sub(paste0("^", field, "___"), "", chk_cols[row == 1])
        paste(selected, collapse = ",")
      })

      # Relocate grouped column immediately BEFORE the first dummy
      col_order <- names(data)
      idx <- which(col_order %in% chk_cols)
      first_idx <- min(idx)

      # Remove grouped column if it already existed
      col_order <- setdiff(col_order, field)

      # Insert grouped column at correct position
      new_order <- append(col_order, values = field, after = first_idx - 1)
      data <- data[, new_order]
    }
  }

  return(data)
}
