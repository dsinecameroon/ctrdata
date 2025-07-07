#' Reattach Unique CTR Identifiers to De-identified Data
#'
#' This function re-identifies CTR records by joining them with the original mapping of
#' `id` to `record_id` and `unique_ctr_id`. It assumes these mappings are stored as RDS
#' files in a standardized "IDs" folder.
#'
#' @param box_path A character string specifying the root directory path (e.g., your Box folder).
#'   This should be the base path that contains the "DMAC/Data Anonymized/IDs" subdirectory.
#' @param data A data frame containing a column `id`, which includes the de-identified CTR IDs
#'   that need to be mapped back to their corresponding `record_id` and `unique_ctr_id`.
#'
#' @return A data frame with the original `data`, now augmented with `record_id` and
#'   `unique_ctr_id` columns. These are relocated to immediately follow the `id` column.
#'
#' @details
#' The function:
#' - Loads all ID mapping files from the specified directory
#' - Filters the ID mappings to only those present in the provided `data`
#' - Merges the filtered ID mappings back into `data` using a left join on `id`
#'
#' @examples
#' \dontrun{
#' box_path <- "/Users/you/Box/"
#' deid_data <- readRDS("path/to/deidentified_data.rds")
#' reid_data <- ctr_Data_reid(box_path, deid_data)
#' }
#'
#' @export
ctr_Data_reid <- function(box_path, data){
  ids_path = paste0(box_path, "DMAC/Data Anonymized/IDs")

  ids_files = list.files(ids_path, full.names = TRUE)

  ids_map <- lapply(ids_files, temp_loader)
  ids_map <- bind_rows(ids_map)

  filetered_ids <- ids_map %>% filter(id %in% data$id)

  data <- data %>%
    left_join(filetered_ids, by = "id") %>%
    relocate(record_id, unique_ctr_id, .after = "id")

  return(data)
}
