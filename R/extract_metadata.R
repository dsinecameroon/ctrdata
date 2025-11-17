#' Retrieve and Parse REDCap Metadata and Choice Labels
#'
#' Extracts variable-choice metadata either directly from a live REDCap project
#' (`mode = "online"`) or from a local data dictionary (`mode = "offline"`).
#' The function parses choice strings into a structured format including
#' \code{value}, \code{english}, and \code{french} label columns and returns
#' them as a named list, suitable for downstream labeling functions.
#'
#' @param mode Character, either `"online"` (default) or `"offline"`.
#'   * `"online"` retrieves metadata via the REDCap API using
#'     `RC_API_URL` and `RC_API_TOKEN` stored in environment variables
#'     (set via \code{rc_config()}).
#'   * `"offline"` loads metadata from an external Excel/CSV file.
#'
#' @param exclude A character pattern used to remove variables from the final
#'   list. Default is `"ctr|wave"`. Matching is case-insensitive and applies
#'   to variable names.
#'
#' @param path (Offline mode only) The file path to a metadata worksheet.
#'   Required when `mode = "offline"`.
#'
#' @param sheet (Offline mode only) The sheet name containing the metadata.
#'   Required when `mode = "offline"`.
#'
#' @param ... Additional arguments passed through when `mode = "offline"`.
#'   Must include `path` and `sheet`.
#'
#' @return
#' A named list where each element corresponds to one REDCap variable and
#' contains a tibble with three standardized fields:
#' \itemize{
#'   \item \code{value}   — Raw stored REDCap value
#'   \item \code{english} — English label
#'   \item \code{french}  — French label (if present)
#' }
#'
#' Variables containing no choices or excluded by `exclude`
#' are automatically removed.
#'
#' @details
#' When \code{mode = "online"}:
#' \enumerate{
#'   \item Metadata is downloaded via `content = "metadata"` REDCap API request.
#'   \item `select_choices_or_calculations` strings are parsed into `value`
#'     and multilingual labels.
#'   \item The function **splits only on the first comma**, ensuring labels
#'     containing commas remain intact.
#'   \item Variables matching `ctr|login|wave` are removed by default.
#' }
#'
#' When \code{mode = "offline"}:
#' \enumerate{
#'   \item Metadata is loaded using \code{ctrdata::temp_loader()}.
#'   \item Choice lists are split per variable using `split()`.
#' }
#'
#' @section Required Configuration:
#' To use `"online"` mode, the following environment variables
#' must be set beforehand:
#'
#' \preformatted{
#' Sys.setenv(RC_API_URL   = "https://redcap.server.org/api/")
#' Sys.setenv(RC_API_TOKEN = "YOURTOKEN123")
#' }
#'
#' @examples
#' \dontrun{
#' ## --- ONLINE MODE ---
#' rc_config("https://redcap.server/api/", "TOKEN123")
#' meta <- get_metadata()
#'
#' ## --- OFFLINE MODE ---
#' meta <- get_metadata(
#'   mode  = "offline",
#'   path  = "data_dictionary.xlsx",
#'   sheet = "metadata"
#' )
#' }
#'
#' @export
get_metadata <- function(mode="online",exclude="ctr|wave",path=NULL, sheet=NULL, ...){

  ## --- Online metadata and labeling ---
  if (mode == "online") {
    # Fetch metadata from REDCap
    # Retrieve API configuration
    api_url <- Sys.getenv("RC_API_URL", unset = NA)
    api_token <- Sys.getenv("RC_API_TOKEN", unset = NA)

    # Metadata extraction
    metadata <- httr::POST(api_url,
                           body = list(
                             token = api_token,
                             content = 'metadata',
                             format = 'json'
                           ),
                           encode = "form"
    ) %>%
      httr::content("parsed")

    # Keep only radio and checkbox fields and exclude login/ID/timestamp fields
    # metadata <- metadata %>%
    #   dplyr::filter(field_type %in% c("radio", "checkbox"),
    #                 !stringr::str_detect(field_name, "login|id|timestamp"))

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
    #
    choices_list <- lapply(metadata, \(x) parse_choices(x$select_choices_or_calculations))
    choices_name <- unlist(lapply(metadata, \(x) x$field_name))
    names(choices_list) <- choices_name

    choices_list <- Filter(Negate(is.null), choices_list)

    choices_list <- lapply(choices_list, \(x)
                           tibble(value= stringr::str_squish(sub(",.*$", "", names(x))),
                                  english=ifelse(stringr::str_detect(x, "/"), stringr::str_squish(sub("/.*$", "", as.character(x))), stringr::str_squish(sub(",.*$", "", as.character(x)))),
                                  french=if_else(stringr::str_detect(x, "/"), stringr::str_squish(sub("^.*/", "", as.character(x))), stringr::str_squish(sub("^.*,", "", as.character(x)))))
    )

    choices_list <- choices_list[names(choices_list)[!grepl("ctr|login|wave", names(choices_list))]]


  }else {
    args <- list(...)

    if (is.null(args$path) | is.null(args$sheet))
      stop("Please set the data dictionary path and language when using offline mode")

    metadata <- ctrdata::temp_loader(path = args$path, sheet = args$sheet)
    choices_list <- split(metadata, metadata$variable)

  }

  return(choices_list)
}
