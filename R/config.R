########### Functions to interact with the RedCap API ###############

#' Configure REDCap API Environment
#'
#' Sets the REDCap API URL and token as environment variables to be used in API calls.
#'
#' @param api_url A character string containing the base URL of the REDCap API (e.g., `"https://redcap.yourdomain.edu/api/"`).
#' @param api_token A character string containing the user's API token for a specific REDCap project.
#'
#' @return None. This function is called for its side effects (setting environment variables).
#'
#' @examples
#' rc_config("https://redcap.yourdomain.edu/api/", "ABC123XYZTOKEN")
#' Sys.getenv("RC_API_URL")  # Should return the set API URL
#'
#' @export
rc_config <- function(api_url, api_token){
  ## setting the environment
  Sys.setenv("RC_API_URL" = api_url)
  Sys.setenv("RC_API_TOKEN" = api_token)
}
