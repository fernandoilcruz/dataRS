#' Build the API URL
#'
#' Internal helper to build full URL for API call.
#'
#' @param info API endpoint name.
#' @return A string with the full API URL.
#' @noRd
#'
get_api_key <- function() {
  # @noRd tells roxygen2 not to generate documentation files in man/ — keeping it out of help search (?get_url won’t work).
  key <- Sys.getenv("MY_API_KEY", unset = "")
  if (key == "") {
    stop("API key not found. Please set it using set_api_key().", call. = FALSE)
  }
  return(key)
}
