#' Set API key as environment variable
#'
#' @description This function allows users to set the API key.
#'
#' @param key The API key
#'
#' @returns No return value. Called for side effects only (sets an environment variable).
#' @export
#'
#' @examples
#'\dontrun{
#'set_api_key("your_api_key_here")
#'}
#'
#'
set_api_key <- function(key) {
  if (!is.character(key) || length(key) != 1) {
    stop("API key must be a single string.")
  }
  Sys.setenv(MY_API_KEY = key)
  invisible(TRUE)

  check_valid_api_key()
}


