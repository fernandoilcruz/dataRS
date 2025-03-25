#' Set API key as environment variable
#'
#' @param key
#'
#' @returns No return value. Called for side effects only (sets an environment variable).
#' @export
#'
#' @examples
#'\dontrun{
#set_api_key("e93368a014d8169d21cf13712290a0cb")
#'}
#'
#'
set_api_key <- function(key) {
  Sys.setenv(MY_API_KEY = key)
}


