#' Test API key
#'
#'Internal helper that tests if the API key set with set_api_key() is valid.
#'
#' @return A character string
#'
#' @noRd
#'
check_valid_api_key <- function() {
  api_key <- get_api_key()

  if (!nzchar(api_key)) {
    message("API key is missing.")
    return("No key provided.")
  }

  test_url <- paste0("https://data.rs.gov.br/api/var_list.php?key=", api_key)

  result <- tryCatch(
    {
      res <-
        test_url |>
        jsonlite::fromJSON()

      if ("erro" %in% names(res)) {
        stop("Invalid key. Please consult the DataRS API documentation.")
      } else {
        "Valid key."
      }
    },
    error = function(e) {

      message("API validation failed: ", e$message)
      return("Key check failed.")
    }
  )

  return(result)
}
