#' List, filter and sort variables' ID and name
#'
#' @param sort Optional. A character string. Either "ASC" to sort from A to Z or "DESC" to sort from Z to A.
#'
#' @return a tibble
#'
#' @import httr jsonlite tidyverse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Run this to see the full list of variables
#' vars()
#' vars(sort = "ASC")
#'}
#'
#'
#'
vars <-
  function(sort = "ASC") {

  # Validate 'sort' argument
  allowed_sorts <- c("ASC", "DESC", NULL)

  if (!is.null(sort) && !sort %in% allowed_sorts) {
    stop("Error: 'sort' must be either 'ASC', 'DESC', or NULL.")
  }

  # Construct URL
  base_url <- get_url(info = "var_list")

  if (!is.null(sort)) {
    url <- paste0(base_url, "&sort=", sort)
  } else {
    url <- base_url
  }

  # Fetch and parse data
  vars_list <- tryCatch(
    {
      res <-
        url |>
        httr::GET(timeout(1000000),
                  add_headers("Integra-Key" = get_api_key()),
                  config = config(ssl_verifypeer = 0)) |>
        content("text", encoding = "UTF-8") |>
        jsonlite::fromJSON() |>
        tibble::as_tibble() |>
        dplyr::rename(var_id = id, var_name = nome)

    },
    error = function(e) {
      stop("Failed to fetch or parse variable list from the API: ", e$message)
    }
  )

  return(vars_list)
}
