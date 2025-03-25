#' Source
#'
#' @description Get sources list from system
#' @return data.frame
#'
#' @import jsonlite tidyverse
#'
#' @export
#'
#' @examples
#' source()
#'
source <-
  function(key){

    x <- paste0(
      get_url(info = "fonte")
    )

    jsonlite::fromJSON(x) |>
      tibble::as_tibble() |>
      dplyr::rename(var_id_source = id,
                    name_id_source = nome)
  }
