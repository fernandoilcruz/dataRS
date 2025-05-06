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
#' \dontrun{
#' source()
#' }
#'
source <-
  function(){

    x <-
      paste0(
      get_url(info = "fonte")
      ) |>
      httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
      content("text", encoding = "UTF-8") |>
      jsonlite::fromJSON() |>
      tibble::as_tibble() |>
      dplyr::rename(var_id_source = id,
                    name_id_source = nome) |>
      dplyr::mutate(
        name_id_source = stringr::str_trim(name_id_source, side = "both")
      )

    return(x)
  }
