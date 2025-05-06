#' Unity of Measurement
#'
#' @description Get list of unity of measurement from system
#'
#' @return data.frame
#'
#' @import jsonlite tidyverse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' um()
#' }
#'
um <-
  function(){

    x <-
      get_url(info = "unid_medida") |>
      httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
      content("text", encoding = "UTF-8") |>
      jsonlite::fromJSON() |>
      tibble::as_tibble() |>
      dplyr::rename(
        um_id = id,
        um_name = nome,
        um_acronym = sigla
      )

    return(x)

  }
