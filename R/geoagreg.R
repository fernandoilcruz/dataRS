#' Geographic Aggregation
#'
#' @description Check ID and name of geographic aggregations. For municipalities, latitude and longitude are also available.
#'
#' @param ag Character. The aggregation category. There are  four valid options:
#' * "municipio" (the default): for municipalities.
#' * "regfunc": for regional functions, a state-specific planning regionalization.
#' * "corede": for coredes, a state-specific planning regionalization.
#' * "meso": for IBGE's mesoregions.
#' * "micro": for IBGE's microregions.
#'
#' @param sort Character. This is used for sorting names from A to Z ("ASC") or Z to A ("DESC").Default is "ASC".
#'
#' @return a data.frame.
#' @export
#'
#' @import jsonlite checkmate tidyverse
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#' geoagreg()
#' geoagreg(ag = "municipio")
#' geoagreg(ag = "corede", sort = "DESC")
#' }
#'
geoagreg <-
  function(ag = "municipio",
           sort = "ASC"){

    #check arguments
    checkmate::assert_character(ag)
    checkmate::assert_character(sort)

    #check available arguments
    ags <- c("municipio", "micro", "meso", "regfunc", "corede", "estado")
    if(!ag %in% ags){stop(paste0("Error: the ag argument is only available for "),
                          paste(ags, collapse = ", "))}

    sorts <- c("ASC","DESC")
    if(!sort %in% sorts){stop(paste0("Error: sort argument is only available for "),
                                 paste(sorts, collapse = ", "))}


    #url
    url <-
      paste0(
        get_url(info = "ag"),
        "&ag=",ag,
        "&sort=",sort,
        "&use_ibge_code=","1"
        )


    #output
    x <-
      url |>
      httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
      content("text", encoding = "UTF-8") |>
      jsonlite::fromJSON() |>
      tibble::as_tibble() |>
      dplyr::rename("geo_id" = "id",
                    "geo_name" = "nome")


    return(x)

  }
