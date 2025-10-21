#' All Geographic Aggregation
#'
#' @description Check ID and name of all geographic aggregations in selected periods. Default is all periods.
#' @param period The year to consult. It allows single string (ex:2010), vector(ex:c(2010,2022) or NULL)
#'
#' @return a tibble.
#' @export
#'
#' @import jsonlite checkmate tidyverse
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#' geoagregall2()
#' geoagregall2(period = 2010)
#' }
#'
geoagregall2 <-
  function(period = NULL){

    #check arguments
    checkmate::assert_numeric(period, null.ok = T)

    url <- ifelse(
      is.null(period),

        get_url(info = "composicao_ag_all"),

        paste0(get_url(info = "composicao_ag_all"),
               "&periodo=",
               paste0(period,collapse = ","),
               "&use_ibge_code=","1"
        )
    )


    #output
    x <-
      url |>
      httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
      content("text", encoding = "UTF-8") |>
      jsonlite::fromJSON() |>
      tibble::as_tibble() |>
      dplyr::rename("geo_id_municipio" = "id_municipio",
                    "geo_id_corede" = "id_corede",
                    "geo_id_regfunc" = "id_regfunc",
                    "geo_id_micro" = "id_microrregiao",
                    "geo_id_meso" = "id_mesorregiao",
                    "geo_id_estado" = "id_estado",

                    "geo_name_municipio" = "municipio",
                    "geo_name_corede" = "corede",
                    "geo_name_regfunc" = "regfunc",
                    "geo_name_micro" = "microrregiao",
                    "geo_name_meso" = "mesorregiao",
                    "geo_name_estado" = "estado",

                    "period" = "ano")

    return(x)

  }
