#' All Geographic Aggregation
#'
#' @description Check ID and name of all geographic aggregations in selected periods. Default is all periods.
#' @param period The year to consult. It allows single string (ex:2010), vector(ex:c(2010,2022) or NULL)
#' @param key The key to access the DataVis API. Check www.datavis.dee.rs.gov.br to learn more.

#'
#' @return a tibble.
#' @export
#'
#' @import jsonlite checkmate tidyverse
#' @importFrom utils data
#'
#' @examples
#' geoagregall2()
#' geoagregall2(period = 2010)
#'
geoagregall2 <-
  function(period = NULL, key){

    #check arguments
    checkmate::assert_numeric(period, null.ok = T)

    url <- ifelse(
      is.null(period),

        get_url(info = "composicao_ag_all"),

        paste0(get_url(info = "composicao_ag_all"),
               "&periodo=",
               paste0(period,collapse = ",")
        )
    )



    # )
    #   paste0("https://datavis.dee.rs.gov.br/api/composicao_ag_all.php?",
    #          "key=",
    #          key,
    #          "&",
    #          "periodo=",
    #          paste0(period,collapse = ",")
    #
    #   )

    #output
    x <-
      url |>
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


# start<-Sys.time()
# geoagregall2(period = 1994:2024)
# end<-Sys.time()
# end-start





# geoagregall3 <-
#   function(period = NULL){
#
#     url <-
#       paste0("http://10.112.42.22/api/composicao_ag_all.php?",
#              "periodo=",paste0(period,collapse = ",")
#       )
#
#     #output
#     x <-
#       url |>
#       jsonlite::fromJSON() |>
#       tibble::as_tibble() |>
#       dplyr::rename("geo_id_municipio" = "id_municipio",
#                     "geo_id_corede" = "id_corede",
#                     "geo_id_micro" = "id_microrregiao",
#                     "geo_id_meso" = "id_mesorregiao",
#
#                     "geo_name_municipio" = "municipio",
#                     "geo_name_corede" = "corede",
#                     "geo_name_micro" = "microrregiao",
#                     "geo_name_meso" = "mesorregiao",
#                     "period" = "ano")
#
#     return(x)
#
#   }
#
#
# start<-Sys.time()
# geoagregall3()
# end<-Sys.time()
# end-start
