#' Download data from DEEDados
#'
#' @description This function allows the user to download data from DEEDados API using R.
#'
#' @param var_id The variable's ID.
#' @param ag The regional aggregation. There are  four valid options:
#' * "municipio" (the default): for municipalities.
#' * "regfunc": for functional regions, a state-specific planning regionalization only applied to Rio Grande do Sul.
#' * "corede": for coredes, a state-specific planning regionalization only applied to Rio Grande do Sul.
#' * "meso": for IBGE's mesoregions.
#' * "micro": for IBGE's microregions.
#' @param period The year to consult. It allows single string (ex:2010), vector(ex:c(2010,2022) or "all")
#' @param sort If the user wants to sort from "ASC" for ascendent order or "DESC" for descendent order. Default is "ASC".
#' @param add_labels Allows the user to add labels to the results
#'
#' @return a data.frame
#'
#' @import jsonlite dplyr tidyverse
#' @importFrom utils data
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Example 1
#' my_data <- getdata(var_id = 4603,ag = "corede",period = c(2016,2017), add_labels = TRUE)
#' print(my_data)
#'
#' #Example 2
#' my_data <- getdata(var_id = 4603,geo_id = 2, ag = "corede",period = "all")
#' print(my_data)
#'}
#'
getdata <-
  function(var_id,
           ag,
           period = "all",
           geo_id = NULL,
           sort = "ASC",
           add_labels = FALSE){


    #check available arguments
    if(missing(var_id)){stop("Error: Select a valid argument for var_id. You can use the var() and vardetails() functions to see the available options")}

    ags <- c("municipio", "micro", "meso", "corede", "regfunc" ,"estado")
    if((missing(ag)) || (!ag %in% ags)){stop(paste0("Error: Select a valid argument for ag. The ag argument is only available for "),
                          paste(ags, collapse = ", "))}

    #if(missing(period)){stop("Error: Select a valid argument for period")}

    #treat var_id
    if(any(var_id == "all")){
      var_id <- vars() |> dplyr::select(var_id) |> dplyr::pull()
    }

    #ARGUMENTO FORÇADO####################################ALOOOOOOOOW
    # var_id <- c(3755, 4784)
    # ag = "municipio"
    # period = "all"
    # sort = "ASC"

    #output
    if(is.null(geo_id)){
      x<-
        var_id |>
        purrr::map_df(function(z){
          paste0(get_url(info = "data"),
                 "&id=",
                 z,
                 "&ag=",
                 ag,
                 "&periodo=",
                 period |> paste0(collapse = ","),
                 "&sort=",
                 sort,
                 "&formato=json"
          ) |>
            httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
            content("text", encoding = "UTF-8") |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = data) |>
            dplyr::mutate(var_id = z)
        })
    }else{
      x<-
        var_id |>
        purrr::map_df(function(z){
          paste0(get_url(info = "data"),
                 "&id=",
                 z,
                 "&ag=",
                 ag,
                 "&periodo=",
                 period |> paste0(collapse = ","),
                 "&filtro=",
                 geo_id,
                 "&sort=",
                 sort,
                 "&formato=json"
          ) |>
            httr::GET(timeout(1000000), config = config(ssl_verifypeer = 0)) |>
            content("text", encoding = "UTF-8") |>
            jsonlite::fromJSON() |>
            tidyr::unnest(cols = data) |>
            dplyr::mutate(var_id = z)
        })
    }

    x <-
      x |>
      dplyr::rename("geo_id" = "id",
                    "year" = "ano",
                    "value" = "valor",
                    "unit" = "un_medida",
                    "note" = "nota") |>
      dplyr::mutate(unit = as.integer(unit)) |>
      dplyr::select(var_id,geo_id,year,value, unit, note)


    #add labels
    vars1 <- vars()
    geos1 <- geoagreg(ag = ag)
    units1 <- um()

    if(add_labels == TRUE){
      x <-
        x |>
        dplyr::left_join(vars1,
                         by = c("var_id" = "var_id")) |>
        dplyr::left_join(geos1,
                         by = c("geo_id" = "geo_id")) |>
        dplyr::left_join(units1,
                         by = c("unit" = "um_id")) |>

        dplyr::select(var_id, var_name, geo_id, geo_name, year, value, um_name, note) |>
        dplyr::rename(unit = um_name)
    }

    return(x)

  }


#teste
# inicio <- Sys.time()
# getdata(var_id = c(3755,4784), ag = "municipio")
# fim <- Sys.time()
# tempo <- fim-inicio

# inicio <- Sys.time()
# teste <- vars() |> dplyr::select(var_id) |> dplyr::pull()
# getdata(var_id = "all", ag = "meso")
# fim <- Sys.time()
# tempo <- fim-inicio
# library(beepr)
# beep(sound = 3)

