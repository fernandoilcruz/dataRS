#' Download data from DataRS
#'
#' @description This function allows the user to download data from DataRS API using R.
#'
#' @param var_id The variable's ID.
#' @param ag The regional aggregation. There are  five valid options:
#' * "municipio" (the default): for municipalities.
#' * "regfunc": for functional regions, a state-specific planning regionalization only applied to Rio Grande do Sul.
#' * "corede": for coredes, a state-specific planning regionalization only applied to Rio Grande do Sul.
#' * "meso": for IBGE's mesoregions.
#' * "micro": for IBGE's microregions.
#' * "estado": for the state of Rio Grande do Sul
#' @param geo_id Optional parameter. Filters a specific municipality, regional function, corede, mesorregion, microrregion or state ID. Default is NULL.
#' @param period The year to consult. It allows single string (ex:2010), vector(ex:c(2010,2022) or "all")
#' @param sort If the user wants to sort from "ASC" for ascendent order or "DESC" for descendent order. Default is "ASC".
#' @param add_labels Allows the user to add labels to the results
#' @param var_name_break Breaks the var_name path into columns when add_labels = TRUE. Default is TRUE.
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
#' my_data<-getdata(var_id=4603,ag ="corede",period=c(2016,2017),add_labels=TRUE,var_name_break=TRUE)
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
           add_labels = FALSE,
           var_name_break = FALSE){


    #check available arguments
    if(missing(var_id)){stop("Error: Select a valid argument for var_id. You can use the var() and vardetails() functions to see the available options")}

    ags <- c("municipio", "micro", "meso", "corede", "regfunc" ,"estado")
    if((missing(ag)) || (!ag %in% ags)){stop(paste0("Error: Select a valid argument for ag. The ag argument is only available for "),
                          paste(ags, collapse = ", "))}



    #Check available periods for var_id and save it to an object for later usage
    var_id_warning <- c()
    for(i in 1:length(var_id)){

      print(paste0("Checking available periods for var_id ", var_id[i]))

      withCallingHandlers({

        available_periods <- available(var_id = var_id[i], ag = ag)

        if((is.null(available_periods$periods)) |
           (is.numeric(period) && any(!period %in% available_periods$periods))){

          warning(available_periods$message)
        }

      }, warning = function(w){

        var_id_warning <<- c(var_id_warning, var_id[i])

        #Print warning
        message("Warning: ", conditionMessage(w))

        # Avoid double print
        invokeRestart("muffleWarning")
      })
    }





    #Drops var_ids with warning message
    print(paste0("var_id's ",var_id_warning, " will be ignored"))

    var_id <- var_id[!var_id %in% var_id_warning]

    #output
    if(is.null(geo_id)){
      x <-
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
                 "&formato=json",
                 "&use_ibge_code=","1"
          ) |>
            httr::GET(timeout(1000000),
                      add_headers("Integra-Key" = get_api_key()),
                      config = config(ssl_verifypeer = 0)) |>
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
                 "&id=",z,
                 "&ag=",ag,
                 "&periodo=",period |> paste0(collapse = ","),
                 "&filtro=",geo_id |> paste0(collapse = ","),
                 "&sort=",sort,
                 "&formato=json",
                 "&use_ibge_code=","1"
          ) |>
            httr::GET(timeout(1000000),
                      add_headers("Integra-Key" = get_api_key()),
                      config = config(ssl_verifypeer = 0)) |>
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
      dplyr::mutate(unit = as.integer(unit),
                    value =
                      value, #|>
                      # stringr::str_replace_all(pattern = "\\.", replacement = "") |>
                      # stringr::str_replace_all(pattern = ",", replacement = "\\.") |>
                      # as.numeric(),
                      #as.character(),
                    year = as.integer(year)) |>
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

      #break var_name
      if(var_name_break == TRUE){
        x <-
          x |>
          dplyr::mutate(var_name = stringr::str_sub(var_name, start = 2)) |>
          tidyr::separate_wider_delim(
            cols = var_name,
            delim = "\\",
            too_few = "align_start",
            names_sep = "_"
          )
      }
    }

    return(x)

  }
