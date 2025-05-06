#' List, filter and sort variables' ID and name
#'
#' @param sort Optional. A character string. Either "ASC" to sort from A to Z or "DESC" to sort from Z to A.
#' @param filters Optional. A character string to filter from the list of variables.
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
  function(
    #filters = NULL,
    sort="ASC"){

    #Return url
    if(is.null(sort)){
      url <- paste0("https://dados.dee.rs.gov.br/api/var_list.php")
    }else{
      if(sort == "ASC"){
        url <- paste0("https://dados.dee.rs.gov.br/api/var_list.php?sort=",sort)
      }else{
        if(sort == "DESC"){
          url <- paste0("https://dados.dee.rs.gov.br/api/var_list.php?sort=",sort)
        }else{
          stop("Error. Choose ASC or DESC when using the optional argument sort.")
        }
      }
    }

    #Return vars list
    vars_list <- jsonlite::fromJSON(url) |>
      tibble::as_tibble() |>
      dplyr::rename(var_id = id,
                    var_name = nome)


    # #Return vars list
    # if(is.null(filters)){
    #   vars_list <- jsonlite::fromJSON(url) |>
    #     tibble::as_tibble() |>
    #     dplyr::rename(var_id = id,
    #                   var_name = nome)
    # }else{
    #   vars_list <- jsonlite::fromJSON(url) |>
    #     tibble::as_tibble() |>
    #     dplyr::rename(var_id = id,
    #                   var_name = nome) |>
    #     dplyr::filter(stringr::str_detect(string = var_name, pattern = filters))
    # }

    return(vars_list)

  }
