#' Consult available data
#'
#' @description Check time periods during which data is available for the selected variable and regional level combination.
#'
#' @param var_id Variable's ID.
#' @param ag Aggregation level
#'
#' @return A list
#' @import httr jsonlite tidyverse
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' available(ag ="municipio",var_id = 4845)
#' available(ag ="meso",var_id = 4845)
#' }


available <-
  function(var_id,
           ag){

  #url
  url <-
    paste0(
      get_url(info = "data_aval"),
      "&id=",var_id,
      "&ag=",ag
    )


  #output
  x <-
    url |>
    httr::GET(timeout(1000000),
              add_headers("Integra-Key" = get_api_key()),
              config = config(ssl_verifypeer = 0)) |>
    content("text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    tibble::as_tibble() |>
    pull()

  #length of the output
  n <-
    x |>
    length()

  #Results
  if(n == 0){
    message <- paste0("No avaliable data for this combination of variable and regional level")

    periods <- NULL
  }else{
    message <-
      paste0(
        c("For this combination of var_id and ag, data is avaliable for the following years: ",
          paste0(x, collapse = ",",recycle0 = F)),
        collapse = "")

    periods <- x
  }

  return(list("message" = message,
              "periods" = periods))

}
