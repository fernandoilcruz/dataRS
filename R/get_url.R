#' Build API URL (internal)
#'
#' Internal helper that constructs the full API URL using the provided endpoint name.
#'
#' @param info A string indicating which endpoint to access.
#' @return A full URL string.
#' @noRd
get_url <- function(info) {

  api_key <- get_api_key()

  #check available arguments
  infos <- c("arvore",
            "var_list",
            "var",
            "data_aval",
            "data",
            "ag",
            "composicao_ag",
            "composicao_ag_all",
            "fonte",
            "unid_medida")

  if(!info %in% infos){stop(paste0("Error: the info argument is only available for "),
                        paste(infos, collapse = ", "))}

  # Construct the URL
  root_url <- "https://datavis.dee.rs.gov.br/api/"
  #root_url <- "10.112.42.22/api/"

  info_url <- paste0(info,".php?")

  key_url <- paste0("key=",api_key)

  base_url <- paste0(root_url, info_url, key_url)

  return(base_url)
}
