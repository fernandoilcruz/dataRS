

get_api_key <- function() {
  key <- Sys.getenv("MY_API_KEY")
  if (is.null(key)) {
    stop("API key not found. Please set it using set_api_key().", call. = FALSE)
  }
  return(key)
}

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
                        paste(ags, collapse = ", "))}

  # Construct the URL
  root_url <- "https://datavis.dee.rs.gov.br/api/"

  info_url <- paste0(info,".php?")

  key_url <- paste0("key=",api_key)

  base_url <- paste0(root_url, info_url, key_url)

  return(base_url)

  # # Make the request
  # response <- httr::GET(full_url)
  #
  # # Check for errors
  # if (httr::status_code(response) != 200) {
  #   stop("Failed to fetch data: ", httr::content(response, "text"), call. = FALSE)
  # }
  #
  # # Parse and return the content
  # return(httr::content(response, "parsed"))
}
