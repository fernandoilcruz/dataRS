#' @title deetree
#'
#' @description
#' This function function plots the DEEDados tree. It is useful for exploring the available datasets at DEEDados.
#'
#'
#' @return A Json Tree
#'
#' @import jsonlite jsTreeR httr tidyverse shiny
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' deetree()
#' }
#'
deetree <-
  function(){

    url_arvore <- get_url(info = "arvore")

    api <- httr::GET(url_arvore, timeout(1000000), config = config(ssl_verifypeer = 0))

    api_content <- base::rawToChar(api$content)

    data <- jsonlite::fromJSON(api_content,
                                simplifyVector = FALSE)



    # UI
    ui <-
      fluidPage(
        tags$head(
        tags$style(HTML("
      #tree-container {
        width: 100%;
        height: 1000px;
        overflow: auto;
        border: 1px solid #ddd;
        padding: 10px;
      }
    "))
      ),

      tags$div(
        id = "tree-container",
        jsTreeR::jstree(data$children, checkboxes = FALSE, search = TRUE)
      )
    )

    # Server (empty since no dynamic behavior needed)
    server <- function(input, output, session) {}

    # Run App
    shinyApp(ui, server)

  }
