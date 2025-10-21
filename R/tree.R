#' @title tree
#'
#' @description
#' This function function plots the DataRS tree. It is useful for exploring the available datasets at DataRS.
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
tree <-
  function(){

    url_arvore <- get_url(info = "arvore")

    api <- httr::GET(url_arvore, timeout(1000000), config = config(ssl_verifypeer = 0))

    api_content <- base::rawToChar(api$content)

    data <- jsonlite::fromJSON(api_content,
                               simplifyVector = FALSE)

    # Recursively update the text field to include id_variavel if present
    add_id_to_text <- function(node) {
      if (!is.null(node$id_variavel)) {
        node$text <- paste0(node$text, " (id: ", node$id_variavel, ")")
      }

      # If the node has children, recursively process them
      if (!is.null(node$children)) {
        node$children <- lapply(node$children, add_id_to_text)
      }

      return(node)
    }

    # Apply the function to each top-level node in the tree
    modified_children <- lapply(data$children, add_id_to_text)



    # UI
    ui <- fluidPage(
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
        jsTreeR::jstree(modified_children, checkboxes = FALSE, search = TRUE)
      )
    )

    server <- function(input, output, session) {}

    shinyApp(ui, server)

  }
