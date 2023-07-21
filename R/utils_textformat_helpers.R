
#' makeTextWithInfoButton
#'
#' @param mytext the main text after which the info button will appear
#' @param infoButtonId the id of info button that can be referenced by e.g. observeEvent
#' @param div_title the id of the html div-container. Value will show as tooltip if mouse hovers over the text
#'
#' @return html object
#'
#' @importFrom htmltools HTML
#' @importFrom shiny actionLink
#'
#' @examples
#' mytext <- "<b>Parameters and settings</b>"
#' infoButtonId <- "boxinfo"
#' div_title <- "BoxHeadline"
#' 
#' deeptox:::makeTextWithInfoButton(mytext, infoButtonId, div_title)
#' @noRd
makeTextWithInfoButton <- function(mytext, infoButtonId, div_title=""){
  
  if (div_title == "No information available")
    out <- mytext
  else{
    id <- sub("^(.*-).*$","\\1",infoButtonId)
    out <- HTML(
    paste0("<div title='",div_title,"'>", mytext),
    as.character(
      actionLink(inputId=infoButtonId,
                 label="",
                 icon=icon("info-circle"),
                 class="info-circle-link",
                 onclick=HTML(getPosition(infoButtonId, nsid=id)))
    ),
    "</div>")
  }
  
  return(out)
}
