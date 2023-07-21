#' generateReport
#'
#' @param x list of parameters that is passed to the markdown file
#' @param file the filename of the output docx
#' @param session the current shiny session
#'
#' @importFrom rmarkdown render
#' @noRd
generateReport <- function(x, file=NA, session){

  show_modal_spinner(spin="orbit")#, color="#859729") # show the modal spinner
  
  # Copy the report file to a temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  sessionTmpDir <- paste0(tempdir(),"/",session$token)
  
  tmp_exists <- file.exists(sessionTmpDir)
  if (!tmp_exists)
    dir.create(sessionTmpDir)
  
  if (tmp_exists){
    message(paste0("tempdir ", sessionTmpDir," exists. Attempt to copy template."))
  }
  
  tempReport <- file.path(sessionTmpDir, "report.Rmd")
  sourcePath <- paste0(system.file(package="deeptox"),"/app/www/report.Rmd")#"inst/app/www/report.Rmd"
  
  fc_ok <- file.copy(sourcePath, tempReport, overwrite = TRUE)
  
  if (!fc_ok){
    warning("Copy of template report for markdown did not succeed.")
    remove_modal_spinner() # remove it when done
    genReportModal(type="fail")
    out <- 0
  } else {
    message("Copy of report.Rmd template successful.")
    
    if (!is.na(file))
      outfile <- file
    else
      outfile <- paste0(tools::file_path_sans_ext(tempReport),".docx")
  
    out <- rmarkdown::render(tempReport,
                             output_file = outfile,
                             #output_dir = sessionTmpDir,
                             intermediates_dir=sessionTmpDir,
                             knit_root_dir=sessionTmpDir,
                             output_format="officedown::rdocx_document",
                             params=x,
                             clean=TRUE
                      )
    
    file.remove(tempReport)
    unlink(sessionTmpDir, recursive = T)
    remove_modal_spinner() # remove it when done

  }
  
  

  return(out)
}



#' genReportModal
#'
#' Display a modal with error message if executed
#'
#' @return a modal
#' @noRd
genReportModal <- function(type){
  if (type=="fail"){
    showModal(
      modalDialog(title = "Error",
                  "The report could not be generated. Please contact the system administrator."
                  )
      )
  }else{
    showModal(
      modalDialog(title = "",
                  "Report successfully generated."
      )
    )
  }
}
