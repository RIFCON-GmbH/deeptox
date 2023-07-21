#' Function to read TOXSWA exposure profiles from out-files
#' 
#' Read several expsoure profiles at their corresponding pathes at 'pathtofiles'
#'
#' @param pathtofiles path to toxswa files that should be read
#' @param profileNames a vector of names to be used for the loaded toxswa files. if "NA" the filename without extension will be used
#'
#' @export
#'
#' @return list of exposure profiles each with two columns (time and concentration)
readToxswaExpProfiles <- function(pathtofiles, profileNames=NA){
  if ( any(sapply(profileNames, is.na)) ) {
    profileNames <- basename(pathtofiles) %>% 
      tools::file_path_sans_ext()
  } else {
    profileNames <- profileNames
  }
  profiles <- lapply(pathtofiles, extract_focus_profile)
  names(profiles) <- profileNames

  flag_null <- do.call(c, lapply(profiles, is.null))
  profiles <- profiles[!flag_null]

  profiles <- lapply(setNames(names(profiles), names(profiles)),
                              function(x){
                                split_toxswa_exposure_profile(profiles[[x]], x)
                              })
  profiles <- flatten_list(profiles)
  
  profiles <- lapply(profiles, melt_Focus_profiles)

  return(profiles)
}

#' Check if the selected files are in a supported TOXSWA format
#'
#' @param file_names the name of the file that is going to be imported
#' @param profileNames the short names for the profiles
#'
#' @return an HTML string indicating which file
#' @importFrom shiny HTML
pre_check_toxswa_files <- function(file_names, profileNames){

  out <- do.call(c, lapply(1:length(file_names), function(i){
    lin <- readLines(file_names[i], encoding = "UTF-8", n = 20)
    isToxswa <- check_if_TOXSWA(lin)
    if ( !isToxswa ){
      return(paste0(profileNames[i], ": not a toxswa file"))
    }
    
    is_supported_version <- check_if_TOXSWA_version(lin, supported_version = c("4","5.5.3"))
    if ( !is_supported_version ){
      return(paste0(profileNames[i], ": unsupported TOXSWA version"))
    }
    
    return(NULL)
  }))
  
  HTML(
    paste0("<div style=\"color: red; padding-top: 0px; vertical-align: top; margin-top: 0px; margin-bottom: 5px; font-size: x-small;\">",
           paste0(out, collapse="<br>"),
           "</div>"
           )
    )

}


#' Extract a focus profile from a single .out file
#'
#' @param file_name 
#' @param layer_string 
#' @param substance_string 
#' 
#' @import stringr
#' @import magrittr
#' @import dplyr
#' @import lubridate
#' @import units
#'
#' @return exposure profiles
#' @noRd
extract_focus_profile <- function(
    file_name,
    layer_string = "ConLiqWatLay",
    substance_string = ""
) {
  
  lin <- readLines(file_name, encoding = "UTF-8")
  
  isToxswa <- check_if_TOXSWA(lin)
  if ( !isToxswa ){
    message("file is not a toxswa file")
    return(NULL)
  }
  
  is_supported_version <- check_if_TOXSWA_version(lin, supported_version = c("4","5.5.3"))
  if ( !is_supported_version ){
    message(paste0("Toxswa version is not supported. So far only version 4 and 5.5.3 are supported."))
    return(NULL)
  }
  
  unit <- lin %>%
    stringr::str_subset(
      paste0("\\* Unit for ", head(stringr::str_split(layer_string, "_")[[1]],1), " is")
    ) %>%
    stringr::str_split("[\\(\\)]") %>%
    unlist() %>%
    magrittr::extract(2) %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\.", " ")
  
  lin %>%
    stringr::str_subset(stringr::str_c(layer_string, "_", substance_string)) %>%
    stringr::str_split(" +") %>%
    sapply(function(x) c(x[3:4], tail(x,1))) %>%
    t() %>%
    dplyr::as_tibble(.name_repair = ~paste0("V", seq_along(.))) %>%
    dplyr::transmute(
      Layer_Specifier = as.character(V2),
      DateTime = as.character(V1) %>% lubridate::dmy_hm(),
      `Time` = units::set_units((DateTime - DateTime[1]) / lubridate::ddays(1), d),
      Exposure = units::set_units(as.numeric(V3), unit, mode = "standard")
    ) %>%
    tibble::add_column(
      RunID =
        stringr::str_split(file_name, "[/\\\\]")[[1]] %>%
        tail(1) %>%
        stringr::str_extract("[[:digit:]]+"),
      .before = "DateTime"
    ) %>%
    dplyr::select(RunID, Layer_Specifier, DateTime, `Time`, Exposure)  %>%
    dplyr::rename_at("Exposure", ~ paste0("Concentration"))
}



#' Split a profile into a list if more then one active substances was detected
#'
#' @param profile the toxswa profile as a tibble
#' 
#' @importFrom tibble tibble
#'
#' @return a tibble or a list of tibbles if more than one active substance was identified
#'
#' @examples
#' dat <- data.frame(RunID = 123, 
#'   Layer_Specifier = rep(c("ConLiqWatLay_ai1","ConLiqWatLay_ai2"),10),
#'   Concentration = rlnorm(20,0,1)
#'   )
#'   deeptox:::split_toxswa_exposure_profile(dat)
split_toxswa_exposure_profile <- function(profile, runid=NA){
  
  # ai holds the name of the active ingredient for each point in the time series
  ai <- sub("^ConLiqWatLay_(.+)$","\\1",profile[["Layer_Specifier"]])
  
  if (length(unique(ai)) == 1 ){ # if only one ai return the profile
    return(profile)
    
  } else if (length(unique(ai)) > 1){ # if more than one split the time series in a list
    if (is.na(runid)){
      runid_ <- profile[1,"RunID"]
    } else {
      runid_ <- runid
    }
    out <- split(profile, profile[,"Layer_Specifier"])
    names(out) <- paste0(runid_,"_",unique(ai))
    return(out)
    
  } else {
    return(NULL)
  }
  
}


#' Convert the unit of a column into a target unit
#'
#' @param tibble_object 
#' @param colname 
#' @param target_unit 
#' 
#' @import dplyr
#' @import units
#'
#' @return a tibble object with converted units
#' @noRd
convert_unit_focus_profile <- function(tibble_object, colname, target_unit){
  if( !length(tibble_object) ){
    return(NULL)
  }
  tibble_object %>%
    dplyr::mutate("{colname}" := units::set_units(get(colname), "g/L", mode="standard"))
}


#' Checks if the input contains the default TOXSWA keywords in the header
#'
#' @param ln 
#'
#' @return TRUE if header matches the default TOXSWA header
#' @noRd
#' @examples
#'  ln <- c("TOXSWA REPORT: Header",
#'  "* Results from the TOXSWA model  (c) Alterra",
#'  "* FOCUS  TOXSWA version   : 4",
#'  "* TOXSWA model version    : 3.3.4",
#'  "* TOXSWA created on       : 02-Apr-2015")
#'  deeptox:::check_if_TOXSWA(ln)
check_if_TOXSWA <- function(ln){
  ln <- ln[1:20]
  strings_to_match <- c(
    "^\\* Results from the TOXSWA model\\s*\\(c\\)",
    "^\\* FOCUS\\s*TOXSWA version\\s*:",
    "^\\* TOXSWA model version\\s*:",
    "^\\* TOXSWA created on\\s*:"
    )
  
  contains_strings <- do.call(c,lapply(strings_to_match, function(x) any(grepl(pattern=x, ln))))
  
  !any(!contains_strings)
  
}

#' Reduce the focus exposure profile
#' 
#' Reduce to a two column data.frame with first column indicating the time
#' and the second column the concentration
#'
#' @param profile 
#' @param time_col 
#' @param conc_col 
#'
#' @import dplyr
#' @import units
#'
#' @return the reduced FOCUS profile
#' @noRd
melt_Focus_profiles <- function(profile, time_col = "Time", conc_col =  "Concentration"){
  out <- profile %>% 
    dplyr::select(c(time_col,conc_col)) %>%
    units::drop_units() %>% 
    as.data.frame()
  
  names(out) <- c("V1","V2")
  
  return(out)
}



#' Checks if the toxswa version is as specified
#'
#' @param ln a vector of strings
#' @param supported_version a string of the version that is looked for
#'
#' @return TRUE if version numbers match
#'
#' @examples
#' ln <- c("* ------------------------------------------------------------------------------",
#' "* Results from the TOXSWA model  (c) Wageningen University & Research",
#' "* FOCUS  TOXSWA version   : 5.5.3",
#' "* TOXSWA model version    : 3.3.6",
#' "* TOXSWA created on       : 17-Dec-2017")
#' deeptox:::check_if_TOXSWA_version(ln, supported_version = "5.5.3")
#' 
#'  ln2 <- c("TOXSWA REPORT: Header",
#'  "* Results from the TOXSWA model  (c) Alterra",
#'  "* FOCUS  TOXSWA version   : 4",
#'  "* TOXSWA model version    : 3.3.4",
#'  "* TOXSWA created on       : 02-Apr-2015")
#' deeptox:::check_if_TOXSWA_version(ln2)
check_if_TOXSWA_version <- function(ln, supported_version = c("4", "5.5.3") ){
  regex_version <- "^\\*\\s+FOCUS\\s+TOXSWA\\s+version"
  Toxswa_version_line <- grep(regex_version, ln, value = TRUE)
  Toxswa_version <- sub(paste0(regex_version,"\\s+:\\s*(.*)$"),"\\1", Toxswa_version_line)
  Toxswa_version %in% supported_version
}






