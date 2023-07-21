#' individualExpProfile
#'
#' @param summaryResults 
#' @param simulationSettings 
#' @param plotResults 
#' @param profileResults 
#' @param expProfiles 
#' @param time_unit 
#' 
#' @import officer
#' @import officedown
#' @noRd
individualExpProfile <- function(summaryResults, simulationSettings, plotResults, profileResults, expProfiles, time_unit = "d"){
  profileNames <- summaryResults[,1]
  
  
  lapply(profileNames, function(selProfile){
    
    #selProfile <- profileNames[1]
    passFail_results <- summaryResults[summaryResults[,1]==selProfile,3]
    EPx <- summaryResults[summaryResults[,1]==selProfile,3]
    critEndpoint <- summaryResults[summaryResults[,1]==selProfile,4]
    
    startDay <- head(profileResults[[selProfile]][,"startTime"],1)
    endDay <- tail(profileResults[[selProfile]][,"startTime"],1)
    
    # **If** EPx >passFail
    text1 <- paste0(
      "For the profile ",selProfile,", the EP~",simulationSettings[["ECpct"]],
      "~ multiplier was predicted to be >", simulationSettings[["cutOff"]],
      " for all virtual toxicity tests. As the EP~",simulationSettings[["ECpct"]],
      "~ multiplier exceeded the user specified ",
      "cutoff value for all parts of the profile, an exact value was not calculated. ",
      "A ",simulationSettings[["ECpct"]],"% reduction in growth, ",
      "reproduction, or survival would not be expected unless the exposure ",
      "profile were multiplied by a factor of >",simulationSettings[["cutOff"]],". ",
      "The risk assessment criterion, requiring the EP~",simulationSettings[["ECpct"]],
      "~ to exceed the critical value of ",simulationSettings[["passFail"]]," is therefore met.  \n","  \n")
    
    # **Elseif** EPx < passFail && pass_fail_results == “Pass”
    text2 <- paste0(
      "The lowest predicted EP~",simulationSettings[["ECpct"]],
      "~ multiplier for the profile ",selProfile," was ",EPx," and the critical endpoint ",
      "was ",critEndpoint,". This was predicted for the virtual toxicity test(s) lasting ",
      "from ",startDay," ",time_unit," to ",endDay," ",time_unit," of the exposure profile. ", 
      "Therefore, a ",simulationSettings[["ECpct"]],"% or higher reduction in ",critEndpoint," ",
      "would not be expected unless the exposure profile were multiplied by a factor of at least ",EPx,". ",
      "As this value exceeds the critical value of ",simulationSettings[["passFail"]],", the risk assessment criterion is met.  \n","  \n")
    
    
    # **Else**
    text3 <- paste0(
      "The lowest predicted EP~",simulationSettings[["ECpct"]],"~ multiplier for the profile ", selProfile," ",
      "was ",EPx," and the critical endpoint was ",critEndpoint,".  This was predicted ",
      "for the virtual toxicity test(s) lasting from ",startDay," ",time_unit," to ",endDay," ",time_unit,
      " of the exposure profile. Therefore, a ",simulationSettings[["ECpct"]],"% reduction in ",
      critEndpoint," would be expected if the exposure profile were multiplied by a factor of ",EPx,". ",
      "As this value is below the critical value of ",simulationSettings[["passFail"]],", the risk assessment criterion is not met.  \n","  \n")
    
    # **end**
    
    
    
    if (EPx >= simulationSettings[["cutOff"]])
      outtext <- text1
    else if (EPx < simulationSettings[["cutOff"]] && EPx > simulationSettings[["passFail"]])
      outtext <- text2
    else
      outtext <- text3
    
    
    # Add headline and text ####
    cat(paste0("## ",selProfile,"  \n  \n"))
    cat(outtext)
    
    # Add exposure profiles plot ####
    # print(expProfiles[[selProfile]]+ggplot2::ggtitle(""))
    print(plotExpProfiles(data=expProfiles[[selProfile]], x="V1", y="V2", title=selProfile))
    figNum1 <- officer::run_autonum(seq_id = "Figure",
                                    pre_label = "Figure\u00A0",
                                    post_label = ": ",
                                    bkm = paste0("expProfPlot_", selProfile))
    knit_print_block(block_caption(paste0("Exposure profile \"", selProfile,"\""),
                                   style = "Image Caption", autonum = figNum1))
    
    
    cat("  \n  \n")
    
    
    # Add EPx plot ####
    print(plotResults[[selProfile]]+ggplot2::ggtitle(""))
    figNum2 <- officer::run_autonum(seq_id = "Figure",
                                   pre_label = "Figure\u00A0",
                                   post_label = ": ",
                                   bkm = paste0("epxplot_", selProfile))
    knit_print_block(block_caption(paste0("EPx plot for exposure profile \"", selProfile,"\""),
                                   style = "Image Caption", autonum = figNum2))

    
    cat("  \n  \n")
  })
  
  
}



#' makeParSettingTable
#'
#' @param x 
#' @param paraDescTable 
#' @param digits 
#'
#' @return table for the paremeters and settings
#' 
#' @import magrittr
#' 
#' @noRd
makeParSettingTable <- function(x, paraDescTable, digits=3){

  x.df <- do.call(data.frame, x)
  whichNum <- which(sapply(x.df , class) == "numeric")
  x.char <- x.df
  x.char[,whichNum] <- x.char[,whichNum] %>% round(3)
  x.char <- data.frame(Parameter=names(x.char), Value=t(x.char))
  
  orderParaDescTable <- match(x.char[,"Parameter"], paraDescTable[,"parameter"])
  x.char <- data.frame(x.char, Description=paraDescTable[orderParaDescTable,"description"])
  
  # rename to more indicative label if available
  x.char[,"Parameter"] <- do.call(c, lapply(x.char[,"Parameter"], function(parname){
    convertParameterNameToLabel(
      getParaInfo(parname, "parameter"),
      getParaInfo(parname, "label")
    )
  }))
  
  return(x.char)
  
}
