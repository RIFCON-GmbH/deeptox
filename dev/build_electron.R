library(jsonlite)

# install electricShine from fork:
#remotes::install_github("erikvona/electricShine")

buildPath <- "C:\\temp\\electricDeEP_build"
nodejs_version <- "v14.19.3"
app_name <- "DeEP"

# create the package ####
electricShine::electrify(app_name = app_name,
                         product_name = "DeEP",
                         short_description = "A User-Friendly R-Shiny Application To Do Forward Projections Of Defined DEB-TKTD Models",
                         semantic_version = "1.0.0",
                         build_path = buildPath,
                         mran_date = NULL,
                         cran_like_url = "https://cran.r-project.org",
                         function_name = "run_app",
                         git_host = NULL,
                         git_repo = NULL,
                         local_package_path = "..\\deeptox_1.0.0.tar.gz",
                         pandoc_version = as.character(rmarkdown::pandoc_version()),
                         nodejs_version = nodejs_version,
                         r_version = "4.2.0",
                         run_build = FALSE,
                         permission = TRUE)


# Prepare to build the electron app ####
#set icon
icon_source <- system.file(package = "deeptox", "app", "www", "icon.ico")
icon_target <- paste0(buildPath,"/",app_name,"/","build/icon.ico")
file.copy(icon_source, icon_target, overwrite = TRUE)


package_json_path <- paste0(buildPath,"/",app_name,"/","package.json")
file.copy(package_json_path, paste0(package_json_path,"_backup"), overwrite = TRUE)
package_json_list <- jsonlite::fromJSON(package_json_path)
package_json_list[["description"]] <- "DeEP"
package_json_list[["publisherName"]] <- "Syngenta, Rifcon GmbH"

# help at https://www.electron.build/configuration/nsis
package_json_list[["build"]][["win"]] <- list(icon = "build/icon.ico")
package_json_list[["build"]][["nsis"]] <- list(oneClick = FALSE,
                                               perMachine = FALSE,
                                               allowElevation = TRUE,
                                               allowToChangeInstallationDirectory = FALSE,
                                               createDesktopShortcut = FALSE,
                                               createStartMenuShortcut = TRUE
                                               )

package_json <- jsonlite::toJSON(package_json_list, 
                                 pretty = TRUE, 
                                 auto_unbox = TRUE,
                                 null="null")
write(package_json, package_json_path)


# build electron app ####
electricShine::run_build_release(
  nodejs_path = file.path(system.file(package = "electricShine"), paste0("nodejs/node-",nodejs_version,"-win-x64")),
  app_path = paste0(buildPath,"/",app_name),
  nodejs_version = nodejs_version
)


