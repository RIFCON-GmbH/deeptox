# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
attachment::att_amend_desc()
usethis::use_package( "tidyr" )
usethis::use_package( "xfun" )
usethis::use_package( "pkgbuild" )
usethis::use_package( "flextable" )
usethis::use_package( "glue" )
usethis::use_package( "pkgload" )
usethis::use_package( "attachment", type = "Suggests" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "projectExportImport" ) 
golem::add_module( name = "welcome" ) 
golem::add_module( name = "trackInputDatVSResults" ) 
golem::add_module( name = "parameters" ) 
golem::add_module( name = "parameterInputGroup" ) 
golem::add_module( name = "parameterInput" )
golem::add_module( name = "exposureProfiles" ) 
golem::add_module( name = "exposureProfilesImport" ) 
golem::add_module( name = "results" ) 
golem::add_module( name = "exportResults" ) 
golem::add_module( name = "electronTop" )

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "expProfiles" )
golem::add_fct( "parameters" )
golem::add_fct( "windows" )
golem::add_fct( "predictions" )
golem::add_fct( "results" )
golem::add_fct( "helpers" )
golem::add_fct( "export" )
golem::add_fct( "customWidgets" )
golem::add_fct( "dashboardTheme" )
golem::add_fct( "report" )
golem::add_fct( "exportResults" )
golem::add_fct( "markdown_fcts" )
golem::add_fct( "js_fcts" )
golem::add_fct( "check_window_length" )
golem::add_fct( "toxswa_profiles" )
golem::add_utils( "markdown_helpers" )
golem::add_utils( "textformat_helpers" )
golem::add_utils( "textblocks" )
golem::add_utils( "unicode_chars" )

## Add non-R code ----
usethis::use_c(name = "debKiss_in_c_v3.c")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "isElectron.js" )
golem::add_css_file( "custom" )
golem::add_css_file( "electron-specific" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "deeptox_ExampleDataset", open = TRUE ) 

# Documentation
## Vignette ----
usethis::use_vignette("deeptox")
usethis::use_vignette("toxswa_import")
devtools::build_vignettes()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

