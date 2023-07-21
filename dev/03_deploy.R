# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
#devtools::run_examples() # sometimes it is better to check the examples before check() since this goes faster. If the examples are fine continue with the check()
devtools::check()
rhub::check_for_cran()

# Deploy
## increase the version number
usethis::use_version()
## add changelog
file.edit('news.md')

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager
pkg_source <- devtools::build()
devtools::build(pkg_source, binary = TRUE)

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile()

## If you want to deploy to ShinyProxy
golem::add_dockerfile_shinyproxy()

## If you want to deploy to Heroku
golem::add_dockerfile_heroku()
