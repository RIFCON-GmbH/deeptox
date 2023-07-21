# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
## 
## /!\ Note: if you want to change the name of your app during development, 
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
## 
golem::fill_desc(
  pkg_name = "deeptox", # The Name of the package containing the App 
  pkg_title = "A User-Friendly R-Shiny Application To Make DEB-TKTD Projections Of Sublethal Effects Of Chemical Exposure", # The Title of the package containing the App 
  pkg_description = "This tool makes predictions of sublethal effects based on different exposure profiles and user-provided input parameters. It is based on the simplified DEB-TKTD model described by Jager (2020). The predicted output is the multiplication factor that would need to be applied to a given exposure profile to reduce growth or reproduction by X% (termed EPx by EFSA). The results are then directly compiled in a basic report document.", # The Description of the package containing the App 
  author_first_name = "Torben", # Your First Name
  author_last_name = "Wittwer", # Your Last Name
  author_email = "Torben.Wittwer@rifcon.de", # Your Email
  repo_url = NULL # The URL of the GitHub Repo (optional) 
)     

desc::desc_add_author(
  given = "Oliver",
  family = "Jakoby",
  email = "",
  role = c('aut'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "Thomas",
  family = "Martin",
  email = "",
  role = c('aut'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "Neil",
  family = "Sherborne",
  email = "",
  role = c('aut'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "Roman",
  family = "Ashauer",
  email = "",
  role = c('aut'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "Tjalling",
  family = "Jager",
  email = "",
  role = c('rev'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "Syngenta",
  family = "",
  email = "",
  role = c('fnd', 'cph'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_add_author(
  given = "RIFCON GmbH",
  family = "",
  email = "",
  role = c('cph'),
  comment = NULL,
  orcid = NULL,
  file = ".",
  normalize = FALSE
)

desc::desc_set("NeedsCompilation","yes")


## Set {golem} options ----
golem::set_golem_options()
golem::set_golem_wd(here::here())

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl3_license()  # You can set another license here
usethis::use_readme_md( open = FALSE )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Use Recommended Packages ----
golem::use_recommended_deps()


## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

