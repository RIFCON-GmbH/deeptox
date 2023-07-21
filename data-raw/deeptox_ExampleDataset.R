## code to prepare `deeptox_ExampleDataset` dataset goes here
pathname <- paste0(here::here(), "/inst/extdata/", "deeptox-2022-08-29_allProfiles.rds")
deeptox_ExampleDataset <- readRDS(pathname)

usethis::use_data(deeptox_ExampleDataset, overwrite = TRUE)
