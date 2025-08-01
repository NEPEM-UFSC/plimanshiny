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
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()
desc::desc_normalize(file = ".")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "mosaic_prepare")
golem::add_module(name = "shapefile_prepare")
golem::add_module(name = "download_excel")
golem::add_module(name = "download_mosaic")
golem::add_module(name = "download_shapefile")
golem::add_module(name = "indexes")
golem::add_module(name = "analyze")
golem::add_module(name = "manipula")
golem::add_module(name = "crop")
golem::add_module(name = "mask")
golem::add_module(name = "plotclip")
golem::add_module(name = "bindlayer")
golem::add_module(name = "interpolate")
golem::add_module(name = "aggregate")
golem::add_module(name = "resample")
golem::add_module(name = "segment")
golem::add_module(name = "imageimport")
golem::add_module(name = "imageanal")
golem::add_module(name = "imageindex")
golem::add_module(name = "imagesegment")
golem::add_module(name = "imagepalette")
golem::add_module(name = "slider")
golem::add_module(name = "sentinel")
golem::add_module(name = "colorpalette")
golem::add_module(name = "colorpalette")
golem::add_module(name = "measurediseasepal")
golem::add_module(name = "measurediseasepick")
golem::add_module(name = "timeseriesinput")
golem::add_module(name = "timeseriesanalysis")
golem::add_module(name = "cropbatch")
golem::add_module(name = "datasets")
golem::add_module(name = "dffilter")
golem::add_module(name = "dfedit")
golem::add_module(name = "dfupdate")
golem::add_module(name = "dfjoin")
golem::add_module(name = "spatjoin")
golem::add_module(name = "spatinterp")
golem::add_module(name = "matanalyze")
golem::add_module(name = "phanalyze")
golem::add_module(name = "utmzonesel")
golem::add_module(name = "geometricmeasures")
golem::add_module(name = "config")
golem::add_module(name = "timeseriesdsm")
golem::add_module(name = "summarize")
golem::add_module(name = "growthmodels")
golem::add_module(name = "vectorize")
golem::add_module(name = "growthmodelscurves")
golem::add_module(name = "graphicalexploration")
golem::add_module(name = "shapefilenative")
golem::add_module(name = "georeference")
golem::add_module(name = "compslider")
golem::add_module(name = "hyperspectral")
golem::add_module(name = "classify")
golem::add_module(name = "wheater")
golem::add_module(name = "userinfo")

## Add packages
usethis::use_package("shiny")
usethis::use_package("histoslider")
usethis::use_package("glue")

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers")
golem::add_utils("helpers")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")


## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("plimanshiny")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
