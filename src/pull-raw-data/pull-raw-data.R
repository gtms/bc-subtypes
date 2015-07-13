## 11Apr2015
## Run this script to download raw-data for all datasets referenced on
## data/csv/datasets.csv

## * Preamble
## set .libPaths
.libPaths(".R-libraries")

## load libraries
library(ProjectTemplate)
load.project()

## scripts location
srcDir <- "src/pull-raw-data"

## * Source
## create filesystem
source(file.path(srcDir, "build-filesystem.R"))

## download
source(file.path(srcDir, "download-cel-files.R"))
