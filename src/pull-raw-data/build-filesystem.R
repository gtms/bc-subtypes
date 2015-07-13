## 10Apr2015
## This script writes the required directories for raw-data .CEL files on disk

## * Functions
## ** makeDir
makeDir <- function(gse) {
    ids <- with(dsets.dfr, id[ref == gse & !is.na(ref)])
    sapply(ids, function(.id) {
        celPath <- sprintf("data/cel/%s", .id)
        normPath <- sprintf("data/normalized-data/%s", .id)
        dir.create(celPath, showWarnings = FALSE)
        dir.create(normPath, showWarnings = FALSE)
    })
    tarPath <- sprintf("data/tar/%s", gse)
    dir.create(tarPath, showWarnings = FALSE)
}

## * Load
dsets.dfr <- read.csv("data/csv/datasets.csv",
                      stringsAsFactors = FALSE)

## * Run
## create directories only for datasets for which .cel files are available
gses <- unique(with(dsets.dfr, ref[cel.files]))
sapply(gses, makeDir)

## * Deprecated versions
## ** makeDir
## makeDir <- function(id) {
##     celPath <- sprintf("data/cel/%s", id)
##     normPath <- sprintf("data/normalized-data/%s", id)
##     dir.create(celPath, showWarnings = FALSE)
##     dir.create(file.path(celPath, "tar"), showWarnings = FALSE)
##     dir.create(normPath, showWarnings = FALSE)
## }
