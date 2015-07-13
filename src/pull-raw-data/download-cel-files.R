## 10Apr2015
## This script downloads raw .CEL tarball files to disk, and extracts them into
## the dataset-specific directory

## * Preamble
## set .libPaths
.libPaths(".R-libraries")

## load libraries
library(ProjectTemplate)
load.project()

## * Functions
## ** downloadCEL
downloadCEL <- function(gse,
                        downloadTarball = TRUE,
                        moveCelFiles = TRUE) {
    tarDir <- file.path("data/tar", gse)
    ## abort if 'success' token found on disk
    if(file.exists(file.path(tarDir, "success"))) {
        return(message(sprintf("'success' token found on %s: skipping download of dataset %s.",
                               tarDir, gse)))
    }
    if(downloadTarball) {
        ## skip tarball download if tarball previously downloaded
        if (length(list.files(tarDir, "tar")) != 0) {
            message(sprintf("Dataset '%s' tarball found on %s: skipping download.",
                            gse, tarDir))
        } else {
            ## otherwise download tarball
            getGEOSuppFiles(gse, makeDirectory = FALSE, baseDir = tarDir)
            message(sprintf("Dataset '%s' tarball downloaded into %s.", gse, tarDir))
        }
    }
    if(moveCelFiles) {
        ## abort if tarball not present; otherwise untar
        tarFiles <- file.path(tarDir, list.files(tarDir, pattern = "tar"))
        if(length(tarFiles) == 0) stop(sprintf("No tarball found on %s.",
                                               tarDir))
        lapply(tarFiles, untar, exdir = tarDir)
        ## move extracted cel files to their final destinations
        pltfrms <- c("HG-U133_Plus_2", "HG-U133A", "HG-U133B")
        allFiles <- list.files(tarDir)
        allCelFiles <- allFiles[grepl("CEL", allFiles, ignore.case = TRUE)]
        lapply(allCelFiles, function(fn) {
            celFile <- file.path(tarDir, fn)
            hdr <- read.celfile.header(celFile)
            if(!hdr$cdfName %in% pltfrms) {
                message(sprintf("File %s was hybridized with chip %s:\nit will remain in directory %s.",
                                celFile,
                                hdr$cdfName,
                                tarDir))
            } else {
                .id <- with(dsets.dfr, id[ref == gse &
                                              !is.na(ref) &
                                              platform == hdr$cdfName &
                                                  !is.na(platform)])
                if(length(.id) == 0) stop(sprintf("Could not find destination for file %s.", celFile))
                destDir <- file.path("data/cel", .id)
                file.rename(from = celFile,
                            to = file.path(destDir, fn))
            }
        })
        ## create 'success' token on disk
        file.create(file.path(tarDir, "success"))
        message(paste(sprintf("All CEL files from dataset %s successfully extracted and moved",
                              gse),
                      "to respective destinations; 'success' token created on disk.",
                      sep = "\n"))
    }
}

## * Load
dsets.dfr <- read.csv("data/csv/datasets.csv",
                      stringsAsFactors = FALSE)

## * Run
## downloads tarball raw cel files, untar them, and move them to final
## destinations
dsets2dload <- unique(with(dsets.dfr, ref[cel.files & from == "geo"]))
system.time(lapply(dsets2dload, downloadCEL))

## dsets.lst <- lapply(na.omit(dsets2dload), getGEO)

## * Deprecated versions
## ** downloadCEL
## downloadCEL <- function(gse) {
##     id <- with(dsets.dfr, id[ref == gse])
##     dsetDir <- sprintf("data/cel/%s", id)
##     tarDir <- file.path(dsetDir, "tar")
##     ## abort if tarball previously downloaded
##     if (length(list.files(tarDir, "tar")) != 0) {
##         message(sprintf("Dataset '%s' tarball found on data/cel/%s/tar, skipping download.", id, id))
##         return(NULL)
##     }
##     ## otherwise download tarball and untar it
##     getGEOSuppFiles(gse, makeDirectory = FALSE, baseDir = tarDir)
##     ## and untar the cel files to the dataset directory
##     untar(file.path(tarDir, list.files(tarDir, pattern = "tar")),
##           exdir = dsetDir)
##     message(sprintf("Dataset '%s' downloaded.", id))
## }

## sapply(ids, function(.id) {
##         allCel <- list.files(tarDir)
##         allCel <- allCel[grepl("GSM", allCel)]
##         dsetDir <- sprintf("data/cel/%s", .id)
##         pltf <- with(dsets.dfr, platform[id == .id])
##         gsm2mv <- allCel[gsub(".CEL.gz", "", allCel) %in% gplSamples.lst[[pltf]]]
##         lapply(gsm2mv, function(gsm) {
##             file.rename(from = file.path(tarDir, gsm),
##                         to = file.path(dsetDir, gsm))
##         })
##     })
##     message(sprintf("Dataset '%s' downloaded.", gse))

## hack to find out on which platform each gsm was hybridized on
## platforms.dfr <- data.frame(gpl = c("GPL96", "GPL97", "GPL570"),
##                             affy = c("HG-U133A", "HG-U133B", "HG-U133PLUS2"))
## gpl.lst <- lapply(platforms.dfr$gpl, getGEO)
## gplSamples.lst <- lapply(gpl.lst, function(gpl) Meta(gpl)$sample_id)
## names(gplSamples.lst) <- platforms.dfr$affy
