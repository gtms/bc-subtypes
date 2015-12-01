## This script normalizes raw data in the context of an array job

## * Preamble
## set .libPaths
.libPaths(".R-libraries")

## load libraries
library(ProjectTemplate)
load.project()

## source functions
source("lib/do-normalization.R")

## capture task id from bash script
tskId <- as.numeric(Sys.getenv("SGE_TASK_ID"))

## * Load
dsets.dfr <- read.csv("data/csv/datasets.csv",
                      stringsAsFactors = FALSE)

## * Run
dsets2norm <- with(dsets.dfr, id[cel.files])
## dset.id <- dsets2norm[tskId]
normMethods <- c("frma", "rma", "mas5")
isBrainarray <- c(TRUE, FALSE)

cbn.dtb <- data.table(expand.grid(dset.id = dsets2norm,
                                  normMethod = normMethods,
                                  isBrainarray = isBrainarray))
## cbn.dtb[, dset.id := dset.id]
cbn.dtb <- cbn.dtb[!(normMethod == "frma" & isBrainarray == TRUE)]
cbn.dtb <- cbn.dtb[!(grepl("-b$", dset.id) & normMethod == "frma")]
## (grepl("-b$", dset.id)) cbn.dtb <- cbn.dtb[normMethod != "frma"]

## apply doNorm to cbn.dfr
apply(cbn.dtb, 1, function(v) {
    isBrainarray <- as.logical(gsub("[[:space:]]+",
                                    "",
                                    v["isBrainarray"]))
    if(isBrainarray) {
        cdf <- "brainarray"
    } else {
        cdf <- "affymetrix"
    }
    ## tokens
    message(sprintf("Now normalizing dataset '%s', with %s (%s cdf files) . . . ",
                    v["dset.id"],
                    toupper(v["normMethod"]),
                    cdf),
            appendLF = FALSE)
    on.exit(message(sprintf("Dataset '%s' normalized!", dset.id)))
    doNorm(v["dset.id"],
           v["normMethod"],
           isBrainarray,
           writeDisk = TRUE)
})

## * Exit
sessionInfo()
q(save = "no")
