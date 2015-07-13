## 13Apr2015
## This script produces normalized matrices of expression with custom cdf
## files.

## * Preamble
## set .libPaths
.libPaths(".R-libraries")

## load libraries
library(ProjectTemplate)
load.project()

## * Functions
## ** baNormalize
## Normalizes batch of CEL files mapped with BrainArray custom cdf files using
## RMA and MAS5 algorithms.  Returns list with eSet normalized with each
## algorithm, plus character vector identifying cdf file used.
baNormalize <- function (dset.id) { # refer to data/csv/datasets.csv
    dsetDir <- sprintf("data/cel/%s", dset.id)
    dsetPltfrm <- with(dsets.dfr, platform[id == dset.id])
    cdfName <- switch(dsetPltfrm,
                      "HG-U133A" = "hgu133a2hsensgcdf",
                      "HG-U133B" = "hgu133bhsensgcdf",
                      "HG-U133_Plus_2" = "hgu133plus2hsensgcdf")
    library(eval(cdfName), character.only = TRUE)
    affyBatch <- ReadAffy(celfile.path = dsetDir, cdfname = cdfName)
    eSet.lst <- list()
    eSet.lst$rma <- rma(affyBatch)
    eSet.lst$mas5 <- mas5(affyBatch)
    prbs <- rownames(exprs(eSet.lst[["rma"]]))
    ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
    filters <- "ensembl_gene_id"
    attributes <- c("entrezgene", "hgnc_symbol", "description")
    a.dfr <- getBM(attributes = c(filters, attributes),
                   filters = filters,
                   values = gsub("_at", "", prbs),
                   mart = ensembl)
    names(a.dfr) <- c("probe", "EntrezGene.ID", "hgnc.symbol", "description")
    gse.id <- with(dsets.dfr, ref[id == dset.id])
    phenoPath <- file.path("data/csv/pheno", gse.id, sprintf("%s.csv", gse.id))
    maxFields <- max(count.fields(phenoPath, sep = ","))
    cc.dfr <- read.csv("data/csv/pheno/phenoColClass.csv",
                       stringsAsFactors = FALSE)
    null.dfr <- data.frame(colName = rep("NULL", maxFields - nrow(cc.dfr)),
                           colClass = rep("NULL", maxFields - nrow(cc.dfr)))
    cc.dfr <- rbind(cc.dfr, null.dfr)
    p.dfr <- read.csv(phenoPath,
                      comment.char = "#",
                      colClasses = with(cc.dfr,
                                        setNames(colClass, colName)))
    rownames(p.dfr) <- p.dfr[["geo.accn"]]
    lapply(eSet.lst, function(eSet) {
        expr.mtx <- exprs(eSet)
        rownames(expr.mtx) <- gsub("_at", "", rownames(expr.mtx))
        expr.mtx <- expr.mtx[a.dfr$probe, ]
        colnames(expr.mtx) <- toupper(gsub("\\.cel\\.gz", "", colnames(expr.mtx),
                                           ignore.case = TRUE))
        p.dfr <- p.dfr[colnames(expr.mtx), ]
        ## collapse probes by Ensembl ID
        .sums <- rowSums(expr.mtx, na.rm = TRUE)
        ids2retain <- tapply(1:nrow(expr.mtx),
                             a.dfr$probe,
                             function(x) x[which.max(.sums[x])])
        .fData <- a.dfr[ids2retain, ]
        rownames(.fData) <- .fData$probe
        eSet <- ExpressionSet(assayData = expr.mtx[ids2retain, ],
                              phenoData = AnnotatedDataFrame(p.dfr),
                              featureData = AnnotatedDataFrame(.fData))
    })
}

## * Load
dsets.dfr <- read.csv("data/csv/datasets.csv",
                      stringsAsFactors = FALSE)

## * Run
dsets2norm <- with(dsets.dfr, id[cel.files])

lapply(dsets2norm, function (dset.id) {
    ## tokens
    message(sprintf("Now normalizing dataset '%s' . . . ", dset.id),
            appendLF = FALSE)
    on.exit(message(sprintf("Dataset '%s' normalized!", dset.id)))
    ## normalize with brain array cdf files
    norm.lst <- baNormalize(dset.id)
    ## save
    normDir <- file.path("data/normalized-data", dset.id)
    ## rma
    rma.eSet <- norm.lst$rma
    saveRDS(rma.eSet,
            file = file.path(normDir, sprintf("%s-rma.Rds", dset.id)))
    ## mas5
    mas5.eSet <- norm.lst$mas5
    saveRDS(mas5.eSet,
            file = file.path(normDir, sprintf("%s-mas5.Rds", dset.id)))
})

## * Exit
sessionInfo()
q(save = "no")
