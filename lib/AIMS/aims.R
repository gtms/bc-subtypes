## Wrapper function to retrieve subtype predictions defined by the Absolute
## Assignement of Breast Cancer Intrinsic Molecular Subtype algorithm (AIMS)
##
## * getAimsPreds
getAimsPreds <- function(e.mtx, a.dfr) {
    rqrdLibs <- "AIMS"
    sapply(rqrdLibs, function(lib) {
        if(sum(grepl(lib, search())) == 0) require(lib, character.only = TRUE)
    })
    eSet <- ExpressionSet(assayData = e.mtx)
    aimsRes.lst <- applyAIMS(eset = eSet,
                             EntrezID = a.dfr[["EntrezGene.ID"]])
    dfr <- as.data.frame(aimsRes.lst[["cl"]])
    names(dfr) <- "AIMS"
    dfr[["AIMS"]] <- ordered(dfr[["AIMS"]],
                             levels = c("Basal",
                                        "Her2",
                                        "LumA",
                                        "LumB",
                                        "Normal"))
    dfr
}
