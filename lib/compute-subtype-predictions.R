## Wrapper function to return, in a data frame, all genefu subtype predictions
## plus the output of the iC10 gene expression classifier

## * computePreds
computePreds <- function(e.mtx, # expression matrix
                         d.fac, # factor informing dataset composition for
                                        # standard correction
                         a.dfr, # annotation data frame, rownames must be the
                                        # same as mtx; must contain 'probe' and
                                        # 'EntrezGene.ID' columns
                         geneSymbols = NULL,
                         doMap = TRUE) {
    if(is.null(geneSymbols)) geneSymbols <- a.dfr[["hgnc.symbol"]]
    ## genefu
    gf.lst <- tapply(colnames(e.mtx),
                     d.fac,
                     function(clnms) {
                         getGenefuPreds(e.mtx[, clnms],
                                  a.dfr,
                                  doMapping = doMap)
                     })
    genefuPreds.dfr <- do.call(rbind, lapply(names(gf.lst), function(nm) {
        dfr <- gf.lst[[nm]]
        dfr$dataset <- nm
        dfr
    }))
    ## iC10
    ## collapse by gene symbol
    ## geneSymb <- a.dfr$hgnc.symbol
    geneSymb <- geneSymbols
    .sums <- rowSums(abs(e.mtx), na.rm = TRUE)
    .idx <- tapply(1:length(geneSymb),
                   geneSymb, 
                   function(x) x[which.max(.sums[x])])
    iC10.mtx <- e.mtx[.idx, ]
    rownames(iC10.mtx) <- geneSymb[.idx]
    ## compute iC10 predictions
    iC10Preds <- getIC10Preds(iC10.mtx)
    ## compute AIMS predictions
    AimsPreds <- getAimsPreds(e.mtx, a.dfr)
    ## return all predictions
    dfr <- as.data.frame(lapply(cbind(genefuPreds.dfr,
                                      iC10 = iC10Preds,
                                      AIMS = AimsPreds),
                                as.factor))
    cbind(sampleName = colnames(e.mtx), dfr)
}
