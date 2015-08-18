## Wrapper functions to retrieve intrinsic and breast cancer subtypes from BHK's genefu
## Note that expression matrix is expected to have Entrez Gene IDs as rownames
##
## * getIntrinsicPreds
getIntrinsicPreds <- function(sbtModel, e.mtx, a.dfr, doMp) {
    vv <- intrinsic.cluster.predict(sbtModel,
                                    data = t(e.mtx),
                                    annot = a.dfr,
                                    do.mapping = doMp)$subtype
    ordered(vv,
            levels = c("Basal",
                       "Her2",
                       "LumA",
                       "LumB",
                       "Normal"))
}

## * getSubtypePreds
getSubtypePreds <- function(sbtModel, e.mtx, a.dfr, doMp) {
    vv <- subtype.cluster.predict(sbtModel,
                                  data = t(e.mtx),
                                  annot = a.dfr,
                                  do.mapping = doMp)$subtype
    ordered(vv,
            levels = c("ER-/HER2-",
                       "ER+/HER2-",
                       "HER2+"))
}

## * getGenefuPreds
getGenefuPreds <- function (expr.mtx, # gene expression matrix, genes in rows,
                                        # samples in columns
                            annot.dfr, # annotation data frame, rownames must be the
                                        # same as exprs.mtx; must contain 'probe' and
                                        # 'EntrezGene.ID' columns
                            doMapping = FALSE) { # TRUE if Agilent; FALSE if Affy
    rqrdLibs <- c("parallel", "genefu")
    sapply(rqrdLibs, function(lib) {
        if(sum(grepl(lib, search())) == 0) require(lib, character.only = TRUE)
    })
    out.lst <- list()
    out.lst$intrinsic <- setNames(mclapply(list(ssp2003, ssp2006, pam50),
                                           getIntrinsicPreds,
                                           e.mtx = expr.mtx,
                                           a.dfr = annot.dfr,
                                           doMp = doMapping,
                                           mc.cores = 3),
                                  c("sorlie2003", "hu2006", "pam50"))
    out.lst$subtype <- setNames(mclapply(list(scmod1.robust, scmod2.robust),
                                         getSubtypePreds,
                                         e.mtx = expr.mtx,
                                         a.dfr = annot.dfr,
                                         doMp = doMapping,
                                         mc.cores = 2),
                                c("desmedt2008", "wiripati2008"))
    do.call(cbind, lapply(out.lst, as.data.frame))
}
