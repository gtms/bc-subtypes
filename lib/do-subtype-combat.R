## Given a concatenated expression matrix (with genes as rows and samples as
## columns) of at least two datasets; a factor describing each dataset; and a
## factor of clinical subtypes as derived by an external classifier, returns an
## integrated matrix of expression, the result of stacking ComBat outputs for
## the integration of all datasets split by clinical subtypes.

## * doSubtypeCombat
doSubtypeCombat <- function(mtx, # concatenated expression matrix of at least
                                        # two datasets
                            d.fac, # factor of length ncol(mtx) describing
                                        # dataset batches
                            subtypePreds.fac,# factor of subtype predictions,
                                        # of length ncol(mtx)
                            nMin = 3) { # minimum number of samples required per
                                        # subtype-specific batch
    sampleInfo.dtb <- data.table(sn = colnames(mtx),
                                 b = d.fac,
                                 st = subtypePreds.fac)
    ## Some requirements have to be met for ComBat to run.  Each
    ## subtype-specific batch must have at least 2 samples (although we require
    ## nMin = 3 to be more stringent) to draw summary statistics from: remove
    ## any sample that contibutes to violate this condition.  Each subtype must
    ## encompass at least two batches, i.e., no given subtype can only
    ## integrate samples from one batch: remove all samples that contribute to
    ## violate this condition.
    lst <- tapply(1:ncol(mtx),
                  subtypePreds.fac, function(x) {
                      list(d = mtx[, x],
                           b = d.fac[x])
                  })
    nSamples.lst <- lapply(lst, function(l) table(l$b))
    samples2rm.lst <- lapply(nSamples.lst, function(sbt) {
        res <- sbt < nMin
        if(sum(!res) < 2) {
            return(setNames(rep(TRUE, length(sbt)),
                            names(sbt)))
        } else {
            return(res)
        }
    })
    ## define index of samples that meet required conditions
    idx2kp <- mapply(function(st, b) !samples2rm.lst[[st]][[b]],
                     st = sampleInfo.dtb[["st"]],
                     b = sampleInfo.dtb[["b"]])
    ## redefine list of subtype-specific batches only with samples that meet
    ## required conditions
    rdx.lst <- tapply(1:ncol(mtx[, idx2kp]),
                      subtypePreds.fac[idx2kp], function(x) {
                          list(d = mtx[, idx2kp][, x],
                               b = droplevels(d.fac[idx2kp][x]))
                      })
    ## remove potential dropped out subtype-specific batches
    rdx.lst <- rdx.lst[!sapply(rdx.lst, is.null)]
    ## perform subtype combat
    cbt.mtx <- do.call(cbind, lapply(rdx.lst, function(sbt) {
        ComBat(dat = sbt$d,
               batch = sbt$b)
    }))
    ## return integrated matrix, with removed samples as NA
    out.mtx <- matrix(rep(NA, nrow(mtx) * ncol(mtx)),
                      ncol = ncol(mtx),
                      dimnames = list(rownames(mtx),
                                      colnames(mtx)))
    for(clnm in colnames(cbt.mtx)) {
        out.mtx[, clnm] <- cbt.mtx[, clnm]
    }
    out.mtx
}
