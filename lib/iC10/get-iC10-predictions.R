## Wrapper function to compute iC10 predictions

## * getIC10Preds
getIC10Preds <- function(mtx) { # mtx: matrix with unique hgnc gene symbols as
                                        # rows and samples as columns
    features <- matchFeatures(Exp = mtx,
                              Exp.by.feat = "gene")
    features <- normalizeFeatures(features, "scale")
    iC10(features,
         seed = 777)$class
}
