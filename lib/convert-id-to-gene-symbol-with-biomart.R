## 16Apr2015
##
## id2GeneSymbolBM
##
## Provided with a character vector of genomic identifiers; and a biomaRt
## hsapiens_gene_ensembl specific set of filters and attributes; this function
## returns a data frame with the corresponding mappings of the given identifiers
## to each of specified attributes.
## The 'filter' argument defaults to ensembl gene id, and the mapped
## 'attributes' argument defaults to hgnc symbol.

id2GeneSymbolBM <- function(ids,
                            filters = "ensembl_gene_id",
                            attributes = "hgnc_symbol"){
    library(biomaRt)
    ensembl <- useMart("ensembl", dataset = "hsapiens_gene_ensembl")
    getBM(attributes = c(filters, attributes),
          filters = filters,
          values = ids,
          mart = ensembl)
}
