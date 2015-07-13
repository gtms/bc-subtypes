## 26Jun2015
## Installs R packages for analysis

## * Preamble
## need to update variable .libPath, as per instructions found in
## https://support.bioconductor.org/p/42547/
pkgDir <- ".R-libraries"

## * Functions
## ** installPackage
installPackage <- function(pkg, src) {
    if(src == "cran") {
        install.packages(pkg,
                         repos = "http://cran.r-project.org",
                         lib = pkgDir,
                         destdir = file.path(pkgDir, "tmp"))
    } else {
        .libPaths(c(pkgDir, .libPaths()))
        source("http://bioconductor.org/biocLite.R")
        biocLite(pkg,
                 suppressUpdates = FALSE,
                 suppressAutoUpdate = FALSE,
                 lib.loc = pkgDir,
                 lib = pkgDir,
                 destdir = file.path(pkgDir, "tmp"),
                 ask = FALSE)
    }
}

## * Load
## ** packages listed to be installed on data/csv/packages.csv
pkgs2install.dfr <- read.csv("data/csv/packages.csv",
                             stringsAsFactors = FALSE)

## * Run
## ** listed packages
with(pkgs2install.dfr, mapply(installPackage,
                              pkg = package,
                              src = source))

## ** brain array cdf packages manually downloaded to data/cdf
cdfPkgs <- list.files("data/cdf", pattern = "tar.gz")

sapply(file.path("data/cdf", cdfPkgs), install.packages,
       repos = NULL,
       lib = pkgDir,
       type = "source")

## * Exit
sessionInfo()
q(save = "no")
