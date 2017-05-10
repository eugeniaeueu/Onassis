#' @importFrom rJava .jpackage
.onLoad <- function(libname, pkgname) {
    .jpackage(pkgname, lib.loc = libname)
    tools::vignetteEngine("knitr", pattern = "[.]Rmd$", package = "knitr")
    # .jinit(force.init=TRUE)
    initJavaLibs()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("OnASSIs successfully loaded")
}
