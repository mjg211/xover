.onAttach <- function(libname, pkgname) {
  packageStartupMessage(rep("-", 10), "\nxover: Design and analysis of ",
                        "cross-over clinical trials\n", rep("-", 10),
                        "\n\nv.0.1: For an overview of the package's ",
                        "functionality enter: ?singlearm\n\n",
                        "For news on the latest updates enter: ",
                        "news(package = \"xover\")")
}

# Required if using Rcpp
#.onUnload <- function (libpath) {
#  library.dynam.unload("xover", libpath)
#}
