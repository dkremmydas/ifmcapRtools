# Initialization hooks for package load/attach

.onLoad <- function(libname, pkgname) {
  # Place initialization code here if needed (register S3 methods, set options quietly, etc.)
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("CONFIG"))
    utils::globalVariables(c("."))
    utils::globalVariables(c(":="))
  }
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Use for startup messages if desired:
   packageStartupMessage("ifmcapRtools loaded - ready to utilize the power of R to work with ifmcap data.")

  #TODO Check if we can find the gams installation, and if not abort and provide instructions on how to initialize
}
