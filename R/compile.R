
#' @useDynLib sass, .registration = TRUE
compile_file <- function(file, opts) {
  .Call(C_compile_file, file, opts)
}

compile_data <- function(data, opts) {
  .Call(C_compile_data, data, opts)
}

.onUnload <- function (libpath) {
  library.dynam.unload("sass", libpath)
}
