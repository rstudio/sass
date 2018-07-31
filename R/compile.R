#' @export
#' @useDynLib sass compile_
compile <- function(file_path) .Call(compile_, file_path)

.onUnload <- function (libpath) {
  library.dynam.unload("sass", libpath)
}
