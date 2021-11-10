# Like file.mtime, but faster
file_mtime <- function(filename) {
  if (length(filename) > 1) stop("file_mtime isn't vectorized")
  .Call(C_file_mtime, filename)
}