write_utf8 <- function(text, ...) {
  writeBin(charToRaw(enc2utf8(text)), ...)
}

raw_to_utf8 <- function(data) {
  res <- rawToChar(data)
  Encoding(res) <- "UTF-8"
  res
}

read_raw <- function(file) {
  # If the file doesn't exist, readBin() will throw a warning and an error. We
  # really only want it to throw the error (when used in DiskCache), so suppress
  # the warning. Unfortunately, the error has litle useful information; it only
  # says 'Error in file(con, "rb") : cannot open the connection'. The warning
  # contains the information about wht the actual problem is, as in 'cannot open
  # file 'asdf': No such file or directory'. For the DiskCache use case, it's OK
  # to throw away that information, but for general use, it would be better to
  # use brio::read_file_raw(), because it only throws an error (no warning), and
  # the error is informative.
  suppressWarnings(
    readBin(file, "raw", n = file.size(file))
  )
}

read_utf8 <- function(file) {
  res <- read_raw(file)
  raw_to_utf8(res)
}
