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
  # really only want it to throw the error (when used in FileCache), so suppress
  # the warning. Unfortunately, the error has litle useful information; it only
  # says 'Error in file(con, "rb") : cannot open the connection'. The warning
  # contains the information about wht the actual problem is, as in 'cannot open
  # file 'asdf': No such file or directory'. For the FileCache use case, it's OK
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


# `file` must be a character vector with files and directories. This will return
# a data frame with the mtimes of all files that are passed in, as well as the
# mtimes of files in the directories that are passed in. The data frame will not
# contain the mtimes of the directories themselves. Also, any files that were
# passed in but don't exist will not be present in the returned data frame.
get_file_mtimes <- function(files) {
  info <- file.info(files, extra_cols = FALSE)

  dirs <- files[info$isdir & !is.na(info$isdir)]
  files_in_dirs <- dir(dirs, full.names = TRUE, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  files_in_dirs_info <- file.info(files_in_dirs, extra_cols = FALSE)

  all_info <- rbind(
    # The (non-dir) files that were passed in directly
    info[!info$isdir & !is.na(info$isdir), , drop = FALSE],
    files_in_dirs_info
  )

  data.frame(
    file = rownames(all_info),
    mtime = all_info$mtime
  )
}
