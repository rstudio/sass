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
    mtime = all_info$mtime,
    stringsAsFactors = FALSE
  )
}

# Checks whether a package is installed
is_installed <- function(package) {
  nzchar(system.file(package = package))
}


# Returns TRUE if this is called while a Shiny app is running; FALSE otherwise.
is_shiny_app <- function() {
  "shiny" %in% loadedNamespaces() && shiny::isRunning()
}

# Is this app hosted? Returns TRUE for both Shiny Server and RStudio Connect.
is_hosted_app <- function() {
  nzchar(Sys.getenv("SHINY_SERVER_VERSION")) && is_shiny_app()
}

# Is this app running on Shiny Server (and not RStudio Connect)?
is_shiny_server_app <- function() {
  is_hosted_app() && !is_connect_app()
}

# Is this app running on RStudio Connect (and not Shiny Server)?
is_connect_app <- function() {
  if (!is_hosted_app()) {
    return(FALSE)
  }

  info <- shiny::serverInfo()
  if (!is.null(info) && is.list(info) && identical(info$edition, "Connect")) {
    return(TRUE)
  }

  FALSE
}

# Wrapper for file.copy that throws if any files fail to copy. Use name
# file.copy2 to make clear that it's a wrapper for a base function, and not for
# fs::file_copy.
file.copy2 <- function(from, to, ...) {
  res <- file.copy(from, to, ...)
  if (!all(res)) {
    stop("Error copying files: ", paste0(from[!res], collapse = ", "))
  }
}

dir.create2 <- function(path, ...) {
  res <- dir.create(path, ...)
  if (!res) {
    stop("Error creating directory: ", path)
  }
}

# adds a class to the object (if possible) (if not already done)
add_class <- function(x, class_val) {
  if (is.null(x)) return(x)
  if (inherits(x, class_val)) return(x)

  class(x) <- c(class_val, class(x))
  x
}

collapse0 <- function(..., collapse = "\n") {
  paste0(..., collapse = collapse)
}

# checks for any valid, non-"" keys in the list x
#' @importFrom  rlang have_name
has_any_name <- function(x) {
  any(have_name(x))
}

is_string <- function(x) {
  is.character(x) && length(x) == 1
}

trim_ws <- function(x) {
  sub("^\\s*", "", sub("\\s*$", "", x))
}

is_available <- function(package, version = NULL) {
  installed <- nzchar(system.file(package = package))
  if (is.null(version)) {
    return(installed)
  }
  installed && isTRUE(utils::packageVersion(package) >= version)
}

has_any_name_recursive <- function(x) {
  if (has_any_name(x)) {
    # if this level has a name, return true
    return(TRUE)
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  # Recursively inspect list objects
  # Use for loop to pre-empty calculations
  for (item in x) {
    if (has_any_name_recursive(item)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
