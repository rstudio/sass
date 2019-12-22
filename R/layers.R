#' Place SASS before and after other SASS
#'
#' [sass_layer()] defines [sass::sass()] input(s) to place before and after
#' existing SASS object(s). To actually surround existing `sass` with a
#' [sass_layer()], use `sass_layer_merge(sass, sass_layer())`.
#'
#' @md
#' @param ... A collection of [sass_layer()]s and/or objects that [as_sass()] understands.
#' @param defaults A suitable [sass::as_sass()] `input`. Intended for declaring
#'   variables with `!default`. When layers are combined, defaults are merged in
#'   reverse order; that is, `sass_layer_merge(layer1, layer2)` will include
#'   `layer2$defaults` before `layer1$defaults`.
#' @param declarations A suitable [sass::as_sass()] `input`.  Intended for
#'   function and mixin declarations, and variable declarations without
#'   `!default`; not intended for actual CSS rules. These will be merged in
#'   forward order; that is, `sass_layer_merge(layer1, layer2)` will include
#'   `layer1$declarations` before `layer2$declarations`.
#' @param rules A suitable [sass::as_sass()] `input`. Intended for actual CSS
#'   rules. These will be merged in forward order; that is,
#'   `sass_layer_merge(layer1, layer2)` will include `layer1$rules` before
#'   `layer2$rules`.
#' @param html_deps An HTML dependency (or a list of them).
#' @param file_attachments A named character vector, representing file assets
#'   that are referenced (using relative paths) from the sass in this layer. The
#'   vector names should be a relative path, and the corresponding vector values
#'   should be absolute paths to files or directories that exist; at render
#'   time, each value will be copied to the relative path indicated by its name.
#'   (For directories, the _contents_ of the source directory will be copied
#'   into the destination directory; the directory itself will not be copied.)
#'   You can also omit the name, in which case that file or directory will be
#'   copied directly into the output directory.
#' @param tags A character vector with zero or more elements. Can be used to
#'   preserve simple metadata as layers are merged.
#' @export
#' @examples
#' blue <- list(color = "blue !default")
#' red <- list(color = "red !default")
#' green <- list(color = "green !default")
#'
#' # a sass_layer() by itself is not very useful, it just defines some
#' # SASS to place before (defaults) and after (declarations, rules)
#' core <- sass_layer(defaults = blue, rules = "body { color: $color; }")
#' core
#' sass(core)
#'
#' # However, by stacking sass_layer()s, we have ability to place
#' # SASS both before and after some other sass (e.g., core)
#' # Here we place a red default _before_ the blue default and export the
#' # color SASS variable as a CSS variable _after_ the core
#' red_layer <- sass_layer(red, rules = ":root{ --color: #{$color}; }")
#' sass(sass_layer_merge(core, red_layer))
#' sass(sass_layer_merge(core, red_layer, sass_layer(green)))
#'
sass_layer_merge <- function(...) {
  layers <- dropNulls(rlang::list2(...))
  is_layer <- vapply(layers, is_sass_layer, logical(1))
  layers[!is_layer] <- lapply(layers[!is_layer], function(x) {
    sass_layer(rules = x)
  })
  Reduce(sass_layers_join, layers)
}

#' @rdname sass_layer_merge
#' @export
sass_layer <- function(defaults = "", declarations = "", rules = "",
  html_deps = NULL, file_attachments = character(0), tags = character(0)) {

  validate_attachments(file_attachments)

  if (inherits(html_deps, "html_dependency")) {
    html_deps <- list(html_deps)
  }
  if (!is.null(html_deps)) {
    is_dependency <- vapply(html_deps, inherits, logical(1), "html_dependency")
    if (any(!is_dependency)) stop("html_deps must be a collection of htmltools::htmlDependency() objects", call. = FALSE)
  }

  layer <- list(
    defaults = as_sass(defaults),
    declarations = as_sass(declarations),
    rules = as_sass(rules),
    html_deps = html_deps,
    file_attachments = file_attachments,
    tags = tags
  )
  structure(layer, class = "sass_layer")
}

sass_layers_join <- function(layer1, layer2) {
  sass_layer(
    defaults = as_sass(list(layer2$defaults, layer1$defaults)),
    declarations = as_sass(list(layer1$declarations, layer2$declarations)),
    rules = as_sass(list(layer1$rules, layer2$rules)),
    html_deps = c(layer1$html_deps, layer2$html_deps),
    file_attachments = join_attachments(layer1$file_attachments, layer2$file_attachments),
    tags = c(layer1$tags, layer2$tags)
  )
}

# Given the `input` to `sass()`, returns either NULL or a single sass_layer
# that merges any sass_layers found in the input
extract_layer <- function(input) {
  if (inherits(input, "sass_layer")) {
    return(input)
  }
  if (!identical(class(input), "list")) {
    return(NULL)
  }

  layers <- lapply(input, extract_layer)
  layers <- dropNulls(layers)
  sass_layer_merge(!!!layers)
}

validate_attachments <- function(attachments) {
  if (is.null(attachments)) {
    return()
  }
  if (!is.character(attachments)) {
    stop("File attachments must be a character vector")
  }
  if (length(attachments) == 0) {
    return()
  }

  dest <- names(attachments)
  if (is.null(dest)) {
    dest <- rep_len("", length(attachments))
  }
  src <- attachments

  if (any(dest == "")) {
    # Actually, unnamed attachments are OK; they'll just be
    # copied to the output dir.
    # stop("All file attachments must be named")
  }

  check_files <- function(files, ok, message) {
    if (any(!ok)) {
      stop(call. = FALSE, message, ": ",
        paste0("'", files[!ok], "'", collapse = ", "))
    }
  }

  check_files(dest, !fs::is_absolute_path(dest),
    "File attachment names must be relative (destination) paths")

  check_files(src, fs::is_absolute_path(src),
    "File attachment names must be absolute (source) paths")

  check_files(src, fs::file_exists(src),
    "File attachments must exist")

  check_files(dest, vapply(dest, fs::path_has_parent, logical(1), parent = "."),
    "Illegal file attachment destination path(s)")
}

# attach2 takes precedence
join_attachments <- function(attach1, attach2) {
  # I thought about removing duplicates here, but it's hard to do so reliably
  # because the paths can be files or directories.
  c(attach1, attach2)
}

write_file_attachments <- function(file_attachments, output_path) {
  validate_attachments(file_attachments)

  if (!dir.exists(output_path)) {
    stop(call. = FALSE,
      "Directory '", output_path, "' not found or is not a directory")
  }

  output_path <- normalizePath(output_path, mustWork = TRUE)

  mapply(function(dest, src) {
    # Don't use fs::path_join here, it drops trailing slashes
    dest <- file.path(output_path, dest)

    if (fs::is_dir(src)) {
      fs::dir_copy(src, dest, overwrite = TRUE)
    } else {
      if (grepl("/$", dest)) {
        # dest is intended to be a directory
        dest <- file.path(dest, basename(src))
      }
      fs::dir_create(dirname(dest))
      fs::file_copy(src, dest, overwrite = TRUE)
    }
    NULL
  }, names(file_attachments), unname(file_attachments))

  invisible()
}

is_sass_layer <- function(x) {
  inherits(x, "sass_layer")
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}
