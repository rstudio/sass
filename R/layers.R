#' Sass layer objects
#'
#' Sass layers are a way to group a set of related Sass variable definitions,
#' function/mixin declarations, and CSS rules into a single object. Use
#' `sass_layer()` to create these objects, and `sass_layers()` to combine
#' two or more layer-like objects into a set of sass layers; this ability to be merged is
#' the main benefit of using Sass layers versus lower-level forms of sass input.
#' At a later time, layers that have been combined can also be removed
#' by referencing the same name that was used during combination.
#'
#' @md
#' @param ... A collection of [sass_layer()]s and/or objects that [as_sass()]
#'   understands. Arguments should be provided in reverse priority order:
#'   defaults, declarations, and rules in later layers will take precedence over
#'   those of previous layers. Non-layer values will be converted to layers by
#'   calling `sass_layer(rules = ...)`. Variable names for [as_sass_layers()] will be ignored.
#' @param defaults A suitable [sass::as_sass()] `input`. Intended for declaring
#'   variables with `!default`. When layers are combined, defaults are merged in
#'   reverse order; that is, `sass_layers(layer1, layer2)` will include
#'   `layer2$defaults` before `layer1$defaults`.
#' @param declarations A suitable [sass::as_sass()] `input`.  Intended for
#'   function and mixin declarations, and variable declarations without
#'   `!default`; not intended for actual CSS rules. These will be merged in
#'   forward order; that is, `sass_layers(layer1, layer2)` will include
#'   `layer1$declarations` before `layer2$declarations`.
#' @param rules A suitable [sass::as_sass()] `input`. Intended for actual CSS
#'   rules. These will be merged in forward order; that is,
#'   `sass_layers(layer1, layer2)` will include `layer1$rules` before
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
#' @seealso [sass_layer_remove_rule()]
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
#' sass(sass_layers(core, red_layer))
#' sass(sass_layers(core, red_layer, sass_layer(green)))
#'
#' # Example of merging layers and removing a layer
#' # Remember to name the layers that are removable
#' core_layers <- sass_layers(core, red = red_layer, green = sass_layer(green))
#' core_layers # pretty printed for console
#' core_slim <- sass_layers_remove(core_layers, "red")
#' sass(core_slim)
#'
#'
#' # File attachment example: Create a checkboard pattern .png, then
#' # use it from a sass layer
#'
#' tmp_png <- tempfile(fileext = ".png")
#' grDevices::png(filename = tmp_png, width = 20, height = 20,
#'   bg = "transparent", antialias = "none")
#' par(mar = rep_len(0,4), xaxs = "i", yaxs = "i")
#' plot.new()
#' rect(c(0,0.5), c(0,0.5), c(0.5,1), c(0.5,1), col = "#00000044", border=NA)
#' dev.off()
#'
#' layer <- sass_layer(
#'   rules = ".bg-check { background-image: url(images/demo_checkboard_bg.png) }",
#'   file_attachments = c("images/demo_checkboard_bg.png" = tmp_png)
#' )
#'
#' output_path <- tempfile(fileext = ".css")
#' sass(layer, output = output_path, write_attachments = TRUE)
#' @describeIn sass_layer Compose the parts of a single Sass layer
#' @export
sass_layer <- function(
  defaults = NULL,
  declarations = NULL,
  rules = NULL,
  html_deps = NULL,
  file_attachments = character(0),
  tags = character(0)
) {

  validate_attachments(file_attachments)

  if (inherits(html_deps, "html_dependency")) {
    html_deps <- list(html_deps)
  }
  if (!is.null(html_deps)) {
    is_dependency <- vapply(html_deps, inherits, logical(1), "html_dependency")
    if (any(!is_dependency)) stop("html_deps must be a collection of htmltools::htmlDependency() objects", call. = FALSE)
  }

  layer <- list(
    defaults =     as_sass_layer_list(defaults,     allow_removable = FALSE, name = "defaults"),
    declarations = as_sass_layer_list(declarations, allow_removable = FALSE, name = "declarations"),
    rules =        as_sass_layer_list(rules,        allow_removable = TRUE,  name = "rules"),
    html_deps = html_deps,
    file_attachments = file_attachments,
    tags = tags
  )
  add_class(layer, "sass_layer")
}

#' @describeIn sass_layer Turn an object into a Sass layers object. Non [sass_layer()] or Sass layers objects will be turned into `sass_layer(rules)`
#' @param x object to inspect or turn into Sass layers
#' @param name Sass layer name to use inside the Sass layers object
#' @export
as_sass_layers <- function(x, name = "") {
  stopifnot(is.character(name) && length(name) == 1)

  if (is_sass_layers(x)) {
    # if there is nothing special about the name, return
    if (identical(name, "")) {
      return(x)
    }

    # convert to a single sass layer so the overall name can be used
    x <- as_sass_layer(x)
  }

  if (is.list(x) && all(vapply(x, is_sass_layer, logical(1)))) {
    # list of sass_layer vals
    layers <- x
  } else {
    layers <- list()
    layers[[name]] <-
      if (is_sass_layer(x)) {
        x
      } else {
        sass_layer(rules = x)
      }
  }

  ret <- list(
    layers = layers
  )
  class(ret) <- "sass_layers"
  ret
}

#' @describeIn sass_layer Merge multiple sass layers or sass layer objects
#' @export
sass_layers <- function(...) {
  layers <- dropNulls(rlang::list2(...))
  layers_upgraded <-
    mapply(
      SIMPLIFY = FALSE,
      layers,
      rlang::names2(layers),
      FUN = function(x, name) {
        # Upgrade pattern:
        # is_sass_layers -> x
        # is_sass_layer  -> as_sass_layers(x, name)
        # else           -> as_sass_layers(sass_layer(rules = x), name)
        as_sass_layers(x, name = name)
      }
    )
  # collect and flatten
  ## unlist(list(list(1), list(), NULL, list(2)), recursive = FALSE)
  #> [[1]]
  #> [1] 1
  #>
  #> [[2]]
  #> [1] 2
  ret_layers <- unlist(
    lapply(unname(layers_upgraded), `[[`, "layers"),
    recursive = FALSE
  )
  as_sass_layers(ret_layers)
}


#' @describeIn sass_layer Remove a whole [sass_layer()] from a Sass layers object
#' @export
sass_layers_remove <- function(x, name) {
  stopifnot(is_sass_layers(x))

  layer_names <- names(x$layers)
  # vector support
  layer_name_matches <- layer_names %in% name
  if (any(layer_name_matches)) {
    name_pos <- which(layer_name_matches)
    x$layers <- x$layers[-1 * name_pos]
  }
  x
}




#' @describeIn sass_layer Check if `x` is a Sass layer object
#' @export
is_sass_layer <- function(x) {
  inherits(x, "sass_layer")
}



#' @describeIn sass_layer Check if `x` is a Sass layers object
#' @export
is_sass_layers <- function(x) {
  inherits(x, "sass_layers")
}


as_sass_layer_list <- function(x, allow_removable = FALSE, name = NULL) {
  if (inherits(x, "sass_removable")) {
    stop("A single removable layer item must be nested in a named list to be removable")
  }

  # make sure it is a list
  if (!is.null(x) && !is.list(x)) {
    x <- as.list(x)
  }

  # validate content
  if (is.list(x)) {
    x_names <- rlang::names2(x)
    item_is_removable <- vapply(x, function(y) { is_sass_removable(y) }, logical(1))
    if (isTRUE(allow_removable)) {
      # list names should not exist except when pointing to sass_removable items
      has_unexpected_name <- which(
        x_names != "" &
        !item_is_removable
      )
      if (any(has_unexpected_name)) {
        stop("sass_layer(", name, ") only allows list names that point to `sass_removable()` output. Unexpected names: ", paste0(x_names[has_unexpected_name], collapse = ", "))
      }
    } else {
      # variable names are allowed, no removable items allowed
      if (any(item_is_removable)) {
        stop("sass_layer(", name, ") does not allow for `sass_removable()` items. Found removable item names: ", paste0(x_names[item_is_removable], collapse = ", "))
      }
    }
  }

  ret <- add_class(x, "sass_layer_list")
  if (isTRUE(allow_removable)) {
    ret <- add_class(ret, "sass_layer_list_removable")
  }
  ret
}

#' Remove Sass layer rule
#'
#' @param layer Output from [sass_layer()]
#' @param rule Layer rule name to remove
#' @param x Value to add or check for a `"sass_removable"` class
#' @rdname sass_removable
#' @export
is_sass_removable <- function(x) {
  inherits(x, "sass_removable")
}
#' @rdname sass_removable
#' @export
sass_removable <- function(x) {
  add_class(x, "sass_removable")
}
#' @rdname sass_removable
#' @export
#' @examples
#' # set up a base blue color
#' blue <- list(color = "blue !default")
#'
#' # make a layer that has a custom red rule
#' core <- sass_layer(
#'   defaults = blue,
#'   rules = list(
#'     custom = sass_removable(
#'       # any R sass definitions can be nested under the `custom` key
#'       "body > custom { color: $color; }"
#'     ),
#'     "body { color: $color; }"
#'   )
#' )
#' # contains custom css
#' sass(core)
#'
#' core_slim <- sass_layer_remove_rule(core, "custom")
#' # does NOT contain custom css
#' sass(core_slim)
sass_layer_remove_rule <- function(layer, rule) {
  type <- "rules"
  key <- rule
  type_vals <- layer[[type]]
  if (! (key %in% names(type_vals))) {
    # key not found
    return(layer)
  }
  key_val <- type_vals[[key]]
  if (is.null(key_val)) {
    # remove the key/NULL pair
    type_vals[[key]] <- NULL
  } else {
    if (is_sass_removable(key_val)) {
      # remove the removable key/val pair
      type_vals[[key]] <- NULL
    } else {
      stop("Not allowed to remove ", type, " key: '", key, "'")
    }
  }
  layer[[type]] <- type_vals
  layer
}


# Used in `as_sass.sass_layers`
as_sass_layer <- function(x) {
  if (is_sass_layer(x)) return(x)
  Reduce(sass_layers_join, sass_layers(x)$layers)
}
sass_layers_join <- function(layer1, layer2) {
  sass_layer(
    defaults = c(layer2$defaults, layer1$defaults),
    declarations = c(layer1$declarations, layer2$declarations),
    rules = c(layer1$rules, layer2$rules),
    html_deps = c(layer1$html_deps, layer2$html_deps),
    file_attachments = join_attachments(layer1$file_attachments, layer2$file_attachments),
    tags = c(layer1$tags, layer2$tags)
  )
}
# attach2 takes precedence
join_attachments <- function(attach1, attach2) {
  # I thought about removing duplicates here, but it's hard to do so reliably
  # because the paths can be files or directories.
  c(attach1, attach2)
}


# Given the `input` to `sass()`, returns either NULL or a single sass_layer
# that merges any sass_layers found in the input
# returns a single `sass_layer()` / `NULL`
extract_layer <- function(input) {
  if (is_sass_layer(input)) {
    return(input)
  }
  if (is_sass_layers(input)) {
    return(as_sass_layer(input))
  }
  if (!identical(class(input), "list")) {
    return(NULL)
  }

  layers <- lapply(input, function(x) extract_layer(x))
  layers <- dropNulls(layers)
  # convert to a sass layer object
  as_sass_layer(
    # merge all sass layers
    sass_layers(!!!layers)
  )
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
  src <- unname(attachments)

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


#' Write file attachments from a sass theme object
#'
#' @param file_attachments A character vector of files or directories.
#' @param output_path A directory to copy the attachments to.
#'
#' @keywords internal
#' @export
write_file_attachments <- function(file_attachments, output_path) {
  validate_attachments(file_attachments)

  if (!dir.exists(output_path)) {
    stop(call. = FALSE,
      "Directory '", output_path, "' not found or is not a directory")
  }

  output_path <- normalizePath(output_path, mustWork = TRUE)

  mapply(function(dest, src) {
    if (dir.exists(src)) {
      dest <- file.path(output_path, dest)
      if (!dir.exists(dest)) {
        dir.create2(dest)
      }
      # We previously used fs::dir_copy(), but changed to file.copy2 for
      # performance reasons. https://github.com/rstudio/sass/pull/53
      file.copy2(
        dir(src, all.files = TRUE, full.names = TRUE, no.. = TRUE),
        dest,
        overwrite = TRUE,
        recursive = TRUE
      )
      return(NULL)
    }

    dest <- if (grepl("/$", dest)) {
      # dest is intended to be a directory
      file.path(output_path, dest, basename(src))
    } else {
      file.path(output_path, dest)
    }

    fs::dir_create(dirname(dest))
    fs::file_copy(src, dest, overwrite = TRUE)

    NULL
  }, names(file_attachments), unname(file_attachments))

  invisible()
}


dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}
