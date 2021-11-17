#' @importFrom rlang list2 names2 %||%
NULL

#' Bundling Sass layers
#'
#' Sass layers provide a way to package Sass variables, rules, functions, and
#' mixins in a structured and composable way that follows best Sass practices.
#' Most importantly, when multiple `sass_layer()` are combined into a
#' `sass_bundle()`, variable `defaults` for later layers are placed _before_
#' earlier layers, effectively 'new' defaults through all the 'old' defaults.
#'
#' @param functions [sass::as_sass()] `input` intended for [Sass
#'   functions](https://rstudio.github.io/sass/articles/sass.html#functions-1).
#'   Functions are placed before `defaults` so that variable definitions may make
#'   use of functions.
#' @param defaults [sass::as_sass()] `input` intended for [variable
#'   defaults](https://rstudio.github.io/sass/articles/sass.html#variables-1).
#'   These variable defaults after placed after `functions` but before `mixins`.
#'   When multiple layers are combined in a `sass_bundle()`, defaults are merged
#'   in reverse order; that is, `sass_bundle(layer1, layer2)` will include
#'   `layer2$defaults` before `layer1$defaults`.
#' @param mixins [sass::as_sass()] `input` intended for [Sass
#'   mixins](https://rstudio.github.io/sass/articles/sass.html#mixins-1). Mixins
#'   are placed after `defaults`, but before `rules`.
#' @param rules [sass::as_sass()] `input` intended for [Sass
#'   rules](https://sass-lang.com/documentation/style-rules). Rules are placed last
#'   (i.e., after `functions`, `defaults`, and `mixins`).
#' @param html_deps An HTML dependency (or a list of them). This dependency
#'   gets attached to the return value of [sass()]/[as_sass()].
#' @param file_attachments A named character vector, representing file assets
#'   that are referenced (using relative paths) from the sass in this layer. The
#'   vector names should be a relative path, and the corresponding vector values
#'   should be absolute paths to files or directories that exist; at render
#'   time, each value will be copied to the relative path indicated by its name.
#'   (For directories, the _contents_ of the source directory will be copied
#'   into the destination directory; the directory itself will not be copied.)
#'   You can also omit the name, in which case that file or directory will be
#'   copied directly into the output directory.
#' @param declarations Deprecated, use `functions` or `mixins` instead.
#' @param tags Deprecated. Preserve meta information using a key in `sass_bundle(KEY = val)`.
#'   preserve simple metadata as layers are merged.
#' @md
#' @examples
#' blue <- list(color = "blue !default")
#' red <- list(color = "red !default")
#' green <- list(color = "green !default")
#'
#' # a sass_layer() by itself is not very useful, it just defines some
#' # SASS to place before (defaults) and after (rules)
#' core <- sass_layer(defaults = blue, rules = "body { color: $color; }")
#' core
#' sass(core)
#'
#' # However, by stacking sass_layer()s, we have ability to place
#' # SASS both before and after some other sass (e.g., core)
#' # Here we place a red default _before_ the blue default and export the
#' # color SASS variable as a CSS variable _after_ the core
#' red_layer <- sass_layer(red, rules = ":root{ --color: #{$color}; }")
#' sass(sass_bundle(core, red_layer))
#' sass(sass_bundle(core, red_layer, sass_layer(green)))
#'
#' # Example of merging layers and removing a layer
#' # Remember to name the layers that are removable
#' core_layers <- sass_bundle(core, red = red_layer, green = sass_layer(green))
#' core_layers # pretty printed for console
#' core_slim <- sass_bundle_remove(core_layers, "red")
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
#' @describeIn sass_layer Compose the parts of a single Sass layer. Object returned is a `sass_bundle()` with a single Sass layer
#' @export
sass_layer <- function(
  functions = NULL,
  defaults = NULL,
  mixins = NULL,
  rules = NULL,
  html_deps = NULL,
  file_attachments = character(0),
  declarations = NULL,
  tags = NULL
) {

  # In a future release cycle (once bslib has been updated to not use
  # declarations), then consider throwing this deprecation message
  #if (!is.null(declarations)) {
  #  .Deprecated(msg="`declarations` is deprecated. Please use `functions` or `mixins instead.`")
  #}

  if (!missing(tags)) {
    .Deprecated(msg="`sass_layer(tags)` is deprecated. Please use a named layer in a `sass_bundle(NAME = layer)`")
  }

  # return a size 1 sass_bundle()
  as_sass_bundle(
    sass_layer_struct(
      functions = functions,
      defaults = defaults,
      mixins = mixins,
      rules = rules,
      declarations = declarations,
      html_deps = html_deps,
      file_attachments = file_attachments
    )
  )
}


#' @export
#' @param file file path to a `.scss` file.
#' @describeIn sass_layer Read in a `.scss` file with parse special `/*-- scss:(functions|defaults|rules|mixins) --*/` comments as relevant sections of a `sass_layer()`.
sass_layer_file <- function(file) {
  src <- readLines(file)
  # https://github.com/quarto-dev/quarto-cli/blob/3d6063/src/command/render/sass.ts#L119-L123
  pattern_key <- "scss:(functions|defaults|rules|mixins)"
  pattern <- paste0("^/\\*--[ \\t]*", pattern_key, "[ \\t]*--\\*/$")
  idx <- grep(pattern, src)
  if (length(idx) == 0) {
    return(sass_layer(rules = src))
  }
  types <- extract_group(src[idx], pattern_key)
  utypes <- unique(types)
  args <- rlang::set_names(vector("list", length(utypes)), utypes)
  for (i in seq_along(idx)) {
    start <- idx[i] + 1
    end <- if (i == length(idx)) length(src) else (idx[i + 1] - 1)
    type <- types[i]
    args[[type]] <- c(args[[type]], src[seq.int(start, end)])
  }
  do.call(sass_layer, args)
}

#' Helps avoid sass_layer / sass_bundle inf recursion
#' @return object of class `sass_layer`
#' @noRd
sass_layer_struct <- function(
  functions = NULL,
  defaults = NULL,
  mixins = NULL,
  rules = NULL,
  declarations = NULL,
  html_deps = NULL,
  file_attachments = character(0),
  validate = TRUE
) {

  if (validate) {
    validate_layer_param(functions, "functions")
    validate_layer_param(defaults, "defaults")
    validate_layer_param(mixins, "mixins")
    validate_layer_param(rules, "rules")
    validate_layer_param(declarations, "declarations")

    if (!is.null(html_deps)) {
      if (is_dependency_maybe(html_deps)) {
        html_deps <- list(html_deps)
      }
      if (!is.list(html_deps)) {
        stop("`html_deps` must be a collection of htmlDependency() and/or tagFunction() objects")
      }
      is_dependency <- vapply(html_deps, is_dependency_maybe, logical(1))
      if (any(!is_dependency)) {
        stop("`html_deps` must be a collection of htmlDependency() and/or tagFunction() objects")
      }
    }
  }

  layer <- list(
    functions = functions,
    defaults = defaults,
    mixins = mixins,
    rules = rules,
    declarations = declarations,
    html_deps = html_deps,
    file_attachments = file_attachments
  )
  add_class(layer, "sass_layer")
}

is_dependency_maybe <- function(x) {
  inherits(x, "html_dependency") || inherits(x, "shiny.tag.function")
}

validate_layer_param <- function(x, name) {
  bundle_item <- find_bundle_or_layer(x)
  if (!is.null(bundle_item)) {
    stop(
      "`sass_layer(", name, ")` can not contain another `sass_bundle()` object.\n",
      "Found:\n",
      collapse0(utils::capture.output(print(bundle_item)))
    )
  }
}
is_sass_bundle_like <- function(x) {
  is_sass_bundle(x) || is_sass_layer(x)
}

# returns the sass bundle like obj or NULL
find_bundle_or_layer <- function(x) {
  if (!is.list(x)) return(NULL)
  if (is_sass_bundle_like(x)) return(x)

  # Recursively inspect list objects
  # Use for loop to pre-empty calculations
  for (item in x) {
    if (is_sass_bundle_like(item)) return(item)
    ret <- find_bundle_or_layer(item)
    if (!is.null(ret)) return(ret)
  }
  return(NULL)
}


# @param x object to inspect or turn into Sass bundle
# @param name Sass layer name to use inside the Sass bundle object
as_sass_bundle <- function(x, name = "") {
  stopifnot(is.character(name) && length(name) == 1)

  # Upgrade pattern:
  # is_sass_bundle(x) && name == "" -> x
  # is_sass_bundle(x) && name != "" -> sass_bundle(!!name := as_sass_layer(x))
  # is_sass_layer(x)                -> sass_bundle(!!name := x)
  # is.list(x) && has_any_name(x)   -> sass_bundle(!!name := sass_layer(defaults = x))
  # else                            -> sass_bundle(!!name := sass_layer(rules = x))

  # A list of sass_layer values can be handled by sass_bundle(...) or sass_bundle(!!!x). Do not test for this

  if (is_sass_bundle(x)) {
    # if there is nothing special about the name, return
    if (identical(name, "")) {
      return(x)
    }

    # convert to a single sass layer object so the overall name can be used
    x <- as_sass_layer(x)
  }

  single_layer <-
    if (is_sass_layer(x)) {
      x
    } else {
      # upgrade via sass_layer
      if (is.list(x) && has_any_name_recursive(x)) {
        # a name was found somewhere
        sass_layer_struct(defaults = x)
      } else {
        sass_layer_struct(rules = x)
      }
    }

  layers <- list()
  layers[[name]] <- single_layer

  sass_bundle_struct(layers = layers)
}

#' Helps avoid sass_bundle inf recursion
#' @param layers named or unnamed list containing sass_layer objects
#' @return object of class `sass_bundle`
#' @noRd
sass_bundle_struct <- function(layers = list()) {
  layers_are_sass_layers <- vapply(layers, is_sass_layer, logical(1))
  stopifnot(all(layers_are_sass_layers))
  ret <- list(
    layers = layers
  )
  class(ret) <- "sass_bundle"
  ret
}

#' @describeIn sass_layer Collect `sass_bundle()` and/or `sass_layer()` objects. Unnamed Sass bundles will be concatenated together, preserving their internal name structures. Named Sass bundles will be condensed into a single Sass layer for easier removal from the returned Sass bundle.
#' @param ... A collection of `sass_layer()`s and/or objects that [sass::as_sass()]
#'   understands. Arguments should be provided in reverse priority order:
#'   defaults, declarations, and rules in later layers will take precedence over
#'   those of previous layers. Non-layer values will be converted to layers by
#'   calling `sass_layer(rules = ...)`.
#' @export
sass_bundle <- function(...) {
  layers <- dropNulls(list2(...))
  layers_upgraded <-
    mapply(
      SIMPLIFY = FALSE,
      layers,
      names2(layers),
      FUN = function(x, name) {
        as_sass_bundle(x, name = name)
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
  sass_bundle_struct(ret_layers)
}


#' @describeIn sass_layer Remove a whole `sass_layer()` from a `sass_bundle()` object.
#' @param bundle Output value from `sass_layer()` or `sass_bundle()`
#' @param name If a Sass layer name is contained in `name`, the matching Sass layer will be removed from the `bundle`
#' @export
sass_bundle_remove <- function(bundle, name) {
  stopifnot(is_sass_bundle(bundle))
  if (!(
    is.character(name) &&
    all(!is.na(name)) &&
    all(nzchar(name))
  )) {
    stop("`name` needs to be a character vector containing non-NA and non-empty values")
  }

  layer_names <- names(bundle$layers)
  # vector support
  layer_name_matches <- layer_names %in% name
  if (any(layer_name_matches)) {
    name_pos <- which(layer_name_matches)
    bundle$layers <- bundle$layers[-1 * name_pos]
  }
  bundle
}


# sass_layer Check if `x` is a Sass layer object
is_sass_layer <- function(x) {
  inherits(x, "sass_layer")
}

#' @describeIn sass_layer Check if `x` is a Sass bundle object
#' @param x object to inspect
#' @export
is_sass_bundle <- function(x) {
  inherits(x, "sass_bundle")
}


#' Sass Bundle to Single Sass Layer
#'
#' Converts a [sass_bundle()] to a single Sass layer object.
#'
#' This is exported for internal use between packages and should not be used.
#' Instead, please use [sass_layer()] or [sass_bundle()] to construct and manage your sass objects
#' and [sass()] and [as_sass()] to convert your objects.
#'
#' @keywords internal
#' @export
as_sass_layer <- function(x) {
  if (is_sass_layer(x)) return(x)
  # sass_bundle(x) will auto upgrade to a sass bundle object
  layers <- rlang::set_names(sass_bundle(x)$layers, NULL)
  sass_layer_struct(
    functions = pluck(layers, "functions"),
    defaults = pluck(rev(layers), "defaults"),
    mixins = pluck(layers, "mixins"),
    rules = pluck(layers, "rules"),
    declarations = pluck(layers, "declarations"),
    html_deps = pluck(layers, "html_deps"),
    file_attachments = pluck(layers, "file_attachments"),
    validate = FALSE
  )
}

pluck <- function(x, y) {
  res <- dropNulls(lapply(x, `[[`, y))
  if (length(res) == 0) {
    return(NULL)
  }
  unlist(res, recursive = FALSE, use.names = TRUE)
}

extract_file_attachments <- function(x) {
  if (is_sass_bundle_like(x)) {
    return(as_sass_layer(x)$file_attachments)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  unlist(
    lapply(x, extract_file_attachments),
    recursive = FALSE, use.names = TRUE
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
