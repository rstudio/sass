#' Compiler Options for Sass
#'
#' Set compiler options for Sass. Used with \code{\link{sass}}.
#'
#'
#' @param precision Number of decimal places.
#' @param output_style Bracketing and formatting style of the CSS output.
#'   Possible styles: \code{"nested"}, \code{"expanded"}, \code{"compact"}, and
#'   \code{"compressed"}.
#' @param indented_syntax Enables the compiler to parse Sass Indented Syntax in
#'   strings. Note that the compiler automatically overrides this option to
#'   \code{TRUE} or \code{FALSE} for files with .sass and .scss file extensions
#'   respectively.
#' @param indent_type Specifies the indent type as \code{"space"} or
#'   \code{"tab"}.
#' @param indent_width Number of tabs or spaces used for indentation. Maximum
#'   10.
#' @param include_path Vector of paths used to resolve \code{@import}. Multiple
#'   paths are possible using a character vector of paths.
#' @param source_comments Annotates CSS output with line and file comments from
#'   Sass file for debugging.
#' @param linefeed Specifies how new lines should be delimited. Possible values:
#'   \code{"lf"}, \code{"cr"}, \code{"lfcr"}, and \code{"crlf"}.
#' @param output_path Specifies the location of the output file. Note: this
#'   option will not write the file on disk. It is only for internal reference
#'   with the source map.
#' @param source_map_file Specifies the location for Sass to write the source
#'   map.
#' @param source_map_root Value will be included as source root in the source
#'   map information.
#' @param source_map_embed Embeds the source map as a data URI.
#' @param source_map_contents Includes the contents in the source map
#'   information.
#' @param omit_source_map_url Disable the inclusion of source map information in
#'   the output file. Note: must specify \code{output_path} when \code{TRUE}.
#'
#' @return List of Sass compiler options to be used with
#'   \code{\link{sass}}.
#'
#' @examples
#' sass(
#'   "foo { margin: 122px * .3; }",
#'   options = sass_options(output_style = "compact")
#' )
#'
#' @export
sass_options <- function(
  precision = 5,
  output_style = "expanded",
  indented_syntax = FALSE,
  include_path = "",
  source_comments = FALSE,
  indent_type = "space",
  indent_width = 2,
  linefeed = "lf",
  output_path = "",
  source_map_file = "",
  source_map_root = "",
  source_map_embed = FALSE,
  source_map_contents = FALSE,
  omit_source_map_url = FALSE
) {
  if (indent_width > 10) {
    warning("Maximum indent width is 10. Setting to 10...")
    indent_width <- 10
  } else if (indent_width < 0) {
    warning("Minimum indent width is 0. Setting to 0...")
    indent_width <- 0
  }

  indent_type <- switch(
    indent_type,
    tab = "\t",
    space = " ",
    stop("invalid indent type. Please specify \"space\" or \"tab\".")
  )

  indent <- strrep(indent_type, indent_width)

  output_style <- switch(
    output_style,
    nested = 0,
    expanded = 1,
    compact = 2,
    compressed = 3,
    stop("output style not supported.")
  )

  linefeed <- switch(
    linefeed,
    lf = "\n",
    cr = "\r",
    crlf = "\r\n",
    lfcr = "\n\r",
    stop("invalid linefeed.")
  )

  sep <- switch(
    .Platform$OS.type,
    unix = ":",
    windows = ";",
    stop("unknown operating system")
  )

  include_path <- paste(include_path, collapse = sep)

  ret <- list(
    precision = precision,
    output_style = output_style,
    indented_syntax = indented_syntax,
    include_path = include_path,
    source_comments = source_comments,
    indent = indent,
    linefeed = linefeed,
    output_path = output_path,
    source_map_file = source_map_file,
    source_map_root = source_map_root,
    source_map_embed = source_map_embed,
    source_map_contents = source_map_contents,
    omit_source_map_url = omit_source_map_url
  )

  class(ret) <- c("sass_options", class(ret))
  ret
}

#' Caching Options for Sass
#'
#' Specifies whether caching is used with sass, and where on the file system the
#' cached data should be stored. Used with \code{\link{sass}}.
#'
#' If caching is enabled, \code{sass()} will attempt to bypass the compilation
#' process by reusing output from previous \code{sass()} calls that used
#' equivalent inputs. This mechanism works by using a hashing algorithm to
#' derive a \emph{cache key} from each \code{sass()} call's \code{input} and
#' \code{options} arguments. If a file named
#' \emph{CACHE_KEY}\code{.sasscache.css} exists within the cache directory, its
#' contents are used instead of performing the compilation. If the file does not
#' exist, then compilation is performed and usual and the results are stored at
#' that file path for next time.
#'
#' If a file that is included using \code{\link{sass_file}} changes on disk
#' (i.e. its last-modified time changes), its previous cache entries will
#' effectively be invalidated (not removed from disk, but they'll no longer be
#' matched). However, if a file imported using \code{sass_file} itself imports
#' other sass files using \code{@import}, changes to those files are invisible
#' to the cache and you will end up with stale results.
#'
#' If a cache directory is explicitly specified (either via the \code{cache_dir}
#' argument or via the \code{sass.cache_dir} R option), note that this package
#' does not do any cleanup of that directory. If disk space is a concern, you
#' will need to delete older entries from that directory yourself.
#'
#' @param cache Logical value indicating whether caching is performed. If no
#'   value is provided, the R option \code{sass.cache} is consulted; if the
#'   option is not set, then caching is performed only if the R session is not
#'   \code{\link{interactive}}.
#' @param cache_dir The directory which will be used to store cache files. If no
#'   value is provided, the R option \code{sass.cache_dir} is consulted; if the
#'   option is not set, then \code{\link{tempdir}} is used. Note that this means
#'   that caches will not persist beyond the current R session by default, since
#'   \code{tempdir()} is unique to each session.
#'
#' @examples
#' # Very slow to compile
#' fib_sass <- "@function fib($x) {
#'   @if $x <= 1 {
#'     @return $x
#'   }
#'   @return fib($x - 2) + fib($x - 1);
#' }
#'
#' body {
#'   width: fib(27);
#' }"
#'
#' # Use a custom cache dir for the purposes of this example. Normally,
#' # you'd want to set the caching behavior using options().
#' temp_cache_dir <- tempfile("sass_cache_dir")
#' dir.create(temp_cache_dir)
#' cache_opts <- sass_cache_options(TRUE, cache_dir = temp_cache_dir)
#'
#' # The first time this runs it will be very slow
#' system.time(sass(fib_sass, cache_options = cache_opts))
#'
#' # But on subsequent calls, it should be very fast
#' system.time(sass(fib_sass, cache_options = cache_opts))
#'
#' @export
sass_cache_options <- function(
  cache = getOption("sass.cache", !interactive()),
  cache_dir = getOption("sass.cache_dir", tempdir())
) {
  force(cache)
  cache_dir <- normalizePath(cache_dir, mustWork = TRUE)

  ret <- list(
    cache = cache,
    cache_dir = cache_dir
  )

  class(ret) <- c("sass_cache_options", class(ret))
  ret
}
