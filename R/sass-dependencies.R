#' Compile Sass to CSS using LibSass, then return an HTML Dependencies
#'
#' Calls `sass(input, ...)` and wraps the output into a [htmltools::htmlDependency()].
#'
#' @inheritParams sass
#' @inheritParams htmltools::htmlDependency
#' @param cache_key additional information to include in the `sass_hash()` call used to
#' determine the output directory. This should include any information that could possibly
#' influence the resulting CSS that isn't already captured by `input`. For example, if
#' `input` contains something like `"@import sass_file.scss"` you may want to include the
#' [file.mtime()] of `sass_file.scss` (or, perhaps, a [packageVersion()] if `sass_file.scss`
#' is bundled with an R package).
#' @param stylesheet Absolute file path(s) pointing to stylesheet(s) to include with the dependency
#'
#' @return returns a list of [htmltools::htmlDependency()] objects. The length
#' of the list use 1 unless `input` contains [sass_layer()] object(s) that
#' have `html_deps`.
#'
#' @export
#' @seealso [sass()]
#' @examples
#' str(sass_html_dependencies(
#'   input = list(list(color = "red"), "body {color: $color}"),
#'   name = "foo", version = "1.0.0",
#'   stylesheet = "my-style.css"
#' ))
sass_html_dependencies <- function(
  input, name, version, stylesheet, cache_key = NULL,
  options = sass_options(), write_attachments = TRUE, cache = sass_cache_get(),
  script = NULL, meta = NULL, head = NULL) {

  key <- sass_hash(list(input, options, cache_key))
  outDir <- file.path(tempdir(), paste(name, version, key, sep = "-"))
  if (!dir.exists(outDir)) dir.create(outDir)
  sass(
    input, output = file.path(outDir, stylesheet), options = options,
    write_attachments = write_attachments, cache = cache
  )

  if (!is.null(script)) {
    if (!all(fs::is_absolute_path(script))) {
      stop("script file paths must all be absolute file paths")
    }
    fs::file_copy(script, outDir, overwrite = TRUE)
    script <- basename(script)
  }

  c(
    htmltools::htmlDependency(
      name = name, version = version,
      src = c(file = outDir),
      stylesheet = stylesheet,
      script = script,
      meta = meta, head = head
    ),
    extract_layer(input)$html_deps
  )
}
