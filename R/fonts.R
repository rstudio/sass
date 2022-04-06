#' Helpers for importing web fonts
#'
#' @description
#'
#' Include font file(s) when defining a Sass variable that represents a CSS
#' `font-family` property.
#'
#' @details
#'
#' These helpers **must be used the named list approach to variable
#' definitions**, for example:
#'
#'  ```
#'  list(
#'    list("font-variable" = font_google("Pacifico")),
#'    list("body{font-family: $font-variable}")
#'  )
#'  ```
#'
#' @section Font fallbacks:
#'
#'  By default, `font_google()` downloads, caches, and serves the relevant font
#'  file(s) locally. By locally serving files, there's a guarantee that the font
#'  can render in any client browser, even when the client doesn't have internet
#'  access. However, when importing font files remotely (i.e., `font_google(...,
#'  local = FALSE)` or `font_link()`), it's a good idea to provide fallback
#'  font(s) in case the remote link isn't working (e.g., maybe the end user
#'  doesn't have an internet connection). To provide fallback fonts, use
#'  [font_collection()], for example:
#'
#'  ```
#'  pacifico <- font_google("Pacifico", local = FALSE)
#'  as_sass(list(
#'    list("font-variable" = font_collection(pacifico, "system-ui")),
#'    list("body{font-family: $font-variable}")
#'  ))
#'  ```
#'
#' @section Default flags:
#'
#'  These font helpers encourage best practice of adding a `!default` to Sass
#'  variable definitions, but the flag may be removed via `font_collection()` if
#'  desired.
#'
#'  ```
#'  as_sass(list("font-variable" = pacifico))
#'  #> $font-variable: Pacifico !default;
#'  as_sass(list("font-variable" = font_collection(pacifico, default_flag = F)))
#'  #> $font-variable: Pacifico;
#'  ```
#'
#' @section Serving non-Google fonts locally:
#'
#'  Non-Google fonts may also be served locally with `font_face()`, but it
#'  requires downloading font file(s) and pointing `src` to the right location
#'  on disk. If you want `src` to be a relative file path (you almost certainly
#'  do), then you'll need to mount that resource path using something like
#'  [shiny::addResourcePath()] (for a shiny app) or `servr::httd()` (for static
#'  HTML).
#'
#' @param family A character string with a _single_ font family name.
#' @param local Whether or not download and bundle local (woff) font files.
#' @param cache A [sass::sass_file_cache()] object (or, more generally, a file
#'   caching class with `$get_file()` and `$set_file()` methods). Set this
#'   argument to `FALSE` or `NULL` to disable caching.
#' @param wght One of the following:
#'   * `NULL`, the default weight for the `family`.
#'   * A character string defining an [axis range](https://developers.google.com/fonts/docs/css2#axis_ranges)
#'   * A numeric vector of desired font weight(s).
#' @param ital One of the following:
#'   * `NULL`, the default `font-style` for the `family`.
#'   * `0`, meaning `font-style: normal`
#'   * `1`, meaning `font-style: italic`
#'   * `c(0, 1)`, meaning both `normal` and `italic`
#' @param display the `font-display` `@font-face` property.
#'
#' @return a [sass_layer()] holding an [htmltools::htmlDependency()] which points
#'   to the font files.
#'
#' @references <https://developers.google.com/fonts/docs/css2>
#' @references <https://developer.mozilla.org/en-US/docs/Web/CSS/@font-face>
#' @references <https://developer.mozilla.org/en-US/docs/Learn/CSS/Styling_text/Web_fonts>
#'
#' @export
#' @rdname font_face
#' @examples
#'
#' library(htmltools)
#'
#' my_font <- list("my-font" = font_google("Pacifico"))
#' hello <- tags$body(
#'   "Hello",
#'   tags$style(
#'     sass(
#'       list(
#'         my_font,
#'         list("body {font-family: $my-font}")
#'       )
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   browsable(hello)
#' }
#'
#' # Three different yet equivalent ways of importing a remotely-hosted Google Font
#' a <- font_google("Crimson Pro", wght = "200..900", local = FALSE)
#' b <- font_link(
#'   "Crimson Pro",
#'   href = "https://fonts.googleapis.com/css2?family=Crimson+Pro:wght@200..900"
#' )
#' url <- "https://fonts.gstatic.com/s/crimsonpro/v13/q5uDsoa5M_tv7IihmnkabARboYF6CsKj.woff2"
#' c <- font_face(
#'   family = "Crimson Pro",
#'   style = "normal",
#'   weight = "200 900",
#'   src = paste0("url(", url, ") format('woff2')")
#' )
font_google <- function(family, local = TRUE,
                        cache = sass_file_cache(sass_cache_context_dir()),
                        wght = NULL, ital = NULL, display = c("swap", "auto", "block", "fallback", "optional")) {
  stopifnot(is.logical(local))
  if (!is.null(wght)) {
    stopifnot(is.character(wght) || is.numeric(wght))
    wght <- sort(wght)
  }
  if (!is.null(ital)) {
    stopifnot(all(ital %in% c(0, 1)))
    ital <- sort(ital)
  }
  display <- match.arg(display)

  axis_rng <-
    if (is.null(wght) && is.null(ital)) {
      ""
    } else if (is.null(ital)) {
      paste0(":wght@", paste0(wght, collapse = ";"))
    } else if (is.null(wght)) {
      paste0(":ital@", paste0(ital, collapse = ";"))
    } else {
      paste0(":ital,wght@", paste0(
        apply(expand.grid(wght, ital)[, 2:1], 1, paste0, collapse = ","),
        collapse = ";"
      ))
    }

  x <- list(
    family = family,
    local = isTRUE(local), cache = cache,
    href = paste0(
      "https://fonts.googleapis.com/css2?family=",
      family, axis_rng, "&display=", display
    )
  )

  dep_func <- if (x$local) font_dep_google_local else font_dep_link
  font_object(x, dep_func)
}

#' @rdname font_face
#' @export
#' @param href A URL resource pointing to the font data.
font_link <- function(family, href) {
  font_object(list(family = family, href = href), font_dep_link)
}

#' @rdname font_face
#' @export
#' @param src A character vector for the `src` `@font-face` property. Beware
#'   that is character strings are taken verbatim, so careful quoting and/or URL
#'   encoding may be required.
#' @param weight A character (or numeric) vector for the `font-weight`
#'   `@font-face` property.
#' @param display A character vector for the `font-display` `@font-face`
#'   property.
#' @param style A character vector for the `font-style` `@font-face` property.
#' @param stretch A character vector for the `font-stretch` `@font-face`
#'   property.
#' @param variant A character vector for the `font-variant` `@font-face`
#'   property.
#' @param unicode_range A character vector for `unicode-range` `@font-face`
#'   property.
font_face <- function(family, src, weight = NULL, style = NULL,
                      display = c("swap", "auto", "block", "fallback", "optional"),
                      stretch = NULL, variant = NULL, unicode_range = NULL) {

  x <- dropNulls(list(
    family = quote_css_font_families(family),
    src = src,
    weight = weight,
    style = style,
    display = if (!is.null(display)) match.arg(display),
    stretch = stretch,
    variant = variant,
    unicode_range = unicode_range
  ))

  # Multiple src values are separated by "," (everything else by white space)
  # TODO: src could accept a list of named lists which might give us the
  # opportunity to handle quoting and encoding of URLs
  for (prop in names(x)) {
    collapse <- switch(prop, src = , unicode_range = ", ", " ")
    x[[prop]] <- paste0(x[[prop]], collapse = collapse)
  }
  x$css <- font_face_css(x)

  font_object(x, font_dep_face)
}

font_face_css <- function(x) {
  props <- names(x)
  font_prop <- !props %in% c("src", "unicode_range")
  props[font_prop] <- paste0("font-", props[font_prop])
  paste0(
    "@font-face {\n",
    paste0("  ", props, ": ", x, ";", collapse = "\n"),
    "\n}"
  )
}

font_object <- function(x, dep_func) {
  stopifnot(is.function(dep_func))
  if (!is_string(x$family)) {
    stop(
      "Font `family` definitions must be a character string (length 1).",
      call. = FALSE
    )
  }
  # Dependency functions want to use unquoted family name
  new_font_collection(
    families = x$family,
    # Produce dependency at render-time (i.e., tagFunction())
    # so the context-aware caching dir has the proper context
    html_deps = tagFunction(function() dep_func(x))
  )
}

#' @rdname font_face
#' @param ... a collection of `font_google()`, `font_link()`, `font_face()`, and/or character vector(s) (i.e., family names to include in the CSS `font-family` properly). Family names are automatically quoted as necessary.
#' @param default_flag whether or not to include a `!default` when converted to a Sass variable with [as_sass()].
#' @param quote whether or not to attempt automatic quoting of family names.
#' @export
font_collection <- function(..., default_flag = TRUE, quote = TRUE) {
  fonts <- dropNulls(list2(...))

  # Transform syntax like font_collection(google = "Pacifico")
  # into font_collection(font_google("Pacifico"))
  # the primary motication for doing this is to support a Rmd
  # syntax like this (for bslib theming):
  # ---
  # theme:
  #   base_font:
  #     google: Pacifico
  # ---
  fonts <- Map(
    names2(fonts), fonts,
    f = function(nm, val) {
      if (identical(nm, "")) return(val)

      func <- known_font_helpers[[nm]] %||% rlang::abort(
        paste0(
          "Unsupported argument name: ", nm, ".\n",
          "Did you want to try one of these names instead: ",
          paste0(names(known_font_helpers), collapse = ", "), "?"
        )
      )

      do.call(func, as.list(val))
    }
  )

  families <- lapply(fonts, function(x) {
    if (is_font_collection(x))
      return(x$families)
    if (is.character(x) && isTRUE(all(nzchar(x, keepNA = TRUE))))
      return(x)
    stop(
      "`font_collection()` expects a collection of `font_google()`, `font_link()`, `font_face()`, and/or non-empty character strings.",
      call. = FALSE
    )
  })

  families <- unlist(families, recursive = FALSE, use.names = FALSE)

  deps <- lapply(fonts, function(x) {
    if (is_font_collection(x)) x$html_deps
  })
  new_font_collection(
    families = families,
    html_deps = unlist(deps, recursive = FALSE, use.names = FALSE),
    default_flag = isTRUE(default_flag),
    quote = quote
  )
}


known_font_helpers <- list(
  "google" = font_google,
  "link" = font_link,
  "face" = font_face,
  "collection" = font_collection
)

new_font_collection <- function(families, html_deps, default_flag = TRUE, quote = TRUE) {
  add_class(
    list(
      families = if (isTRUE(quote)) quote_css_font_families(families) else families,
      html_deps = html_deps,
      default_flag = default_flag
    ),
    "font_collection"
  )
}

#' @rdname font_face
#' @param x test whether `x` is a `font_collection()`, `font_google()`, `font_link()`, or `font_face()` object.
#' @export
is_font_collection <- function(x) {
  inherits(x, "font_collection")
}

quote_css_font_families <- function(x) {
  stopifnot(is.character(x))

  quoted_contents <- c(
    unlist(regmatches(x, gregexpr("'([^']*)'", x))),
    unlist(regmatches(x, gregexpr('"([^"]*)"', x)))
  )
  if (any(grepl(",", quoted_contents))) {
    x <- paste0(x, collapse = ", ")
    warning(
      "`sass::font_collection()` doesn't automatically quote CSS ",
      "`font-family` names when they contain a ','. ",
      "If fonts don't render properly, make sure family names are ",
      "quoted properly: ", x,
      call. = FALSE
    )
    return(x)
  }

  pieces <- trim_ws(unlist(strsplit(x, ",")))

  # Are there non-alpha, non-dash characters? If so, then quote
  needs_quote <- grepl("[^A-Za-z-]", pieces, perl = TRUE)
  has_quote <- grepl("^'", pieces) | grepl('^"', pieces)
  pieces <- ifelse(
    needs_quote & !has_quote,
    paste0("'", pieces, "'"),
    pieces
  )

  paste0(pieces, collapse = ", ")
}


font_dep_name <- function(x) {
  gsub("\\s+", "_", trim_ws(x$family))
}

#' @import htmltools
font_dep_face <- function(x) {
  # TODO: memoise::memoise() this or do something similar
  # to output_template() to reduce file redundancy?
  src_dir <- tempfile()
  dir.create(src_dir)
  writeLines(x$css, file.path(src_dir, "font.css"))
  htmlDependency(
    font_dep_name(x), get_package_version("sass"),
    src = src_dir,
    stylesheet = "font.css",
    all_files = FALSE
  )
}

font_dep_link <- function(x) {
  htmlDependency(
    font_dep_name(x), get_package_version("sass"),
    head = format(tags$link(
      href = utils::URLencode(x$href),
      rel = "stylesheet"
    )),
    # The src dir doesn't actually matter...this is just a way
    # to pass along <link> tags as a dependency
    src = tempdir(), all_files = FALSE
  )
}

# -------------------------------------------------------
# Local dependency logic
# -------------------------------------------------------

# For our purposes, cache objects must support these methods.
is_cache_object <- function(x) {
  # Use tryCatch in case the object does not support `$`.
  tryCatch(
    is.function(x$get_file) && is.function(x$set_file),
    error = function(e) FALSE
  )
}

resolve_cache <- function(cache) {
  if (is_cache_object(cache)) return(cache)
  list(
    get_file = function(...) FALSE,
    set_file = function(...) FALSE
  )
}

font_dep_google_local <- function(x) {
  # TODO: memoise::memoise() this or do something similar
  # to output_template() to reduce file redundancy?
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  css_file <- file.path(tmpdir, "font.css")

  # TODO: could the hash be more aware of when the href updates?
  x$cache <- resolve_cache(x$cache)
  css_key <- rlang::hash(x$href)
  css_hit <- x$cache$get_file(css_key, css_file)

  # Even if we have a cache hit on the CSS file, we may need
  # to re-download font file(s) if they've been pruned from the cache
  css <- if (css_hit) readLines(css_file) else read_gfont_url(x$href, css_file)

  # basename() of these url()s contain a hash key of the font data
  urls <- extract_group(css, "url\\(([^)]+)")
  basenames <- basename(urls)

  # If need be, download the font file(s) that the CSS imports,
  # and modify the CSS to point to the local files
  Map(function(url, nm) {
    key <- rlang::hash(nm)
    f <- file.path(tmpdir, nm)
    hit <- x$cache$get_file(key, f)
    if (hit) return()
    # In the event we have a CSS cache hit but miss here, url should actually be
    # a local file. In that case, bust the CSS cache, and start over so we know
    # where to find the font files
    if (!grepl("^\\s*http", url)) {
      x$cache$remove(css_key)
      return(font_dep_google_local(x))
    }
    download_file(url, f)
    x$cache$set_file(key, f)
    css <<- sub(url, nm, css, fixed = TRUE)
  }, urls, basenames)

  # Cache the *modified* form of the CSS file
  # (with the local file paths instead of remote URLs)
  if (!css_hit) {
    writeLines(css, css_file)
    x$cache$set_file(css_key, css_file)
  }

  htmltools::htmlDependency(
    font_dep_name(x), get_package_version("sass"),
    src = dirname(css_file),
    stylesheet = basename(css_file),
    all_files = TRUE
  )
}

# Request the relevant @font-face definitions for the font url
# (without the IE11 user-agent header we'd get truetype fonts, but
# there's no reason why we can't use woff, which IE11 supports)
read_gfont_url <- function(url, file) {
  download_file(
    utils::URLencode(url), file,
    headers = c(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko"
    )
  )
  readLines(file)
}

extract_group <- function(x, pattern, which = 1) {
  matches <- regmatches(x, regexec(pattern, x))
  na.omit(sapply(matches, "[", which + 1))
}

# similar to thematic:::download_file, but also translates headers to curl
#' @importFrom stats na.omit
#' @importFrom utils download.file packageVersion
download_file <- function(url, dest, headers = NULL, ...) {
  if (is_installed("curl")) {
    if (!curl::has_internet()) {
      warning(
        "Looks like you don't have internet access, which is needed to ",
        "download and install Google Fonts files. Try either changing ",
        "thematic::font_spec(), manually installing the relevant font, or ",
        "trying again with internet access.",
        call. = FALSE
      )
    }
    handle <- curl::handle_setheaders(curl::new_handle(), .list = headers)
    return(curl::curl_download(url, dest, handle = handle, quiet = FALSE, ...))
  }

  if (capabilities("libcurl")) {
    return(download.file(url, dest, method = "libcurl", headers = headers, ...))
  }

  stop(
    "Downloading Google Font files requires either the curl package or ",
    "`capabilities('libcurl')`. ", call. = FALSE
  )
}
