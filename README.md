README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# sass <a href="https://sass-lang.com"><img src="man/figures/sass-logo-color.png" align="right" height = 150 style="background-color: white; padding-left: 20px;"/></a>

[![Travis build
status](https://travis-ci.org/rstudio/sass.svg?branch=master)](https://travis-ci.org/rstudio/sass)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/rstudio/sass?branch=master&svg=true)](https://ci.appveyor.com/project/rstudio/sass)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sass)](https://cran.r-project.org/package=sass)

The `sass` R package is a CSS preprocessor, letting R developers use
variables, inheritance, and functions to generate dynamic style sheets.

`sass` uses the [Sass](https://sass-lang.com/) CSS extension language.
[Sass](https://sass-lang.com/) is stable, powerful, and CSS compatiable.
`sass` is an R wrapper for [LibSass](https://github.com/sass/libsass), a
fast [Sass](https://sass-lang.com/) compiler written in C++.

## Installation

Install the released version of `sass` from CRAN:

``` r
install.packages("sass")
```

Install the latest development build from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("rstudio/sass")
```

## Getting Started

The [Sass](https://sass-lang.com/) language syntax is similar to CSS,
but allows functions and variables, and it can do arbitrary
computations.

``` r
library(sass)

sass("
  $size: 50%;
  foo { margin: $size * .33; }
")
```

    #> /* CSS */
    #> foo {
    #>   margin: 16.5%;
    #> }

For an overview of the major features of [Sass](https://sass-lang.com/)
such as variables, nesting, and imports check out the official [Sass
Basics](https://sass-lang.com/guide).

## Examples

  - Sass Vignette
      - <https://rstudio.github.io/sass/articles/sass.html>
  - Background color
      - <https://gallery.shinyapps.io/sass-color>
      - `shiny::runApp(system.file("sass-color", package = "sass"))`
  - Font Color
      - <https://gallery.shinyapps.io/sass-font>
      - `shiny::runApp(system.file("sass-font", package = "sass"))`
  - Sizing
      - <https://gallery.shinyapps.io/sass-size>
      - `shiny::runApp(system.file("sass-size", package = "sass"))`
  - Themes
      - <https://gallery.shinyapps.io/sass-theme>
      - `shiny::runApp(system.file("sass-theme", package = "sass"))`

<br />

![](man/figures/sass-color.gif)
