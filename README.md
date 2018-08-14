
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sassr

[![Travis build
status](https://travis-ci.org/rstudio/sassr.svg?branch=master)](https://travis-ci.org/rstudio/sassr)

Sassr is a CSS preprocessor, letting you use variables, nesting, and
inheritance in CSS.

Sassr uses the [Sass](https://sass-lang.com/) CSS extension language.
Sass is stable, powerful, and CSS compatiable. Sassr is an R wrapper for
[LibSass](https://github.com/sass/libsass), a fast Sass compiler written
in C++.

## Installation

You can install the released version of sass from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sassr")
```

You can install the latest development build from Github with:

``` r
# install.packages("devtools")
devtools::install_github("rstudio/sassr")
```

## Example

At it’s most basic, Sass allows you to format and perform operations on
CSS code:

``` r
library(sassr)
compile_sass(text = "foo { margin: 122px * .3; }")
#> foo {
#>   margin: 36.6px;
#> }
```

For an overview of the major features such as variables, nesting, and
imports check out the [Sass Basics](https://sass-lang.com/guide).
