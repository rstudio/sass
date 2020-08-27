
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sass <a href='https://rstudio.github.io/sass/'><img src='man/figures/logo.svg' align="right" height="139" /></a>

[![R build status](https://github.com/rstudio/sass/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/sass/actions)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sass)](https://cran.r-project.org/package=sass)

<style>
pre {
  border: 1px solid #eee;
}

pre.r {
  background-color: #ffffff;
}

pre.r code {
  background-color: #ffffff;
}

pre.css {
  margin-top: -1.25rem;
  background-color: #f8f8f8;
  border-radius: 0;
  border-bottom-left-radius: 4px;
  border-bottom-right-radius: 4px;
}
</style>

<br>

The `sass` R package provides bindings to
[LibSass](https://github.com/sass/libsass), a fast
[Sass](https://sass-lang.com/) compiler written in C++. Sass is a mature
and stable CSS extension language that makes styling modern websites
less complex and more composible.

### Installation

Install the released version of `sass` from CRAN:

``` r
install.packages("sass")
```

Install the latest development build from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rstudio/sass")
```

### Basic usage

To compile Sass into CSS, provide Sass to the `input` argument of the
`sass()` function. `input` can be any of the following:

  - An R string (as in the example below).
  - A named `list()` defining [Sass
    variables](https://rstudio.github.io/sass/articles/sass.html#variables).
  - A `sass_file()`, `sass_import()`, or `sass_layer()`.
  - A nested `list()` comprising of all the above.

<!-- end list -->

``` r
library(sass)
sass(input = "
  $size: 50%;
  foo { margin: $size * .33; }
")
```

``` css
foo {
  margin: 16.5%;
}
```

### Learn more

See **sass**’s [overview
vignette](https://rstudio.github.io/sass/articles/sass.html) as well as
the official [Sass documentation](https://sass-lang.com/documentation).

### Ask a question

If you have a question about **sass**, try asking one on
<https://community.rstudio.com>:

[![RStudio Ask a question:
sass](https://img.shields.io/badge/Ask%20a%20question-sass-75aadb.svg?style=popout&logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2ZXJzaW9uPSIxLjEiIHg9IjBweCIgeT0iMHB4IiB2aWV3Qm94PSIwIDAgNjI1LjkgNjI1LjkiIHN0eWxlPSJlbmFibGUtYmFja2dyb3VuZDpuZXcgMCAwIDYyNS45IDYyNS45OyIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSI+CjxzdHlsZSB0eXBlPSJ0ZXh0L2NzcyI+Cgkuc3Qwe2ZpbGw6Izc1QUFEQjt9Cgkuc3Qxe2ZpbGw6IzRENEQ0RDt9Cgkuc3Qye2ZpbGw6I0ZGRkZGRjt9Cgkuc3Qze2ZpbGw6dXJsKCNTVkdJRF8xXyk7fQoJLnN0NHtmaWxsOnVybCgjU1ZHSURfMl8pO30KCS5zdDV7ZmlsbDp1cmwoI1NWR0lEXzNfKTt9Cgkuc3Q2e2ZpbGw6dXJsKCNTVkdJRF80Xyk7fQoJLnN0N3tmaWxsOnVybCgjU1ZHSURfNV8pO30KCS5zdDh7ZmlsbDp1cmwoI1NWR0lEXzZfKTt9Cgkuc3Q5e2ZpbGw6dXJsKCNTVkdJRF83Xyk7fQoJLnN0MTB7ZmlsbDp1cmwoI1NWR0lEXzhfKTt9Cgkuc3QxMXtmaWxsOnVybCgjU1ZHSURfOV8pO30KCS5zdDEye2ZpbGw6dXJsKCNTVkdJRF8xMF8pO30KCS5zdDEze29wYWNpdHk6MC4xODtmaWxsOnVybCgjU1ZHSURfMTFfKTt9Cgkuc3QxNHtvcGFjaXR5OjAuMzt9Cjwvc3R5bGU+CjxnIGlkPSJHcmF5X0xvZ28iPgo8L2c+CjxnIGlkPSJCbGFja19MZXR0ZXJzIj4KPC9nPgo8ZyBpZD0iQmx1ZV9HcmFkaWVudF9MZXR0ZXJzIj4KCTxnPgoKCQkJPGVsbGlwc2UgdHJhbnNmb3JtPSJtYXRyaXgoMC43MDcxIC0wLjcwNzEgMC43MDcxIDAuNzA3MSAtMTI3LjkyNjUgMzE3LjAzMTcpIiBjbGFzcz0ic3QwIiBjeD0iMzE4LjciIGN5PSIzMTIuOSIgcng9IjMwOS44IiByeT0iMzA5LjgiLz4KCQk8Zz4KCQkJPHBhdGggY2xhc3M9InN0MiIgZD0iTTQyNC43LDQxMS44aDMzLjZ2MjYuMWgtNTEuM0wzMjIsMzEwLjVoLTQ1LjN2MTAxLjNoNDQuM3YyNi4xSDIwOS41di0yNi4xaDM4LjNWMTg3LjNsLTM4LjMtNC43di0yNC43ICAgICBjMTQuNSwzLjMsMjcuMSw1LjYsNDIuOSw1LjZjMjMuOCwwLDQ4LjEtNS42LDcxLjktNS42YzQ2LjIsMCw4OS4xLDIxLDg5LjEsNzIuM2MwLDM5LjctMjMuOCw2NC45LTYwLjcsNzUuNkw0MjQuNyw0MTEuOHogICAgICBNMjc2LjcsMjg1LjNsMjQuMywwLjVjNTkuMywwLjksODIuMS0yMS45LDgyLjEtNTIuM2MwLTM1LjUtMjUuNy00OS41LTU4LjMtNDkuNWMtMTUuNCwwLTMxLjMsMS40LTQ4LjEsMy4zVjI4NS4zeiIvPgoJCTwvZz4KCTwvZz4KPC9nPgo8ZyBpZD0iV2hpdGVfTGV0dGVycyI+CjwvZz4KPGcgaWQ9IlJfQmFsbCI+CjwvZz4KPC9zdmc+)](https://community.rstudio.com/new-topic?title=&category_id=10&tags=sass&body=%0A%0A%0A%20%20--------%0A%20%20%0A%20%20%3Csup%3EReferred%20here%20by%20%60sass%60%27s%20README%3C/sup%3E%0A&u=rich_i)
