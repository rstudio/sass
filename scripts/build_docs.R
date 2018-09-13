



(function() {

owd <- getwd()
on.exit({
  setwd(owd)
})

setwd(devtools::as.package(".")$path)

devtools::document()

devtools::install()

rmarkdown::render("README.Rmd")

unlink("docs", recursive = TRUE)
pkgdown::build_site()

})()
