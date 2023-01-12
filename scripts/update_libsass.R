# Script to update libsass from https://github.com/sass/libsass/
# After running this, run the scripts/apply_libsass_patches.R script

(function() {
  owd <- getwd()
  on.exit({setwd(owd)})


  ROOT <- rprojroot::find_package_root_file()
  setwd(ROOT)

  LIBSASS_VERSION <- "3.6.5"

  url <- sprintf(
    "https://github.com/sass/libsass/archive/%s.tar.gz",
    LIBSASS_VERSION
  )

  destfile <- tempfile("libsass-tarball-")
  on.exit({unlink(destfile)}, add = TRUE)
  download.file(url, destfile = destfile)

  exdir <- file.path("src")

  untar(tarfile = destfile, exdir = exdir)

  finaldir <- file.path(exdir, "libsass")
  unlink(finaldir, recursive = TRUE)
  file.rename(
    file.path(exdir, paste0("libsass-", LIBSASS_VERSION)),
    finaldir
  )

  message("Saved in ", finaldir)
})()
