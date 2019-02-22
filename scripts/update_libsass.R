# Script to update libsass from https://github.com/sass/libsass/

(function() {
  owd <- getwd()
  on.exit({setwd(owd)})


  ROOT <- rprojroot::find_package_root_file()
  setwd(ROOT)

  # Getting libsass at a recent commit to avoid gcc-8
  # compilation warnings (current release 3.5.5 doesn't
  # have gcc-8 fixes in place)

  # This is from version 3.5.2-161-gb260
  LIBSASS_VERSION <- "b260394263f8db363e0b1bd335edcead011800c3"

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
