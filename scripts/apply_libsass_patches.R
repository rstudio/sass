#!/usr/bin/env Rscript
# Applies patches stored in scripts/patches
# Should be run after running scripts/update_libsass.R

library(rprojroot)

patch_dir <- rprojroot::find_package_root_file("scripts/patches")

for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch({
    message(sprintf("Applying %s", basename(patch)))
    system(sprintf("git apply '%s'", patch))
  },
  error = function(e) {
    quit(save = "no", status = 1)
  }
  )
}
