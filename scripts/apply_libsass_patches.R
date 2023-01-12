#!/usr/bin/env Rscript
# Applies patches stored in scripts/patches
# Should be run after running scripts/update_libsass.R

library(rprojroot)

# Remove libsass test files (the entire `test` directory) since they contain unicode
# directory paths, and R CMD check provides a warning.
unlink(
  find_package_root_file("src/libsass/test"),
  recursive = TRUE
)

patch_dir <- find_package_root_file("scripts/patches")

for (patch in list.files(patch_dir, full.names = TRUE)) {
  tryCatch({
    message(sprintf("Applying %s", basename(patch)))
    system(sprintf("git apply --reject --whitespace=fix '%s'", patch))
  },
  error = function(e) {
    quit(save = "no", status = 1)
  }
  )
}
