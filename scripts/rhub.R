
if (!require("rhub", quietly = TRUE)) install.packages("rhub")

root_store <- rprojroot::find_package_root_file()

build_path <- file.path(root_store, "scripts", "build")

dir.create(
  file.path(root_store, "scripts", "build"),
  recursive = TRUE, showWarnings = FALSE
)

build_file <- devtools::build(root_store, build_path)

platforms <-
  c("windows-x86_64-release",
    "debian-gcc-release",
    rhub:::default_cran_check_platforms(build_file))

# platforms <- "debian-gcc-release"

check_output <-
  rhub::check_for_cran(
    build_file,
    email = "carson@rstudio.com",
    platforms = platforms,
    show_status = FALSE
  )

for (i in seq_along(platforms)) {
  check_output$livelog(i)
}

check_output$web()

print(check_output)
