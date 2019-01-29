
if (!require("rhub", quietly = TRUE)) install.packages("rhub")

dir.create("../builds", recursive = TRUE, showWarnings = FALSE)
build_file <- devtools::build(".", "../builds")

platforms <- c("windows-x86_64-release", rhub:::default_cran_check_platforms(build_file))

check_output <- rhub::check_for_cran(
  build_file,
  email = "rich@rstudio.com",
  platforms = platforms,
  show_status = FALSE
)

for (i in seq_along(platforms)) {
  check_output$livelog(i)
}

# check_output$web()

print(check_output)
