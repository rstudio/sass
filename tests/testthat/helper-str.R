# R-devel (svn r90805) changed str() to consistently print a space after the
# ":" separator (e.g., "$ :List of 10" is now "$ : List of 10"). Normalize
# str() output in snapshots so tests pass on both old and new R.
normalize_str <- function(lines) {
  gsub(":List of ", ": List of ", lines, fixed = TRUE)
}
