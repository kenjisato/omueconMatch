pkg_file <- function(..., pkg = .packageName) {
  system.file(..., package = pkg, mustWork = TRUE)
}

