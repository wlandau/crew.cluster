# TODO: always test and bump required versions when the next
# crew is on CRAN.
skip_if_low_dep_versions <- function() {
  sufficient_versions <- rlang::is_installed(
    pkg = "crew (>= 0.3.0)"
  )
  if (!sufficient_versions) {
    skip("version of crew is too low")
  }
}
