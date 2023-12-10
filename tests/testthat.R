library(testthat)
library(crew.cluster)

# TODO: remove conditional and require crew >= 0.7.0.
if (packageVersion("crew") > "0.6.0") {
  test_check("crew.cluster")
}
