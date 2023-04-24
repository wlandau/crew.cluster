#' crew.cluster: crew launcher plugins for traditional high-performance
#'   computing clusters
#' @docType package
#' @name crew.cluster-package
#' @description In computationally demanding analysis projects,
#'   statisticians and data scientists asynchronously
#'   deploy long-running tasks to distributed systems,
#'   ranging from traditional clusters to cloud services.
#'   The `crew.cluster` package extends the
#'   [`mirai`](https://github.com/shikokuchuo/mirai)-powered
#'   [`crew`](https://wlandau.github.io) package with worker
#'   launcher plugins for traditional
#'   high-performance computing systems.
#'   Inspiration also comes from packages
#'   [`mirai`](https://github.com/shikokuchuo/mirai),
#'   [`future`](https://future.futureverse.org/),
#'   [`rrq`](https://mrc-ide.github.io/rrq/),
#'   [`clustermq`](https://mschubert.github.io/clustermq/),
#'   and [`batchtools`](https://mllg.github.io/batchtools/).
#' @family help
#' @importFrom crew crew_assert crew_class_launcher crew_launcher
#'   crew_random_name
#' @importFrom processx run
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#' @importFrom utils globalVariables
NULL

utils::globalVariables(".")
