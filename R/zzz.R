#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

dummy_import = function() {
  # R CMD check does not detect the usage of randomForest in R6 classes
  # This function is a workaround to suppress check notes about
  # "All declared imports should be used"
  randomForest::randomForest()
}

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

  x$add("classif.randomForest", LearnerClassifRandomForest)
  x$add("regr.randomForest", LearnerRegrRandomForest)
}

.onLoad = function(libname, pkgname) {
  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end
