#' @title Regression Random Forest Learner
#'
#' @aliases mlr_learners_regr.randomForest
#'
#' @description
#' A [mlr3::LearnerRegr] for a classification random from package \CRANpkg{randomForest}.
#' Calls [randomForest::randomForest()].
#'
#' @references
#' Breiman, L. (2001).
#' Random Forests
#' Machine Learning
#' \url{https://doi.org/10.1023/A:1010933404324}
#'
#' @export
LearnerRegrRandomForest = R6Class("LearnerRegrRandomForest", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "regr.randomForest",
        packages = "randomForest",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "ntree", default = 500L, lower = 1L, tags = c("train", "predict")),
            ParamInt$new(id = "mtry", lower = 1L, tags = "train"),
            ParamLgl$new(id = "replace", default = TRUE, tags = "train"),
            ParamUty$new(id = "strata", tags = "train"),
            ParamUty$new(id = "sampsize", tags = "train"), # lower = 1L
            ParamInt$new(id = "nodesize", default = 5L, lower = 1L, tags = "train"),
            ParamInt$new(id = "maxnodes", lower = 1L, tags = "train"),
            ParamFct$new(id = "importance", default = "none", levels = c("mse", "nudepurity", "none"), tag = "train"), # importance is a logical value in the randomForest package.
            ParamLgl$new(id = "localImp", default = FALSE, tags = "train"),
            ParamLgl$new(id = "proximity", default = FALSE, tags = "train"),
            ParamLgl$new(id = "oob.prox", tags = "train"),
            ParamLgl$new(id = "norm.votes", default = TRUE, tags = "train"),
            ParamLgl$new(id = "do.trace", default = FALSE, tags = "train"),
            ParamLgl$new(id = "keep.forest", default = TRUE, tags = "train"),
            ParamLgl$new(id = "keep.inbag", default = FALSE, tags = "train")
          )
        ),
        properties = c("weights", "importance", "oob_error")
      )
    },

    #' @description
    #' The importance scores are extracted from the slot `importance`.
    #' Parameter 'importance' must be set to either `"mse"` or `"nodepurity"`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      imp = data.frame(self$model$importance)
      colnames(imp)[colnames(imp) == "X.IncMSE"] = "%IncMSE"
      ## correct for language error on special characters
      pars = self$param_set$get_values()

      scores = switch(pars[["importance"]],
        "mse" = imp[["%IncMSE"]],
        "nodepurity" = imp[["IncNodePurity"]],
        stop("No importance available. Try setting 'importance' to 'accuracy' or 'gini'")
      )

      sort(setNames(scores, rownames(imp)), decreasing = TRUE)
    },

    #' @description
    #' OOB errors are extracted from the model slot `mse`.
    #' @return `numeric(1)`.
    oob_error = function() {
      mean(self$model$mse)
    }
  ),

  private = list(
    .train = function(task) {
      if (is.null(self$param_set$values[["importance"]])) self$param_set$values[["importance"]] = "none"
      pars = self$param_set$get_values()
      # setting the importance value to logical
      pars[["importance"]] = (pars[["importance"]] != "none")

      # get formula, data, classwt, cutoff for the randomForest package
      f = task$formula()
      data = task$data()
      levs = levels(data[[task$target_names]])
      n = length(levs)

      invoke(randomForest::randomForest, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      pars = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      type = self$predict_type

      p = invoke(predict, self$model, newdata = newdata,
                 type = type, .args = pars)

      PredictionRegr$new(task = task, response = p)
    }
  )
)
