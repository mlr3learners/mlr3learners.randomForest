context("classif.randomForest")

test_that("autotest", {
  learner = LearnerRegrEarth$new()
  learner$param_set$values = list(degree = 2)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

