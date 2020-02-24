context("regr.randomForest")

test_that("autotest", {
  learner = LearnerRegrRandomForest$new()
  learner$param_set$values = list(ntree = 20, importance = "mse")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("Learner produces identical results as package version.", {
  set.seed(20191111)
  data(Boston, package = "MASS")
  Boston$chas = as.factor(Boston$chas)
  Boston$rad = as.factor(Boston$rad)
  task = TaskRegr$new(id = "Boston", backend = Boston, target = "crim")
  learner = LearnerRegrRandomForest$new()
  learner$param_set$values = list(ntree = 30L, importance = "mse")
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  set.seed(20191111)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)

  set.seed(20191111)
  model = randomForest::randomForest(x = Boston[train_set, task$feature_names], y = Boston$crim[train_set], ntree = 30L, importance = TRUE)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")

  expect_true(all.equal(prediction$response, unname(pred)))
})
