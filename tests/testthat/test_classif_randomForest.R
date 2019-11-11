context("classif.randomForest")

test_that("autotest", {
  learner = LearnerClassifRandomForest$new()
  learner$param_set$values = list(ntree = 20L, importance = "gini")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("Learner produces identical results as package version (multiclass, response).", {
  set.seed(20191111)
  data(Boston, package = "MASS")
  Boston$chas = as.factor(Boston$chas)
  Boston$rad = as.factor(Boston$rad)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "rad")
  learner = LearnerClassifRandomForest$new()
  learner$param_set$values = list(ntree = 30L, importance = "gini")
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  set.seed(20191111)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  
  set.seed(20191111)
  model = randomForest(x = Boston[train_set, task$feature_names], y = Boston$rad[train_set], ntree = 30L, importance = TRUE)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")
  
  expect_true(all.equal(prediction$response, as.factor(unname(pred))))
})

test_that("Learner produces identical results as package version (multiclass, prob).", {
  set.seed(20191111)
  data(Boston, package = "MASS")
  Boston$chas = as.factor(Boston$chas)
  Boston$rad = as.factor(Boston$rad)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "rad")
  learner = LearnerClassifRandomForest$new()
  learner$param_set$values = list(ntree = 30L, importance = "gini")
  learner$predict_type <- "prob"
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  set.seed(20191111)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  
  set.seed(20191111)
  model = randomForest(x = Boston[train_set, task$feature_names], y = Boston$rad[train_set], ntree = 30L, importance = TRUE)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "prob")
  
  expect_true(all.equal(unname(prediction$prob), unname(pred)))
})

test_that("Learner produces identical results as package version (twoclass, response).", {
  set.seed(20191111)
  data(Boston, package = "MASS")
  Boston$chas = as.factor(Boston$chas)
  Boston$rad = as.factor(Boston$rad)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifRandomForest$new()
  learner$param_set$values = list(ntree = 30L, importance = "gini")
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  set.seed(20191111)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  
  set.seed(20191111)
  model = randomForest(x = Boston[train_set, task$feature_names], y = Boston$chas[train_set], ntree = 30L, importance = TRUE)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "response")
  
  expect_true(all.equal(prediction$response, as.factor(unname(pred))))
})

test_that("Learner produces identical results as package version (twoclass, prob).", {
  set.seed(20191111)
  data(Boston, package = "MASS")
  Boston$chas = as.factor(Boston$chas)
  Boston$rad = as.factor(Boston$rad)
  task = TaskClassif$new(id = "Boston", backend = Boston, target = "chas")
  learner = LearnerClassifRandomForest$new()
  learner$param_set$values = list(ntree = 30L, importance = "gini")
  learner$predict_type <- "prob"
  train_set = sample(task$nrow, 0.8 * task$nrow)
  test_set = setdiff(seq_len(task$nrow), train_set)
  set.seed(20191111)
  learner$train(task, row_ids = train_set)
  prediction = learner$predict(task, row_ids = test_set)
  
  set.seed(20191111)
  model = randomForest(x = Boston[train_set, task$feature_names], y = Boston$chas[train_set], ntree = 30L, importance = TRUE)
  pred = predict(model, newdata = Boston[test_set, task$feature_names], type = "prob")
  
  expect_true(all.equal(unname(prediction$prob), unname(pred)))
})
