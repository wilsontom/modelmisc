
context("helpers")

test_that("helpers", {

  # PCA

  pca_ob <- prcomp(iris[,-5], scale = TRUE)

  expect_true(is.numeric((variance_exp(pca_ob))))
  expect_that(length(variance_exp(pca_ob)), equals(length(pca_ob$sdev)))
  expect_error(variance_exp(iris[,-5]))

  #RF Prox

  rfModel <- randomForest::randomForest(iris[,-5],iris[,5], proximity = TRUE)
  expect_true(is.data.frame(proximity_to_mds(rfModel)))

  rfModelno <- randomForest::randomForest(iris[,-5],iris[,5], proximity = FALSE)
  expect_error(proximity_to_mds(rfModelno))
  expect_error(proximity_to_mds(pca_ob))


  # Forest (training) metrics

  expect_true(is.numeric(forest_kappa(rfModel)))
  expect_true(is.numeric(forest_auc(rfModel)))


  # feature ranks
  rfModelimp <- randomForest::randomForest(iris[,-5],iris[,5], proximity = TRUE, importance = TRUE)
  expect_error(forest_feat_ranks(rfModel, meth = "perm"))
  expect_true(is.data.frame(forest_feat_ranks(rfModel, meth = "gini")))
  expect_true(is.data.frame(forest_feat_ranks(rfModelimp, meth = "perm")))


  }
)
