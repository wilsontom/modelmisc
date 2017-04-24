
context("feature stability")

test_that("distances", {

  some_random_rank_a <- seq(1, 10)
  some_random_rank_b <- seq(1, 20)
  some_random_rank_c <- seq(21, 30)

  some_random_vector_a <-  sample(c(letters, LETTERS), 20, replace = FALSE)
  some_random_vector_b <-  sample(c(letters, LETTERS), 10, replace = FALSE)
  some_random_vector_c <-  sample(c(LETTERS), 10, replace = FALSE)

  expect_error(canberraDist(some_random_rank_a,some_random_rank_b))
  expect_that(canberraDist(some_random_rank_a,some_random_rank_c), not(throws_error()))
  expect_error(canberraDist(some_random_vector_a,some_random_vector_b))

  expect_error(diceSorensen(some_random_rank_a,some_random_rank_b))
  expect_that(diceSorensen(some_random_vector_a,some_random_vector_b), not(throws_error()))
  expect_that(diceSorensen(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(diceSorensen(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(jaccardsIndex(some_random_rank_a,some_random_rank_b))
  expect_that(jaccardsIndex(some_random_vector_a,some_random_vector_b), not(throws_error()))
  expect_that(jaccardsIndex(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(jaccardsIndex(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(kunchevasIndex(some_random_rank_a,some_random_rank_b, m = 27))
  expect_error(kunchevasIndex(some_random_vector_a,some_random_vector_b, m = 27))
  expect_that(kunchevasIndex(some_random_vector_b,some_random_vector_c, m = 17), not(throws_error()))
  expect_that(kunchevasIndex(some_random_vector_b,some_random_vector_c, m = 17), is_less_than(1))
  expect_that(kunchevasIndex(some_random_vector_b,some_random_vector_c, m = 17), is_more_than(-1))

  expect_error(ochiaisIndex(some_random_rank_a,some_random_rank_b))
  expect_that(ochiaisIndex(some_random_vector_a,some_random_vector_b), not(throws_error()))
  expect_that(ochiaisIndex(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(ochiaisIndex(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(hammingDist(some_random_rank_a,some_random_rank_b,m = 27))
  expect_that(hammingDist(some_random_vector_a,some_random_vector_b,m = 27), not(throws_error()))
  expect_that(hammingDist(some_random_vector_a,some_random_vector_b,m = 27), is_less_than(1))
  expect_that(hammingDist(some_random_vector_a,some_random_vector_b,m = 27), is_more_than(0))

  expect_error(RPT(some_random_vector_a,some_random_vector_b,beta = 1))
  expect_error(RPT(some_random_rank_a,some_random_rank_b,beta = 1))
  expect_error(RPT(0.5,1.5,beta = 1))
  expect_error(RPT(1.5,0.5,beta = 1))
  expect_that(RPT(0.75, 0.5, beta = 1), not(throws_error()))

  }
)
