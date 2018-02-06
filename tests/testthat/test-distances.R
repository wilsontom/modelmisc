
context("feature stability")

test_that("distances", {

  some_random_rank_a <- seq(1, 10)
  some_random_rank_b <- seq(1, 20)
  some_random_rank_c <- seq(21, 30)

  some_random_vector_a <-  sample(c(letters, LETTERS), 20, replace = FALSE)
  some_random_vector_b <-  sample(c(letters, LETTERS), 10, replace = FALSE)
  some_random_vector_c <-  sample(c(LETTERS), 10, replace = FALSE)

  expect_error(canberra_distance(some_random_rank_a,some_random_rank_b))
  expect_error(canberra_distance(some_random_rank_a,some_random_rank_c), NA)
  expect_error(canberra_distance(some_random_vector_a,some_random_vector_b))

  expect_error(dice_sorensen(some_random_rank_a,some_random_rank_b))
  expect_error(dice_sorensen(some_random_vector_a,some_random_vector_b), NA)
  expect_that(dice_sorensen(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(dice_sorensen(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(jaccards_index(some_random_rank_a,some_random_rank_b))
  expect_error(jaccards_index(some_random_vector_a,some_random_vector_b), NA)
  expect_that(jaccards_index(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(jaccards_index(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(ochiais_index(some_random_rank_a,some_random_rank_b))
  expect_error(ochiais_index(some_random_vector_a,some_random_vector_b), NA)
  expect_that(ochiais_index(some_random_vector_a,some_random_vector_b), is_less_than(1))
  expect_that(ochiais_index(some_random_vector_a,some_random_vector_b), is_more_than(0))

  expect_error(hammings_distance(some_random_rank_a,some_random_rank_b,m = 27))
  expect_error(hammings_distance(some_random_vector_a,some_random_vector_b,m = 27), NA)
  expect_that(hammings_distance(some_random_vector_a,some_random_vector_b,m = 27), is_less_than(1))
  expect_that(hammings_distance(some_random_vector_a,some_random_vector_b,m = 27), is_more_than(0))

  expect_error(RPT(some_random_vector_a,some_random_vector_b,beta = 1))
  expect_error(RPT(some_random_rank_a,some_random_rank_b,beta = 1))
  expect_error(RPT(0.5,1.5,beta = 1))
  expect_error(RPT(1.5,0.5,beta = 1))
  expect_error(RPT(0.75, 0.5, beta = 1), NA)

  }
)
