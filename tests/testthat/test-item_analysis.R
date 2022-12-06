library(testthat)

test_that("the function of item_analysis() works", {
  # store returned values
  results <- item_analysis(score_csv_data = system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"))

  x <- results$n_students_deleted # check the number of students deleted
  expect_equal(x, 0)

  x <- results$difficulty_index$difficulty_index[1] # check the difficulty data
  expect_lt(x, 1)

  x <- results$difficulty_index_plot$plot_env$data_original$Q1 # check the sample size
  expect_length(x, 50)

  x <- results$discrimination_index$discrimination_index[1] # check the discrimination data
  expect_lt(x, 0)

  x <- results$discrimination_index_plot$data$avg_score[23] # check the calculation of averages
  expect_equal(x, 1)

  x <- results$non_discrimination_items  # check the number of non-discrimination items
  expect_vector(x, ptype = character(), size = 12)
})
