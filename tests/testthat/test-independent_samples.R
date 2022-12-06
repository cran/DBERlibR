library(testthat)

test_that("the function of independent_samples() works", {

  # store returned values
  results <- independent_samples(treat_csv_data =
                                   system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
                                 ctrl_csv_data =
                                   system.file("extdata", "data_ctrl_post.csv", package = "DBERlibR"),
                                 m_cutoff = 0.15)

  x <- results$n_students_deleted$n[1] # show the number of students deleted from the pre-test dataset
  expect_equal(x, 1)
  x <- results$n_students_deleted$n[2] # show the number of students deleted from the post-test dataset
  expect_equal(x, 0)

  x <- results$descriptive_statistics$mean[1] # check the average in the descriptive statistics - control group post-test data
  expect_lt(x, 1)
  x <- results$descriptive_statistics$mean[2] # check the average in the descriptive statistics - treatment group post-test data
  expect_lt(x, 1)

  x <- results$shapiro_wilk_test$p[2] # check the normality of the treatment group data
  expect_gt(x, 0.05)
  x <- results$shapiro_wilk_test$p[1]  # check the normality of the control group data
  expect_gt(x, 0.05)

  x <- results$independent_samples_t_test_equal$p.value # check the p.value from t-test results (equal)
  expect_lt(x, 0.01)
  x <- results$independent_samples_t_test_unequal$p.value # check the p.value from t-test results (unequal)
  expect_lt(x, 0.01)
  x <- results$mann_whitney_u_test$p.value  # check the p.value from Mann-Whitney U test results
  expect_lt(x, 0.01)
})
