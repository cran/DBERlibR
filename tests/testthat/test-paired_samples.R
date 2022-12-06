library(testthat)

test_that("the function of paired_samples() works", {

  # store returned values
  results <- paired_samples(pre_csv_data =
                              system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
                            post_csv_data =
                              system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
                            m_cutoff = 0.15)

  x <- results$n_students_deleted$n[1] # show the number of students deleted from the pre-test dataset
  expect_equal(x, 0)

    x <- results$n_students_deleted$n[2] # show the number of students deleted from the post-test dataset
  expect_equal(x, 1)

  x <- results$shapiro_wilk_test$p.value # check the normality
  expect_lt(x, 0.5)

  x <- results$descriptive_statistics$mean[1] # check the average in the descriptive statistics - pre-test data
  expect_lt(x, 1)
  x <- results$descriptive_statistics$mean[2] # check the average in the descriptive statistics - post-test data
  expect_lt(x, 1)

  x <- results$paired_samples_t_test$p.value  # check the results of the paired samples t-test
  expect_lt(x, 0.001)

  x <- results$wilcoxon_signed_rank_test$p.value  # check the results of the wilcoxon signed rank test
  expect_lt(x, 0.001)
})

