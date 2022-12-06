library(testthat)

test_that("the function of one_way_ancova() works", {

  # store returned values
  results <- one_way_ancova(treat_pre_csv_data =
                              system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
                            treat_post_csv_data =
                              system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
                            ctrl_pre_csv_data =
                              system.file("extdata", "data_ctrl_pre.csv", package = "DBERlibR"),
                            ctrl_post_csv_data =
                              system.file("extdata", "data_ctrl_post.csv", package = "DBERlibR"),
                            m_cutoff = 0.15)

  x <- results$n_students_deleted$n[1] # show the number of students deleted from the treatment group pre-test dataset
  expect_equal(x, 0)
  x <- results$n_students_deleted$n[2] # show the number of students deleted from the treatment group post-test dataset
  expect_equal(x, 1)
  x <- results$n_students_deleted$n[3] # show the number of students deleted from the control group pre-test dataset
  expect_equal(x, 1)
  x <- results$n_students_deleted$n[4] # show the number of students deleted from the control group post-test dataset
  expect_equal(x, 0)

  x <- results$pre_descriptive_statistics$mean[1] # check the average in the descriptive statistics - control group pre-test data
  expect_lt(x, 1)
  x <- results$pre_descriptive_statistics$mean[2] # check the average in the descriptive statistics - treatment group pre-test data
  expect_lt(x, 1)
  x <- results$post_descriptive_statistics$mean[1] # check the average in the descriptive statistics - control group post-test data
  expect_lt(x, 1)
  x <- results$post_descriptive_statistics$mean[2] # check the average in the descriptive statistics - treatment group post-test data
  expect_lt(x, 1)

  x <- results$levene_test$`Pr(>F)`[1] # check the equality of variances
  expect_lt(x, 1)

  x <- results$one_way_ancova$p[1] # check the p.value from the ancova results
  expect_lt(x, 0.01)

  x <- results$estimated_marginal_means$emmean[1] # check the emmean for the treatment group
  expect_lt(x, 1)
  x <- results$estimated_marginal_means$emmean[2] # check the emmean for the control group
  expect_lt(x, 1)

})
