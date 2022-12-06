library(testthat)

test_that("the function of one_way_repeated_anova() works", {

  # store returned values
  results <- one_way_repeated_anova(treat_pre_csv_data =
                                      system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
                                    treat_post_csv_data =
                                      system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
                                    treat_post2_csv_data =
                                      system.file("extdata", "data_treat_post2.csv", package = "DBERlibR"),
                                    m_cutoff = 0.15)

  x <- results$n_students_deleted$n[1] # show the number of students deleted from the pre-test dataset
  expect_equal(x, 0)
  x <- results$n_students_deleted$n[2] # show the number of students deleted from the post-test dataset
  expect_equal(x, 1)
  x <- results$n_students_deleted$n[3] # show the number of students deleted from the post2-test dataset
  expect_equal(x, 0)

  x <- results$descriptive_statistics$mean[1] # check the average in the descriptive statistics - pre-test data
  expect_lt(x, 1)
  x <- results$descriptive_statistics$mean[2] # check the average in the descriptive statistics - post-test data
  expect_lt(x, 1)
  x <- results$descriptive_statistics$mean[3] # check the average in the descriptive statistics - post2-test data
  expect_lt(x, 1)

  x <- results$one_way_repeated_anova$p # check the results of the one-way repeated measures anova
  expect_lt(x, 0.001)
  x <- results$friedman_test$p.value  # check the results of the friedman test
  expect_lt(x, 0.001)
})

