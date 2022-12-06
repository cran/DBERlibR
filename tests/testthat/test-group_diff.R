library(testthat)

test_that("the function of demo_group_diff() analysis works", {
  # store returned values
  results <- demo_group_diff(score_csv_data =
                               system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
                             group_csv_data =
                               system.file("extdata", "demographic_data.csv", package = "DBERlibR"),
                             m_cutoff = 0.15,
                             group_name = "grade")

  x <- results$n_students_deleted # show the number of students deleted from the pre-test dataset
  expect_equal(x, 0)

  x <- results$descriptive_statistics$n[1] # check the sample size in the descriptive statistics
  expect_equal(x, 11)
  x <- results$descriptive_statistics$mean[2] # check the average in the descriptive statistics
  expect_lt(x, 1)

  x <- results$descriptive_statistics$mean[3] # check the average in the descriptive statistics - post2-test data
  expect_lt(x, 1)

  x <- results$welch_anova_test$p # check the results of the one-way anova
  expect_gt(x, 0.05)
  x <- results$games_howell_test$p.adj[1]  # check the results of pairwise comparisons for the one-way anova
  expect_gt(x, 0.05)

  x <- results$kruskal_wallis_test$p.value # check the results of the Kruskal-Wallis test
  expect_gt(x, 0.05)
  x <- results$kruskal_wallis_test_pwc$p.value[1]  # check the results of pairwise comparisons for the Kruskal-Wallis test
  expect_gt(x, 0.05)
})
