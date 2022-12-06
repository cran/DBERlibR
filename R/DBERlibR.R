###############################################################################
#' Item Analysis
#'
#' This function automatically reads and cleans the data (e.g., converting
#' missing values to "0"), and calculates difficulty and discriminant scores.
#'
#' @import car
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @import emmeans
#' @import ggplot2
#' @import ggpubr
#' @import ggrepel
#' @import readr
#' @import reshape
#' @import rstatix
#' @importFrom psych describe
#' @importFrom psych describeBy
#' @importFrom graphics hist
#' @importFrom stats reshape
#' @importFrom stats TukeyHSD
#' @importFrom stats aov
#' @importFrom stats complete.cases
#' @importFrom stats cor
#' @importFrom stats friedman.test
#' @importFrom stats interaction.plot
#' @importFrom stats kruskal.test
#' @importFrom stats lm
#' @importFrom stats mcnemar.test
#' @importFrom stats pairwise.wilcox.test
#' @importFrom stats reorder
#' @importFrom stats resid
#' @importFrom stats residuals
#' @importFrom stats sd
#' @importFrom stats shapiro.test
#' @importFrom stats t.test
#' @importFrom stats wilcox.test
#' @import tibble
#'
#' @param score_csv_data data This function requires a csv data file.
#' Its name (e.g., "data_treat_pre.csv") can be passed as an argument.
#' Make sure to set the folder with the data file(s) as the working directory.
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' item_analysis(score_csv_data =
#'          system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
#'          m_cutoff = 0.15)
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{difficulty_index}: Calculated difficulty scores
#'  \item \code{difficulty_index_plot}: Plot of difficulty scores in the
#'  ascending order
#'  \item \code{too_difficulty_items}: List of items of which difficulty score
#'  is less than 0.2
#'  \item \code{discrimination_index}: Calculated discrimination scores
#'  \item \code{discrimination_index_plot}: Plot of discrimination scores in the
#'  ascending order
#'  \item \code{non_discrimination_items}: List of items of which discrimination
#'  score is less than 0.2
#' }
#'
#' @export
#'

item_analysis <- function(score_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- X2 <- difficulty_index <- q.number <- avg_score <- NULL

  # Reading
  data_original <- read_csv(score_csv_data, col_types = cols())

  # Deleting students with too many skipped answers-----------------
  nrow_all <- nrow(data_original)
  n <- as.numeric(length(data_original[,-1]))
  data_original <- data_original %>%
                   mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

  data_original <- subset(data_original, m_rate < m_cutoff)
  nrow_subset <- nrow(data_original)
  n_students_deleted <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_original$m_rate=NULL

  # Clean data
  n_col <- ncol(data_original)
  for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
  data_original[is.na(data_original)]= 0
  na.sum <- sum(is.na(data_original))
  # Calculate average scores
  data_original <- data_original %>%
                   mutate(avg_score = rowMeans(data_original[,-1]))

  # ggplots: difficulty index
  suppressWarnings({
  difficulty_index <- data_original[,c(-1)] %>%
                     colMeans() %>%
                     t() %>%
                     as.data.frame() %>%
                     t()
  difficulty_index <- reshape::melt(round(difficulty_index,2), id.vars=c("Q_id"))  %>%
                     dplyr::select(-X2)
  colnames(difficulty_index) = c("q.number","difficulty_index")
  difficulty_index_plot<- ggplot(difficulty_index, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
                    geom_point(alpha=0.9) +
                    geom_vline(xintercept = 0.2, color ="red")+
                    geom_hline(yintercept = difficulty_index$q.number[length(data_original)-1],color="blue") +
                    ggtitle("Difficulty Plot") +
                    xlab("Proportion") +
                    ylab("Question Item Number") +
                    theme_minimal() +
                    geom_text_repel(aes(label = round(difficulty_index,2)))
  })

  # Create data subset: difficulty index lower than 0.2
  toodifficultitems <- subset(difficulty_index, difficulty_index < 0.2)
  toodifficultitems_nrow <- nrow(toodifficultitems)

  # ggplots: discrimination index
  discrimination_data <- cor(data_original[,-1]) %>%
                         as.data.frame()
  discrimination_index_plot <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) +
                         geom_point() +
                         geom_vline(xintercept = 0.2, color ="red") +
                         xlab("Relationship Coefficient") +
                         ylab("Question Item Number") +
                         ggtitle("Discrimination Plot") +
                         theme_minimal() +
                         geom_text_repel(aes(label = round(avg_score,2)))

  # Create data subset: discrimination index lower than 0.2
  qnumber <- colnames(discrimination_data)
  discrimination_index <- round(discrimination_data$avg_score,2)
  discrimination_index <- data.frame(qnumber,discrimination_index)
  nondiscriminantitems <- subset(discrimination_index, discrimination_index < 0.2)
  nondiscriminantitems_nrow <- nrow(nondiscriminantitems)

  ##### present the results in the console for the convenience of users
  message("==============================")
  message("The number of students deleted: ",n_students_deleted, " student(s) has(have) been deleted from the data since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("==================================")
  message("Item Analysis Results - Difficulty")
  print(difficulty_index)
  plot(difficulty_index_plot)
  message("Refer to 'Difficulty Plot' in the 'Plots' panel.", sep="\n")
  too_difficulty_items <- if (toodifficultitems_nrow > 0) {
    message("As seen in the difficulty plot, the following question items present a difficulty plot lower than:")
    print(toodifficultitems$Q_id)
  } else {
    message("As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", sep="\n")
  }
  message("======================================")
  message("Item Analysis Results - Discrimination")
  print(discrimination_index)
  plot(discrimination_index_plot, sep="\n")
  message("Refer to 'Discrimination Plot' in the 'Plots' panel", sep="\n")
  non_discrimination_items <- if (nondiscriminantitems_nrow > 0) {
    message("As seen in the discrimination plot, the following question items present a discrimination index lower than 0.2:")
    print(nondiscriminantitems$qnumber)
  } else {
    message("As seen in the discrimination plot, None of the discrimination indixes was found to be lower than 0.2", sep="\n")
  }

  # To list up return values
  out <- tibble::lst(
    n_students_deleted,
    difficulty_index,
    difficulty_index_plot,
    too_difficulty_items,
    discrimination_index,
    discrimination_index_plot,
    non_discrimination_items,
  )
  return(invisible(out))

}

###############################################################################
#' Paired Samples Data Analysis
#'
#' This function automatically cleans the datasets (e.g., converting missing
#' values to "0), merges pre-post datasets, checks assumptions, and then runs
#' the (parametric) Paired Samples T-test and (nonparametric) Wilcoxon
#' Signed-Rank test to help you examine the difference between pre-post scores.
#'
#' @param pre_csv_data This function requires a csv file with pre-test data.
#' Its name (e.g., "data_treat_pre.csv") can be passed as an argument. Make sure
#' to set the folder with the data file(s) as the working directory.
#' @param post_csv_data This function requires a csv file with post-test data.
#' Its name (e.g., "data_treat_post.csv") can be passed as an argument.
#' Make sure to set the folder with the data file(s) as the working directory.
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' paired_samples(pre_csv_data =
#'          system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
#'          post_csv_data =
#'          system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
#'          m_cutoff = 0.15)
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{shapiro_wilk_test}: Shapiro-Wilk test results to determine
#'  normality
#'  \item \code{normal_qq_plot}: The normal q-q plot to visually inspect the
#'  normality
#'  \item \code{descriptive_statistics}: Descriptive statistics
#'  \item \code{boxplots}: Boxplots - visual presentation of the descriptive
#'  statistics
#'  \item \code{paired_samples_t_test}: Paired samples t-test results
#'  \item \code{wilcoxon_signed_rank_test}: Wilcoxon signed rank test results
#' }
#'
#' @export

paired_samples <- function(pre_csv_data, post_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- avg_score_pre <- avg_score_post <- Time <- Score <- avg_diff <- outliers <- NULL

  # Read pre-post datasets (of the treatment group)
  data_treat_pre <- read_csv(pre_csv_data, show_col_types = FALSE)
  data_treat_post <- read_csv(post_csv_data, show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data")

  #----------------------------------------------------------------

  n <- as.numeric(length(data_treat_pre[,-1]))
  n=n-1

  # Clean data (e.g., Replace skipped answers with "0")
  n_col <- ncol(data_treat_pre)
  for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
  data_treat_pre[is.na(data_treat_pre)]= 0
  na.sum_pre <- sum(is.na(data_treat_pre))
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0

  # Change column names
  colnames(data_treat_pre) <- paste(colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste(colnames(data_treat_post), "post", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>%
    mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))

  # Merge pre/post data
  treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
  treat_data_merged <- treat_data_merged %>%
    mutate(avg_diff=avg_score_post-avg_score_pre)
  Mean_Differences <- treat_data_merged$avg_diff

  # Convert Data Frame to a Long Format & Define the Variable
  avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post)
  df_Long <- melt(avg_score_df)
  names(df_Long) <- c("id", "Time", "Score")
  # Name Time(group) -> 1=Pre, 2=Post
  df_Long$Time <- factor(df_Long$Time,levels=c(1,2),labels=c("Pre", "Post"))
  # Get descriptive statistics
  descriptive_statistics <- df_Long %>%
    group_by(Time) %>%
    get_summary_stats(Score, type="common")
  boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Paired Samples - Boxplots")

  #Check Assumptions
  #No Outliers
  Outliers <- treat_data_merged %>%
    identify_outliers(avg_diff)
  # Normality
  shapiro_wilk_test <- treat_data_merged %>%
    shapiro_test(avg_diff)
  normal_qq_plot <- ggqqplot(treat_data_merged, "avg_diff", title="Normal Q-Q Plot")

  # Run paired samples t-test (two-sided)
  paired_samples_t_test <- t.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, mu=0, alt="two.sided", paired=T, conf.level=0.95)
  ttest_p.value <- paired_samples_t_test$p.value

  # Run paired samples t-test (two-sided)
  suppressWarnings({
    wilcoxon_signed_rank_test <- wilcox.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, paired=T, conf.level=0.95)
  })

  ####### Print the results in the console for the conveniece of the user

  message("======================")
  message("--> ",n_students_deleted_pre, " student(s) from the pre-test data and ", n_students_deleted_post, " student(s) from the post-test data have been deleted since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("=================================================")
  message("Pre-test/Post-test Scores' Descriptive Statistics")
  print(descriptive_statistics)
  message("Refer to the boxplot in the 'Plots' panel to visually examine the descriptive statistics.", sep="\n")
  print(boxplots)
  message("===================================")
  message("Testing the Assumption of Normality")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p > 0.05) {
    message("## Interpretation: the assumption of normality by group has been met (p>0.05)")
  } else {
    message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05)")
  }
  message("Refer to the histogram and normal q-q plot to check the normality visually", sep="\n")
  hist(Mean_Differences)
  print(normal_qq_plot)
  message("=====================")
  message("Paired T-Test Results")
  print(paired_samples_t_test)
  message("## Sample Interpretation of the outputs above:")
  if (paired_samples_t_test$p.value < 0.05) {
    message("--> The average pre-test score was ",round(descriptive_statistics$mean[1],2),
                   " and the average post-test score was ",round(descriptive_statistics$mean[2],2),". ",
                   "The Paired Samples T-Test showed that the pre-post difference was statistically significant (p=",
                   round(paired_samples_t_test$p.value,3),").")
  } else {
    message("--> The average pre-test score was ",round(descriptive_statistics$mean[1],2),
                   " and the average post-test score was ",round(descriptive_statistics$mean[2],2),". ",
                   "The Paired Samples T-Test showed that the pre-post difference was NOT statistically significant (p=",
                   round(paired_samples_t_test$p.value,3),").")
  }
  # Check the condition of data to determine the need to run a non-parametric analysis
  message("", sep="\n")
  message("_______________________________________")
  sample_size <- nrow(treat_data_merged)
  shapiro_wilk_test <- shapiro.test(treat_data_merged$avg_diff)
  if (sample_size < 15 & shapiro_wilk_test$p.value < 0.05) {
    message("## The sample size of ", sample_size, " is small, and the Shapiro-Wilk test result shows a violation of normality assumption (p=",shapiro_wilk_test$p.value,"). Although the t-test is robust to a small sample size and a violation of the normality assumption, you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size < 15 & shapiro_wilk_test$p.value > 0.05) {
    message("## The sample size of ", sample_size, " may be too small to safely interpret the parametric t-test results above. Although the t-test is known to be robust to a small sample size you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size >= 15 & shapiro_wilk_test$p.value<0.05) {
    message("## The Shapiro-Wilk test result shows a violation of normality assumption (p=",round(shapiro_wilk_test$p.value,3),"). Although the t-test is known to be robust to a violation of the normality assumption, you may want to refer to the Wilcoxon signed rank test results below to be safe.")
  }
  if (sample_size < 15 | shapiro_wilk_test$p.value < 0.05) {
    message("=====================================")
    message("Wilcoxon Signed Rank Sum Test Results")
    print(wilcoxon_signed_rank_test)
    message("## A sample interpretation of the Wilcoxon signed rank test results above:")
    if (wilcoxon_signed_rank_test$p.value < 0.05) {
      message("--> The Wilcoxon signed rank test results above show that the pre-post difference was statistically significant (p=",
                     round(wilcoxon_signed_rank_test$p.value,3),").")
    } else {
      message("--> The Wilcoxon signed rank test results above show that the pre-post difference was NOT statistically significant (p=",
                     round(wilcoxon_signed_rank_test$p.value,3),").")
    }
  }
  message("", sep="\n")

  # Rename variables for return

  # Return values for testing the function
  out <- tibble::lst(
    n_students_deleted,
    shapiro_wilk_test,
    normal_qq_plot,
    descriptive_statistics,
    boxplots,
    paired_samples_t_test,
    wilcoxon_signed_rank_test,
  )
  return(invisible(out))

}

###############################################################################
#' Independent Samples Data Analysis
#'
#' This function automatically cleans the datasets (e.g., converting missing
#' values to "0), binds treatment-control group datasets, check assumptions,
#' and then runs the Independent Samples T-test (parametric) and Mann–Whitney U
#' test (nonparametric) to help you examine the difference between the groups.
#' R scripts and their outputs are as follows (just pay attention to the outputs
#' since the codes are automatically run back-end by the function).
#'
#' @param treat_csv_data This function requires a csv file with treatment group
#' data. Its name (e.g., "data_treat_post.csv") can be passed as an argument.
#' Make sure to set the folder with the data file(s) as the working directory.
#' @param ctrl_csv_data This function requires a csv file with control group
#' data. Its name (e.g., "data_ctrl_post.csv") can be passed as an argument.
#' Make sure to set the folder with the data file(s) as the working directory.
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' independent_samples(treat_csv_data =
#'          system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
#'          ctrl_csv_data =
#'          system.file("extdata", "data_ctrl_post.csv", package = "DBERlibR"),
#'          m_cutoff = 0.15)
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{descriptive_statistics}: Descriptive statistics
#'  \item \code{boxplots}: Boxplots - visual presentation of the descriptive
#'  statistics
#'  \item \code{shapiro_wilk_test}: Shapiro-Wilk test results to determine
#'  normality
#'  \item \code{normal_qq_plot}: The normal q-q plot to visually inspect the
#'  normality
#'  \item \code{independent_samples_t_test_equal}: Results of the independent
#'  samples t-test with equal variances assumed
#'  \item \code{independent_samples_t_test_unequal}: Results of the independent
#'  samples t-test with unequal variances assumed
#'  \item \code{mann_whitney_u_test}: Results of the Mann-Whitney U test
#' }
#'
#' @export

independent_samples <- function(treat_csv_data, ctrl_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- datagroup <- avg_score_post <- NULL

  # Read the treatment and control group post-test datasets
  data_treat_post<- read_csv(treat_csv_data, show_col_types = FALSE)
  data_ctrl_post<- read_csv(ctrl_csv_data, show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
                     mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL
  message("", sep="\n")

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
                    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL
  message("", sep="\n")

  n_students_deleted <- data.frame(c(n_students_deleted_treat, n_students_deleted_ctrl))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_data", "ctrl_data")


  #----------------------------------------------------------------

  # Clean data (Replace missing values and all other code for missing values with "0")
  n_col <- ncol(data_treat_post)
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
                     mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
                     mutate(datagroup=2)
  data_ctrl_post <- data_ctrl_post %>%
                    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
                    mutate(datagroup=1)

  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)

  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)

  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(1,2),labels=c("Control", "Treatment"))

  # Get descriptive statistics
  descriptive_statistics <- group_data_binded %>%
                 group_by(datagroup) %>%
                 get_summary_stats(avg_score_post, type = "mean_sd")

  boxplots <- ggboxplot(group_data_binded, x="datagroup", y="avg_score_post", add="point", title="Independent Samples - Boxplots")

  # Check Assumptions
  # No Outliers
  Outliers <- group_data_binded %>%
              group_by(datagroup) %>%
              identify_outliers(avg_score_post)

  # Normality
  shapiro_wilk_test <- group_data_binded %>%
                 group_by(datagroup) %>%
                 shapiro_test(avg_score_post)
  normal_qq_plot <- ggqqplot(group_data_binded, x = "avg_score_post", facet.by = "datagroup", title="Normal Q-Q Plot")

  # Homogeneity of the variance
  equalvariance <- group_data_binded %>%
    levene_test(avg_score_post ~ datagroup)

  # Run independent samples t-test (two-sided)
  independent_samples_t_test_equal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
  independent_samples_t_test_unequal <- t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

  # Run Mann-Whitney U Test
  mann_whitney_u_test <- wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)

  ####### Present the resutls in the R console for the convenience of the user

  message("==============================")
  message("The number of students deleted")
  print(n_students_deleted)
  message("--> ",n_students_deleted_treat, " student(s) from the treatment group data and ", n_students_deleted_ctrl, " student(s) from the control group data have been deleted since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("======================")
  message("Descriptive Statistics")
  print(descriptive_statistics)
  message("Refer to the boxplots in the 'Plots' panel for visual inspection of the descriptive statistics.", sep="\n")
  print(boxplots)
  message("====================")
  message("Checking Assumptions", sep="\n")
  message("The Equality of Variances")
  print(equalvariance)
  if (equalvariance$p > 0.05) {
    message("## Interpretation: the assumption of equality of variances has been met (p>0.05)", sep="\n")
  } else {
    message("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05)", sep="\n")
  }
  message("The Nomality by Groups")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p[2] >0.05 & shapiro_wilk_test$p[1] >0.05) {
    message("## Interpretation: the assumption of normality has been met (p>0.05 for each group).", sep="\n")
  } else {
    message("## Interpretation: the assumption of normality has NOT been met (p<0.05 for at least one of the groups).", sep="\n")
  }
  message("Refer to the Q-Q Plots in the 'Plots' panel for visual inspection of the normality.", sep="\n")
  print(normal_qq_plot)

  message("===============================================")
  message("Independent Samples T-Test Results (Parametric)")
  if (equalvariance$p > 0.05) {
    print(independent_samples_t_test_equal)
    message("## A sample interpretation of the t-test results above:")
    if (independent_samples_t_test_equal$p.value < 0.05) {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
                 " and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
                 "The independent samples t-test results show that the pre-post difference is statistically significant
                 (p=", round(independent_samples_t_test_equal$p.value,3),").")
    } else {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
                 ", and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
                 "The independent samples t-test results show that the group difference is NOT statistically significant (p=",
                 round(independent_samples_t_test_equal$p.value,3),").")
    }
  } else {
    print(independent_samples_t_test_unequal)
    message("## A sample interpretation of the t-test results above:")
    if (independent_samples_t_test_unequal$p.value < 0.05) {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
                 " and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
                 "The independent samples t-test results show that the pre-post difference is statistically significant
                 (p=", round(independent_samples_t_test_unequal$p.value,3),").")
    } else {
      message("--> The treatment group's average score was ",round(treat_post_average$mean,2),
                 ", and the control group's average score was ",round(ctrl_post_average$mean,2),". ",
                 "The independent samples t-test results show that the group difference is NOT statistically significant (p=",
                 round(independent_samples_t_test_unequal$p.value,3),").")
    }
  }
  sample_size_t <- nrow(data_treat_post)
  sample_size_c <- nrow(data_ctrl_post)
  if (sample_size_t < 15 | sample_size_c < 15 | shapiro_wilk_test$p[2] < 0.05 | shapiro_wilk_test$p[1] < 0.05) {
    message("## Either a sample size is too small or the data violates the assumption of eval variances (refer to the descriptive statistics and the Shapiro-Wilk test resutls). Although the t-test is known to be robust to a small sample size or violation of the normality assumption, you may want to refer to the Mann-Whitney U (Wilcoxon Rank Sum) test results below to be safe.")
    message("===============================================================")
    message("Mann-Whitney U (Wilcoxon Rank Sum) Test Results (Nonparametric)", sep="\n")
    print(mann_whitney_u_test)
    message("## A sample interpretation of the Mann-Whitney U (Wilcoxon Rank Sum) test results above:")
    if (mann_whitney_u_test$p.value < 0.05) {
      message("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was statistically significant (p=",
               round(mann_whitney_u_test$p.value,3),").")
    } else {
      message("--> The the Mann-Whitney U (Wilcoxon Rank Sum) test results above show that the pre-post difference was NOT statistically significant (p=",
               round(mann_whitney_u_test$p.value,3),").")
    }
  }

  # Return values
  out <- tibble::lst(
    n_students_deleted,
    descriptive_statistics,
    boxplots,
    shapiro_wilk_test,
    normal_qq_plot,
    independent_samples_t_test_equal,
    independent_samples_t_test_unequal,
    mann_whitney_u_test,
  )
  return(invisible(out))
}

###############################################################################
#' One-way ANCOVA
#'
#' This function automatically merges pre-post data sets, binds
#' treatment-control data sets, checks assumptions of one-way ANCOVA,
#' and then runs the main One-way ANCOVA all at once. Please make sure to name
#' data files accurately (i.e., "data_treat_pre.csv", "data_treat_post.csv",
#' "data_ctrl_pre.csv"," data_ctrl_post.csv") and have them saved
#' in the working directory.
#'
#' @param treat_pre_csv_data This function requires a csv file with treatment
#' group's pre-test data. Its name (e.g., "data_treat_pre.csv") can be passed
#' as an argument. Make sure to set the folder with the data file(s) as the
#' working directory.
#' @param treat_post_csv_data This function requires a csv file with treatment
#' group's post-test data. Its name (e.g., "data_treat_post.csv") can be passed
#' as an argument. Make sure to set the folder with the data file(s) as the
#' working directory.
#' @param ctrl_pre_csv_data This function requires a csv file with control
#' group's pre-test. Its name (e.g., "data_ctrl_pre.csv") can be passed as an
#' argument. Make sure to set the folder with the data file(s) as the working
#' directory.
#' @param ctrl_post_csv_data This function requires a csv file with control
#' group's post-test data. Its name (e.g., "data_ctrl_post.csv") can be passed
#' as an argument. Make sure to set the folder with the data file(s) as the
#' working directory.
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' one_way_ancova(treat_pre_csv_data =
#'          system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
#'          treat_post_csv_data =
#'          system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
#'          ctrl_pre_csv_data =
#'          system.file("extdata", "data_ctrl_pre.csv", package = "DBERlibR"),
#'          ctrl_post_csv_data =
#'          system.file("extdata", "data_ctrl_post.csv", package = "DBERlibR"),
#'          m_cutoff = 0.15)
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{pre_descriptive_statistics}: Pre-test data descriptive statistics
#'  \item \code{post_descriptive_statistics}: Post-test data descriptive statistics
#'  \item \code{boxplots}: Boxplots - visual presentation of the descriptive
#'  statistics
#'  \item \code{scatter_plot}: Scatter plots to test the linearity
#'  statistics
#'  \item \code{shapiro_wilk_test}: Shapiro-Wilk test results to determine
#'  normality of residuals
#'  \item \code{normal_qq_plot}: The normal q-q plot to visually inspect the
#'  normality of residuals
#'  \item \code{levene_test}: Test homogeneity of variances
#'  \item \code{independent_samples_t_test_equal}: Results of the independent
#'  samples t-test with equal variances assumed
#'  \item \code{one_way_ancova}: Results of the one-way ANCOVA
#'  \item \code{estimated_marginal_means}: Estimated marginal means
#' }
#'
#' @export

one_way_ancova <- function(treat_pre_csv_data, treat_post_csv_data, ctrl_pre_csv_data, ctrl_post_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- datagroup <- avg_score_pre <- avg_score_post <- avg_score <- ..eq.label.. <- ..rr.label.. <- .hat <- .sigma <- .fitted <- .std.resid <- NULL

  # Read all data sets
  data_treat_pre <- read_csv(treat_pre_csv_data, show_col_types = FALSE)
  data_treat_post<- read_csv(treat_post_csv_data, show_col_types = FALSE)
  data_ctrl_pre <- read_csv(ctrl_pre_csv_data, show_col_types = FALSE)
  data_ctrl_post<- read_csv(ctrl_post_csv_data, show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_treat_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
    mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_treat_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL

  # Deleting students with too many skipped answers: data_ctrl_pre.csv-----------------
  nrow_all <- nrow(data_ctrl_pre)
  n <- as.numeric(length(data_ctrl_pre[,-1]))
  data_ctrl_pre <- data_ctrl_pre %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_pre))/n,3))

  data_ctrl_pre <- subset(data_ctrl_pre, m_rate < m_cutoff)
  nrow_subset <- nrow(data_ctrl_pre)
  n_students_deleted_ctrl_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_ctrl_post.csv-----------------
  nrow_all <- nrow(data_ctrl_post)
  n <- as.numeric(length(data_ctrl_post[,-1]))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(m_rate = round(rowSums(is.na(data_ctrl_post))/n,3))

  data_ctrl_post <- subset(data_ctrl_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_ctrl_post)
  n_students_deleted_ctrl_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_ctrl_post$m_rate=NULL

  n_students_deleted <- data.frame(c(n_students_deleted_treat_pre, n_students_deleted_treat_post, n_students_deleted_ctrl_pre, n_students_deleted_ctrl_post))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("treat_pre_data", "treat_post_data", "ctrl_pre_data", "ctrl_post_data")

  #----------------------------------------------------------------

  # Clean data (e.g., Replace skipped answers with "0")
  n_col <- ncol(data_treat_pre)
  for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
  data_treat_pre[is.na(data_treat_pre)]= 0
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_ctrl_pre[,i][data_ctrl_pre[,i]!=1]=0}
  data_ctrl_pre[is.na(data_ctrl_pre)]= 0
  for (i in 2:n_col) {data_ctrl_post[,i][data_ctrl_post[,i]!=1]=0}
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Creat data sets for descriptive statistics
  data_c_pre <- data_ctrl_pre %>%
    mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
  data_c_post <- data_ctrl_post %>%
    mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
  data_t_pre <- data_treat_pre %>%
    mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
  data_t_post <- data_treat_post %>%
    mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

  # Change column names with their origin'sir
  colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>%
    mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
  data_ctrl_pre <- data_ctrl_pre %>%
    mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

  # Merge pre/post data and generate group code (treat=1, control=0)
  treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
  treat_data_merged <- treat_data_merged %>%
    mutate(datagroup=1)

  ctrl_data_merged <- merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
  names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
  ctrl_data_merged <- ctrl_data_merged %>%
    mutate(datagroup=0)

  # Bind treat/control data
  full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

  # Name datagroup -> 0=control, 1=treatment
  full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

  # Descriptive Statistics
  pre_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
    summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
  post_descriptive_statistics <- group_by (full_data_binded, datagroup) %>%
    summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

  data_c_pre <- data_c_pre %>%
    mutate(datagroup=1)
  data_c_post <- data_c_post %>%
    mutate(datagroup=2)
  data_t_pre <- data_t_pre %>%
    mutate(datagroup=3)
  data_t_post <- data_t_post %>%
    mutate(datagroup=4)

  df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
  df_Long <- df_Long %>%
    select(avg_score, datagroup)
  names(df_Long) <- c("Average_Score", "Group")
  # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
  df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

  boxplots <- ggboxplot(df_Long, x="Group", y="Average_Score", add="point", title="Boxplots by Group" )

  # Check assumptions
  # Check linearity (visual inspection)
  scatter_plot <- ggscatter(full_data_binded, x="avg_score_pre",y= "avg_score_post", color = "datagroup",add = "reg.line" ) +
    stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = datagroup)) +
    ggtitle("scatter Plot to Check Linearity") + theme_minimal() +
    xlab("Pre-test Scores") + ylab("Post-test Scores")

  # Check homogeneity of regression slopes
  # line_slopes <- lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded)
  line_slopes <- full_data_binded %>% anova_test(avg_score_post ~ datagroup*avg_score_pre)

  # Inspect the model diagnostic metrics
  model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
  model_metrics <- augment(model) %>%
    dplyr::select(-.hat, -.sigma, -.fitted)
  # print(head(model_metrics, 3))

  # Check normality of Residuals
  norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
  shapiro_wilk_test <- shapiro.test(norm.all.aov$residuals)
  Residuals <- norm.all.aov$residuals
  normal_qq_plot <- ggqqplot(residuals(norm.all.aov),title = "Normal Q-Q Plot of Residuals")

  # Check homogeneity of variances
  # levene_test <- model_metrics %>% levene_test(.resid ~ as.factor(datagroup)) %>% as.data.frame()
  levene_test <- leveneTest(avg_score_post ~ datagroup, full_data_binded)

  # Check outlier
  outliers <- model_metrics %>%
    dplyr::filter(abs(.std.resid) > 3) %>%
    as.data.frame()

  # Run one-way ANCOVA
  one_way_ancova <- res.aov <- full_data_binded %>%
    rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre)

  # Display the adjusted means (a.k.a., estimated marginal means) for each group
  emms <- emmeans_test(full_data_binded,
                       avg_score_post ~ datagroup,
                       covariate = avg_score_pre,
                       p.adjust.method = "bonferroni",
                       conf.level=0.95,
                       detailed=TRUE
  )
  estimated_marginal_means <- get_emmeans(emms) %>%
    as.data.frame()

  ####### Present the results in the console panel for the convenience of users
  message("==============================")
  message("The number of students deleted")
  print(n_students_deleted)
  message("--> ",n_students_deleted_treat_pre, " student(s) from the treatment group pre-test data, ", n_students_deleted_treat_post, " student(s) from the treatment group post-test data, ",n_students_deleted_ctrl_pre, " student(s) from the control group pre-test data, and ", n_students_deleted_ctrl_post, " student(s) from the control group post-test data have been deleted since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("======================")
  message("Descriptive Statistics")
  message("Pre-test scores by group")
  print(pre_descriptive_statistics)
  message("-------------------------")
  message("Post-test scores by group")
  print(post_descriptive_statistics)
  message("Refer to the boxplots in the 'Plots' panel to visually inspect the descriptive statistics")
  print(boxplots)
  message("", sep="\n")
  message("===============================")
  message("Results of Checking Assumptions", sep="\n")
  message("# Linearity:")
  message("Refer to the scatter plot to check linearity in the 'Plots' panel. If two lines look almost paralleled, they can be interpreted as meeting the assumption of linearity.")
  plot(scatter_plot)
  message("## Interpretation: if you are seeing a liner relationship between the covariate (i.e., pre-test scores for this analysis) and dependent variable (i.e., post-test scores for this analysis) for both treatment and control group in the plot, then you can say this assumption has been met or the data has not violated this assumption of linearity. If your relationships are not linear, you have violated this assumption, and an ANCOVA is not a suitable analysis. However, you might be able to coax your data to have a linear relationship by transforming the covariate, and still be able to run an ANCOVA.")
  message("-------------------------")
  message("# Normality of Residuals:")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p.value > 0.05) {
    message("## Interpretation: the assumption of normality by group has been met (p>0.05).")
  } else {
    message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANCOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue)")
  }
  message("Refer to the histogram and the normal Q-Q plot in the 'Plots' panel to visually inspect the normality of residuals.")
  hist(Residuals)
  print(normal_qq_plot)
  message("---------------------------")
  message("# Homogeneity of Variances:")
  print(levene_test)
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    message("## Interpretation: the assumption of equality of error variances has been met (p>0.05).")
  } else {
    message("## Interpretation: the assumption of equality of error variances has NOT been met (p<0.05). You need to transform your dependent variable (post-test average scores) to see if you can remove the heterogeneity in your ANCOVA model.")
  }
  message("----------------------------------------")
  message("# Homogeneity of Regression Slopes:")
  print(line_slopes)
  if (line_slopes$p[3] > 0.05) {
    message("## Interpretation: there was homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was not statistically significant (p>0.05).")
  } else {
    message("## Interpretation: the data has violated the assumption of homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was statistically significant (p<0.05). You will not be able to undertake an ANCOVA analysis.")
  }
  if (nrow(outliers) != 0) {
    message("-----------")
    message("# Outliers:")
    print(outliers)
  } else {
    message("----------")
    message("# Outliers: No outlier has been found.")
  }
  message("", sep="\n")
  message("==================================")
  message("Results of the main One-way ANCOVA")
  print(res.aov)
  message("--------------------------")
  message("# Estimated Marginal Means")
  print(estimated_marginal_means)
  if (res.aov$p[1] < 0.05) {
    message("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be significant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means$emmean[2],2),", SE=,",round(estimated_marginal_means$se[2],2),") was significantly different from that of the control group (",round(estimated_marginal_means$emmean[1],2),", SE=,",round(estimated_marginal_means$se[1],2),").")
  } else {
    message("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be insignificant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(estimated_marginal_means$emmean[2],2),", SE=,",round(estimated_marginal_means$se[2],2),") was not significantly different from that of the control group (",round(estimated_marginal_means$emmean[1],2),", SE=,",round(estimated_marginal_means$se[1],2),").")
  }

  # Return values
  out <- tibble::lst(
    n_students_deleted,
    pre_descriptive_statistics,
    post_descriptive_statistics,
    boxplots,
    scatter_plot,
    line_slopes,
    shapiro_wilk_test,
    normal_qq_plot,
    levene_test,
    outliers,
    one_way_ancova,
    estimated_marginal_means,
  )
  return(invisible(out))

}

###############################################################################
#' One-way Repeated Measures ANOVA
#'
#' This function automatically merges pre, post, and post2 datasets,
#' and then runs the one-way repeated measures ANOVA with assumptions check
#' all at once. Please make sure to name data files accurately (i.e.,
#' “data_treat_pre.csv”, “data_treat_post.csv”, and “data_treat_post2.csv”)
#' and have them saved in the working directory.
#'
#' @param treat_pre_csv_data This function requires a csv file with treatment
#' group's pre-test data. Its name (e.g., "data_treat_pre.csv") can be passed
#' as an argument. Make sure to set the folder with the data file(s) as the
#' working directory.
#' @param treat_post_csv_data This function requires a csv file with treatment
#' group's post-test data. Its name (e.g., "data_treat_post.csv") can be passed
#' as an argument. Make sure to set the folder with the data file(s) as the
#' working directory.
#' @param treat_post2_csv_data This function requires a csv file with treatment
#' group's post2-test. Its name (e.g., "data_treat_post2.csv") can be passed as
#' an argument. Make sure to set the folder with the data file(s) as the working
#' directory.
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' one_way_repeated_anova(treat_pre_csv_data =
#'        system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
#'        treat_post_csv_data =
#'        system.file("extdata", "data_treat_post.csv", package = "DBERlibR"),
#'        treat_post2_csv_data =
#'        system.file("extdata", "data_treat_post2.csv", package = "DBERlibR"),
#'        m_cutoff = 0.15)
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{descriptive_statistics}: Descriptive statistics
#'  \item \code{boxplots}: Boxplots - visual presentation of the descriptive
#'  statistics
#'  \item \code{shapiro_wilk_test}: Shapiro-Wilk test results to determine
#'  normality of residuals
#'  \item \code{normal_qq_plot}: The normal q-q plot to visually inspect the
#'  normality of residuals
#'  \item \code{one_way_repeated_anova}: Results of the one-way repeated
#'  measures ANOVA
#'  \item \code{one_way_repeated_anova_pwc}: Pairwise t-test results for the
#'  one-way repeated measures ANOVA
#'  \item \code{friedman_test}: Results of the friedman test
#'  \item \code{friedman_pwc}: Pairwise t-test results for the Friedman test
#' }
#'
#' @export

one_way_repeated_anova <- function(treat_pre_csv_data, treat_post_csv_data, treat_post2_csv_data, m_cutoff = 0.15) {

  # binding for global variable
  m_rate <- Time <- Score <- id <- NULL

  # Read all datasets
  data_treat_pre <- read_csv(treat_pre_csv_data, show_col_types = FALSE)
  data_treat_post<- read_csv(treat_post_csv_data, show_col_types = FALSE)
  data_treat_post2 <- read_csv(treat_post2_csv_data, show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_treat_pre.csv-----------------
  nrow_all <- nrow(data_treat_pre)
  n <- as.numeric(length(data_treat_pre[,-1]))
  data_treat_pre <- data_treat_pre %>%
                    mutate(m_rate = round(rowSums(is.na(data_treat_pre))/n,3))

  data_treat_pre <- subset(data_treat_pre, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_pre)
  n_students_deleted_pre <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_pre$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post.csv-----------------
  nrow_all <- nrow(data_treat_post)
  n <- as.numeric(length(data_treat_post[,-1]))
  data_treat_post <- data_treat_post %>%
                     mutate(m_rate = round(rowSums(is.na(data_treat_post))/n,3))

  data_treat_post <- subset(data_treat_post, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_post)
  n_students_deleted_post <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post$m_rate=NULL

  # Deleting students with too many skipped answers: data_treat_post2.csv-----------------
  nrow_all <- nrow(data_treat_post2)
  n <- as.numeric(length(data_treat_post2[,-1]))
  data_treat_post2 <- data_treat_post2 %>%
                      mutate(m_rate = round(rowSums(is.na(data_treat_post2))/n,3))

  data_treat_post2 <- subset(data_treat_post2, m_rate < m_cutoff)
  nrow_subset <- nrow(data_treat_post2)
  n_students_deleted_post2 <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_treat_post2$m_rate=NULL

  n_students_deleted <- data.frame(c(n_students_deleted_pre, n_students_deleted_post, n_students_deleted_post2))
  colnames(n_students_deleted) <- c("n")
  rownames(n_students_deleted) <- c("pre_data", "post_data", "post2_data")

  #----------------------------------------------------------------

  # Clean data (e.g., Replace skipped answers with "0")
  n_col <- ncol(data_treat_pre)
  for (i in 2:n_col) {data_treat_pre[,i][data_treat_pre[,i]!=1]=0}
  data_treat_pre[is.na(data_treat_pre)]= 0
  for (i in 2:n_col) {data_treat_post[,i][data_treat_post[,i]!=1]=0}
  data_treat_post[is.na(data_treat_post)]= 0
  for (i in 2:n_col) {data_treat_post2[,i][data_treat_post2[,i]!=1]=0}
  data_treat_post2[is.na(data_treat_post2)]= 0

  # Change column names with their origin'sir
  colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_treat_post2) <- paste( colnames(data_treat_post2), "post2", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>%
                    mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>%
                     mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
  data_treat_post2 <- data_treat_post2 %>%
                      mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

  # Merge pre/post/post2 data
  data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

  # Convert Data Frame to a Long Format & Define the Variable
  avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
  df_Long <- melt(avg_score_df)
  names(df_Long) <- c("id", "Time", "Score")
  # Name Time(group) -> 1=Pre, 2=post, 3=Post2
  df_Long$id <- factor(df_Long$id)
  df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "post", "Post2"))

  # Descriptive Statistics
  descriptive_statistics <- df_Long %>%
                 group_by(Time) %>%
                 get_summary_stats(Score, type="common")
  boxplots <- ggboxplot(df_Long, x="Time", y="Score", add="point", title="Boxplots")

  # Check Outliers
  outliers <- df_Long %>%
              group_by(Time) %>%
              identify_outliers(Score)

  # Check normality of Residuals
  res.aov <- aov(Score~Time, data=df_Long)
  shapiro_wilk_test <- shapiro.test(resid(res.aov))
  Residuals <- res.aov$residuals
  normal_qq_plot <- ggqqplot(residuals(res.aov),title = "Normal Q-Q Plots of Residuals")

  # One-way Repeated Measures ANOVA
  res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
  one_way_repeated_anova <- result <- get_anova_table(res.aov)
  repeated_anova_p.value <- result$p

  # Pairwise Comparisons
  one_way_repeated_anova_pwc <- pwc <- df_Long %>%
         pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

  # Friedman Test (Non-parametric)
  friedman_test <- friedman.test(avg_score_df)

  # Pairwise Comparisons - Friedman
  friedman_pwc <- df_Long %>%
                  wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")

  ####### Present the results in the console for the convenience of users
  message("==============================")
  message("The number of students deleted")
  print(n_students_deleted)
  message("--> ",n_students_deleted_pre, " student(s) from the pre-test data, ", n_students_deleted_post, " student(s) from the post-test data, and ",n_students_deleted_post2, " student(s) from the post2-test data have been deleted since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("======================================")
  message("Descriptive Statistics: Average Scores")
  print(descriptive_statistics)
  message("Refer to the boxplots in the 'Plots' panel to visually inspect the descriptive statistics.")
  print(boxplots)
  message("", sep="\n")
  message("==============================")
  message("Results of Testing Assumptions", sep="\n")
  message("-----------")
  message("# Outliers:")
  print(outliers)
  if (outliers$is.extreme[1]==FALSE & outliers$is.extreme[2]==FALSE) {
    message("## Interpretation: No extreme outlier was identified in your data.", sep="\n")
  } else {
    message("## Interpretation: At least one extreme outlier was identified in your data, probably due to data entry errors, mesurement errors, or unusual values. you need to check the outlier(s) to understand them.You can still include the outlier(s) in the analysis if you don't believe the result will be substantially affected. This can be evaluated by comparing the result of the ANOVA with and without the outlier(s).", sep="\n")
  }

  message("---------------------------")
  message("# Normality:")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p>0.05) {
    message("--> Interpretation: the average test score was normally distributed at each time point, as assessed by Shapiro-Wilk test (p>0.05).", sep="\n")
  } else {
    message("--> Interpretation: the assumption of normality has NOT been met; the average score was not normally distributed at least at one time point. Although the repeated measures ANOVA is known to be robust to a violation of the assumption of normality, you may want/need to refer to the result of the Friedman test, which is a non-parametric version of one-way repeated measures ANOVA in you report.", sep="\n")
  }
  message("If the sample size is greater than 50, it would be better refer to the normal Q-Q plot displayed in the 'Plots' panel to visually inspect the normality. This is because the Shapiro-Wilk test becomes very sensitive to a minor deviation from normality at a larger sample size (>50 in this case).", sep="\n")
  hist(Residuals)
  print(normal_qq_plot)
  message("--> Interpretation: if all the points fall in the plots above approximately along the reference line, you can assume normality.", sep="\n")

  message("-------------")
  message("# Sphericity:")
  message("--> The assumption of sphericity has been checked during the computation of the ANOVA test (the Mauchly's test has been internally run to assess the sphericity assumption). Then, the Greenhouse-Geisser sphericity correction has been automatically applied to factors violating the sphericity of assumption.", sep="\n")

  message("===============================================================")
  message("Result of the main One-way Repeated Measures ANOVA (Parametric)")
  print(one_way_repeated_anova)
  if (one_way_repeated_anova$p<0.001) {
    message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.001, eta2(g)=",result$ges,".")
  } else if (one_way_repeated_anova$p<0.01) {
    message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.01, eta2(g)=",result$ges,".")
  } else if (one_way_repeated_anova$p<0.05) {
    message("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.05, eta2(g)=",result$ges,".")
  } else {
    message("--> Interpretation: The average test score at different time points of the intervention are not statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p>0.05, eta2(g)=",result$ges,".")
  }
  message("", sep="\n")
  message("--------------------")
  message("Pairwise Comparisons")
  print(pwc)
  # Between Pre and Post
  if (pwc$p.adj[2]<0.001) {
    if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.001).")
    } else {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
    }
  } else if (pwc$p.adj[2]<0.01) {
    if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.01).")
    } else {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
    }
  } else if (pwc$p.adj[2]<0.05) {
    if((descriptive_statistics$mean[2]-descriptive_statistics$mean[1])>0) {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.05).")
    } else {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
    }
  } else {
      message("--> Interpretation for 1: The average pre-test score (",descriptive_statistics$mean[1],") and the average post-test score (",descriptive_statistics$mean[2],") are not significantly different (p.adj>0.05).")
  }
  # Between Post and Post2
  if (pwc$p.adj[1]<0.001) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.001).")
    } else {
    message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.001).")
    }
  } else if (pwc$p.adj[1]<0.01) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.01).")
    } else {
      message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.01).")
    }
  } else if (pwc$p.adj[1]<0.05) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.05).")
    } else {
      message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.05).")
    }
  } else {
    message("--> Interpretation for 2: The average post-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are not significantly different (p.adj>0.05).")
  }
  # Between Pre and Post2
  if (pwc$p.adj[3]<0.001) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.001).")
    } else {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.001).")
    }
  } else if (pwc$p.adj[3]<0.01) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.01).")
    } else {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average Post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.01).")
    }
  } else if (pwc$p.adj[3]<0.05) {
    if((descriptive_statistics$mean[3]-descriptive_statistics$mean[2])>0) {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.05).")
    } else {
      message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[2],") and the average post2-test score (",descriptive_statistics$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.05).")
    }
  } else {
    message("--> Interpretation for 3: The average pre-test score (",descriptive_statistics$mean[1],") and the average post2-test score (",descriptive_statistics$mean[3],") are not significantly different (p.adj>0.05).")
  }

  # Display the result of the Friedman test only when the assumption of normality is violated
  if (shapiro_wilk_test$p<0.05) {
    message("---------------------------------------------")
    message("As the result of the Shapiro-Wilk normality test show above, the assumption of normality of residuals has been violated (i.e., p-value from the Shapiro-Wilk test is less than 0.05). Although the repeated measures ANOVA is fairly 'robust' to violations of normality, you may want/need to use the Friedman test results presented below:")
    message("", sep="\n")
    message("===============================================")
    message("Friedman Rank Sum Test - Result (Nonparametric)")
    print(friedman_test)
    if (friedman_test$p.value<0.001) {
      message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.001).")
    } else if (friedman_test$p.value<0.01) {
      message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.01).")
    } else if (friedman_test$p.value<0.05) {
      message("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.05).")
    } else {
      message("--> Interpretation: the median test score is not significantly different at the different time points during the intervention (p>0.05).")
    }
    message("", sep="\n")
    message("---------------------------------------------")
    message("Friedman Rank Sum Test - Pairwise Comparisons")
    print(friedman_pwc)
    # Between Pre and post
    if (friedman_pwc$p.adj[2]<0.001) {
      if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.001).")
      } else {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
      }
    } else if (friedman_pwc$p.adj[2]<0.01) {
      if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.01).")
      } else {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
      }
    } else if (friedman_pwc$p.adj[2]<0.05) {
      if((descriptive_statistics$median[2]-descriptive_statistics$median[1])>0) {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.05).")
      } else {
        message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
      }
    } else {
      message("--> Interpretation for 1: the median pre-test score (",descriptive_statistics$median[1],") and the median post-test score (",descriptive_statistics$median[2],") are not significantly different (p.adj>0.05).")
    }
    # Between post and Post2
    if (friedman_pwc$p.adj[1]<0.001) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.001).")
      } else {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.001).")
      }
    } else if (friedman_pwc$p.adj[1]<0.01) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.01).")
      } else {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.01).")
      }
    } else if (friedman_pwc$p.adj[1]<0.05) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.05).")
      } else {
        message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.05).")
      }
    } else {
      message("--> Interpretation for 2: the median post-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are not significantly different (p.adj>0.05).")
    }
    # Between Pre and Post2
    if (friedman_pwc$p.adj[3]<0.001) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.001).")
      } else {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.001).")
      }
    } else if (friedman_pwc$p.adj[3]<0.01) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.01).")
      } else {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.01).")
      }
    } else if (friedman_pwc$p.adj[3]<0.05) {
      if((descriptive_statistics$median[3]-descriptive_statistics$median[2])>0) {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.05).")
      } else {
        message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[2],") and the median post2-test score (",descriptive_statistics$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.05).")
      }
    } else {
      message("--> Interpretation for 3: the median pre-test score (",descriptive_statistics$median[1],") and the median post2-test score (",descriptive_statistics$median[3],") are not significantly different (p.adj>0.05).")
    }
  }

  # Return values for testing the function
  out <- tibble::lst(
    n_students_deleted,
    descriptive_statistics,
    boxplots,
    outliers,
    shapiro_wilk_test,
    normal_qq_plot,
    one_way_repeated_anova,
    one_way_repeated_anova_pwc,
    friedman_test,
    friedman_pwc,
  )
  return(invisible(out))

}

###############################################################################
#' Demographic Group Differences
#'
#' This function automatically combines demographic variables to a dataset,
#' and runs the analysis of variance (ANOVA) with assumptions check to examine
#' demographic sub-group differences all at once. Please make sure to name data
#' files accurately and have them saved in the working directory.
#'
#' @param score_csv_data This function requires a csv data file. Its name
#' (e.g., "data_treat_pre.csv") can be passed as an argument. Make sure to set
#' the folder with the data file(s) as the working directory.
#' @param group_csv_data This function requires a csv data file. Its name
#' (e.g., "demographic_data.csv") can be passed as an argument. Make sure to set
#' the folder with the data file(s) as the working directory.
#' @param group_name This function requires a group name as indicated in the csv
#' data file (e.g., "gender", "grade")
#' @param m_cutoff This package will treat skipped answers as incorrect.
#' However, too many skipped answers may skew the results of the data analysis.
#' User can can provide a cutoff for the proportion of skipped answers.
#' For example, if the user enters 0.1, students who skipped more than
#' 10 percent of the answers will be excluded from the data analysis to prevent
#' skewed results. The default of 0.15 is commonly applied as a rule of thumb.
#'
#' @examples
#' # Run the following codes directly in the console panel. The plots
#' # generated through the link above may be displaced depending on the screen
#' # resolution.
#' demo_group_diff(score_csv_data =
#'         system.file("extdata", "data_treat_pre.csv", package = "DBERlibR"),
#'         group_csv_data =
#'         system.file("extdata", "demographic_data.csv", package = "DBERlibR"),
#'         m_cutoff = 0.15,
#'         group_name = "grade")
#'
#' @returns This function returns a \code{tibble()} including the following
#' information:
#' \itemize{
#'  \item \code{n_students_deleted}: Number of students deleted from the data
#'  for analysis based on the percentage obtained via the argument of m_cutoff
#'  \item \code{descriptive_statistics}: Descriptive statistics
#'  \item \code{boxplots}: Boxplots - visual presentation of the descriptive
#'  statistics
#'  \item \code{shapiro_wilk_test}: Shapiro-Wilk test results to determine
#'  normality of residuals
#'  \item \code{normal_qq_plot}: The normal q-q plot to visually inspect the
#'  normality of residuals
#'  \item \code{levene_test}: Test homogeneity of variances
#'  \item \code{one_way_anova}: Results of the one-way anova with equal
#'  variances assumed
#'  \item \code{one_way_anova_pwc}: Pairwise t-test results for the
#'  one-way ANOVA with equal variances assumed
#'  \item \code{welch_anova_test}: Results of the one-way ANOVA with unequal
#'  variance
#'  \item \code{games_howell_test}: Pairwise t-test results for the
#'  one-way ANOVA with unequal variances assumed
#'  \item \code{kruskal_wallis_test}: Results of the Kruskal-Wallis test (non-
#'  parametric version of the one-way ANOVA)
#'  \item \code{kruskal_wallis_test_pwc}: Pairwise t-test results for the
#'  Kruskal-Wallis test
#' }
#'
#' @export

demo_group_diff <- function(score_csv_data, group_csv_data, m_cutoff = 0.15, group_name) {

  # binding for global variable
  m_rate <- all_of <- group <- average_score <- NULL

  # Reading
  data_original <- read_csv(score_csv_data,col_types = cols())
  n_col <- ncol(data_original)
  demographic_data <- read_csv(group_csv_data, show_col_types = FALSE)

  # Deleting students with too many skipped answers: data_original.csv-----------------
  nrow_all <- nrow(data_original)
  n <- as.numeric(length(data_original[,-1]))
  data_original <- data_original %>%
                   mutate(m_rate = round(rowSums(is.na(data_original))/n,3))

  data_original <- subset(data_original, m_rate < m_cutoff)
  nrow_subset <- nrow(data_original)
  n_students_deleted <- nrow_all-nrow_subset

  # Delete the column of "m_rate" to exclude from the analysis down the road.
  data_original$m_rate=NULL
  message("", sep="\n")
  #----------------------------------------------------------------

  # Clean data (e.g., Replace skipped answers with "0")
  n_col <- ncol(data_original)
  for (i in 2:n_col) {data_original[,i][data_original[,i]!=1]=0}
  data_original[is.na(data_original)]= 0

  # Calculate average scores
  data_original <- data_original %>%
                   mutate(avg_score = round(rowMeans(data_original[,-1]),3))

  # Merge test data and demographic data
  data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

  data_original <- select(data_original, c('avg_score', all_of(group_name)))
  names(data_original) <- c("average_score", "group")
  data_original <- data_original[complete.cases(data_original),]

  # Descriptive statistics
  descriptive_statistics <- data_original %>%
                 group_by(group) %>%
                 get_summary_stats(average_score, type = "mean_sd")
  boxplots <- ggboxplot(data_original, x = "group", y = "average_score", add="point", title="Boxplots by Demographic Sub-group")

  # Check Outliers
  outliers <- data_original %>%
              group_by(group) %>%
              identify_outliers(average_score)

  # Conduct one-way ANOVA and pairwise sub-group comparisons
  one_way_anova <- aov(average_score ~ factor(group), data=data_original)
  one_way_anova_pwc <- TukeyHSD(one_way_anova)

  welch_anova_test <- data_original %>%
    welch_anova_test(average_score ~ group)
  games_howell_test <- data_original %>%
    games_howell_test(average_score ~ group)

  # Check normality of residuals
  shapiro_wilk_test <- shapiro.test(resid(one_way_anova))
  normal_qq_plot <- ggqqplot(residuals(one_way_anova),title = "Normal Q-Q Plot of Residuals")
  Residuals <- residuals(one_way_anova)

  # Check homogeneity of variances
  levene_test <- leveneTest(average_score ~ factor(group), data=data_original)

  ## Kruskal-Wallis Test: Non-parametric version of one-way ANOVA
  kruskal_wallis_test <- kruskal.test(average_score ~ group, data=data_original)
  suppressWarnings({
  kruskal_wallis_test_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group,
                                 p.adjust.method = "BH")
  })

  ####### Present the results in the console panel for the convenience of users
  message("==============================")
  message("The number of students deleted: ",n_students_deleted, " student(s) has(have) been deleted from the data since they have more than ",as.numeric(m_cutoff)*100,"% of skipped answers.")
  message("", sep="\n")
  message("======================")
  message("Descriptive Statistics")
  print(descriptive_statistics)
  message("Refer to the boxplot in the 'Plots' panel.")
  print(boxplots)
  message("", sep="\n")
  message("==============================")
  message("Results of Testing Assumptions", sep="\n")
  message("# Normality of Residuals:")
  print(shapiro_wilk_test)
  if (shapiro_wilk_test$p.value > 0.05) {
    message("## Interpretation: the assumption of normality by group has been met (p>0.05).", sep="\n")
  } else {
    message("## Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue).", sep="\n")
  }
  message("Refer to the histogram and the normal Q-Q plot in the 'Plots' panel to visually inspect the normality of residuals.", sep="\n")
  hist(Residuals)
  print(normal_qq_plot)

  message("-------------------------")
  message("Homogeneity of Variances:")
  print(levene_test)
  if (levene_test$`Pr(>F)`[1] > 0.05) {
    message("## Interpretation: the assumption of equality of variances has been met (p>0.05).", sep="\n")
  } else {
    message("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05).", sep="\n")
  }
  plot(one_way_anova,1)
  if (levene_test$`Pr(>F)`[1]>0.05) {
  message("===================================================================================")
  message("Results of One-way ANOVA: Group Difference(s) (Parametric: Equal variances assumed)")
  print(summary(one_way_anova))
  message("----------------------------------------------")
  message("Pairwide Comparisons (Equal variances assumed)")
  print(one_way_anova_pwc)
  } else {
    message("Since the assumption of equal variances has NOT been satisfied, the Welch one-way test and the Games-Howell post-hoc test results are presented below.")
    message("=====================================================================================")
    message("Results of One-way ANOVA: Group Difference(s) (Parametric: Unequal variances assumed)")
    print(summary(welch_anova_test))
    message("--------------------")
    message("Pairwide Comparisons (Unequal variances assumed)")
    print(games_howell_test)
  }
  if (shapiro_wilk_test$p.value < 0.05) {
    message("----------------------------------------------------------")
    message("As shown above in the the result of the Shapiro-Wilk test, the assumption of normality is violated. Although ANOVA is known to be robust to a violation of normality, you may want/need to use the Kruskal-Wallis test result presented below")
    message("===============================================")
    message("Results of Kruskal_Wallis Test (Non-parametric)")
    print(kruskal_wallis_test)
    print(kruskal_wallis_test_pwc)
  }

  # Return values for testing the function
  out <- tibble::lst(
    n_students_deleted,
    descriptive_statistics,
    boxplots,
    outliers,
    shapiro_wilk_test,
    normal_qq_plot,
    levene_test,
    one_way_anova,
    one_way_anova_pwc,
    welch_anova_test,
    games_howell_test,
    kruskal_wallis_test,
    kruskal_wallis_test_pwc,
  )

  return(invisible(out))

}





