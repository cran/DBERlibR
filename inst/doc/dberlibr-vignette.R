## ---- include = FALSE---------------------------------------------------------
  knitr::opts_chunk$set(
  collapse = TRUE,
  echo = TRUE,
  comment = "#>"
  )

## ----load_dependencies, message=FALSE, warning=FALSE, echo=FALSE--------------
  library(car)
  library(dplyr)
  library(emmeans)
  library(ggplot2)
  library(ggpubr)
  library(ggrepel)
  library(psych)
  library(readr)
  library(reshape)
  library(rstatix)
  library(tibble)

## ----load_DBERlibR------------------------------------------------------------
library(DBERlibR)

## ---- message=FALSE-----------------------------------------------------------
system.file("extdata", package = "DBERlibR")

## ----data_format, echo = FALSE, out.width = "100%", fig.cap = "Data File Format"----
  knitr::include_graphics("data_format.png")

## ----data_format_demo, echo = FALSE, out.width = "100%", fig.cap = "Demographic Data File Format"----
  knitr::include_graphics("data_format_demo.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------

  # Reading
  data_original <- read_csv("data_treat_pre.csv",col_types = cols())


  # Calculate average scores
  data_original[is.na(data_original)]= 0
  # colnames(data_original) <- paste( colnames(data_original), file_name, sep = "_")
  data_original <- data_original %>% mutate(avg_score = rowMeans(data_original[,-1]))

  # ggplots: difficulty index
  difficulty_data <- data_original[,c(-1)] %>% colMeans() %>% t() %>% as.data.frame() %>% t()
  difficulty_data <- reshape::melt(round(difficulty_data,2), id.vars=c("Q_id"))  %>% dplyr::select(-X2)
  colnames(difficulty_data) = c("q.number","difficulty_index")
  plot_difficulty<- ggplot(difficulty_data, aes(x= difficulty_index , y= reorder(q.number,difficulty_index))) +
    geom_point() +
    geom_vline(xintercept = 0.2, color ="red")+
    geom_hline(yintercept = difficulty_data$q.number[length(data_original)-1],color="blue") +
    ggtitle("Difficulty Plot")+
    xlab("Proportion")+ ylab("Question Item Number") +theme_minimal() + geom_text_repel(aes(label = round(difficulty_index,2)))

  # Create data subset: difficulty index lower than 0.2
  toodifficultitems <- subset(difficulty_data, difficulty_index < 0.2)
  toodifficultitems_nrow <- nrow(toodifficultitems)

  # ggplots: discrimination index
  discrimination_data <- cor(data_original[,-1]) %>% as.data.frame()
  plot_discrimination <- ggplot(discrimination_data, aes(x=avg_score , y= reorder (colnames(discrimination_data),avg_score))) + geom_point() +
    geom_vline(xintercept = 0.2, color ="red")+
    xlab("Relationship Coefficient") +
    ylab("Question Item Number") + ggtitle("Discrimination Plot") +
    theme_minimal() + geom_text_repel(aes(label = round(avg_score,2)))

  # Create data subset: discrimination index lower than 0.2
  qnumber <- colnames(discrimination_data)
  discrimination_index <- round(discrimination_data$avg_score,2)
  discrimination_df <- data.frame(qnumber,discrimination_index)
  nondiscriminantitems <- subset(discrimination_df, discrimination_index < 0.2)
  nondiscriminantitems_nrow <- nrow(nondiscriminantitems)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Print difficulty data results
  cat("Item Analysis Results - Difficulty", sep="\n")
  print(difficulty_data)

## ----plot_difficulty, echo = FALSE, out.width = "70%", fig.cap = "Difficulty Plot"----
  knitr::include_graphics("itemanalysis_difficulty_plot_treat_pre.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  cat("Refer to 'Difficulty Plot' in the 'Plots' panel.")
  if (toodifficultitems_nrow > 0) {
    cat("As seen in the difficulty plot, the following question items present a difficulty plot lower than:", sep="\n")
    print(toodifficultitems$Q_id)
  } else {
    cat("As seen in the difficulty plot, none of the difficulty indixes was found to be lower than 0.2.", sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------

    # Print discrimination data results
  cat("Item Analysis Results - Discrimination:", sep="\n")
  print(discrimination_df)

## ----plot_discrimination, echo = FALSE, out.width = "70%", fig.cap = "Discrimination Plot"----
  knitr::include_graphics("itemanalysis_discrimination_plot_treat_pre.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  cat("Refer to 'Discrimination Plot' in the 'Plots' panel", sep="\n")
  if (nondiscriminantitems_nrow > 0) {
    cat("As seen in the discrimination plot, the following question items present a discrimination index lower than 0.2:", sep="\n")
    print(nondiscriminantitems$qnumber)
  } else {
    cat("As seen in the discrimination plot, none of the discrimination indixes was found to be lower than 0.2", sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # read pre-post data sets (of the treatment group)
  data_treat_pre <- read_csv("data_treat_pre.csv")
  data_treat_post<- read_csv("data_treat_post.csv")
  n <- as.numeric(length(data_treat_pre[,-1]))
  n=n-1
  # replace skipped answers with "0"
  data_treat_pre[is.na(data_treat_pre)]= 0
  data_treat_post[is.na(data_treat_post)]= 0
  # change column names with their origin'sir
  colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  # calculate average scores
  data_treat_pre <- data_treat_pre %>% mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>% mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
  # merge pre/post data and generate group code (treat=1, control=0)
  treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
  treat_data_merged <- treat_data_merged %>% mutate(avg_diff=avg_score_post-avg_score_pre)
  Mean_Differences <- treat_data_merged$avg_diff

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # get descriptive statistics for the average of pre-test scores
  avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post)
  df_Long <- melt(avg_score_df)
  names(df_Long) <- c("id", "Time", "Score")
  # Name Time(group) -> 1=Pre, 2=Post
  df_Long$Time <- factor(df_Long$Time,levels=c(1,2),labels=c("Pre", "Post"))
  # Descriptive Statistics
  df_Long %>% group_by(Time) %>% get_summary_stats(Score, type="common")

## ----paired_samples_boxplots, echo = FALSE, out.width = "70%", fig.cap = "Paired Samples - Boxplots"----
  knitr::include_graphics("pairedsamples_boxplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Results of checking normality
  treat_data_merged %>% shapiro_test(avg_diff)

## ----paired_samples_histogram, echo = FALSE, out.width = "70%", fig.cap = "Paired Samples - Histogram"----
  knitr::include_graphics("pairedsamples_histogram.png")

## ----paired_samples_normal_QQ_plot, echo = FALSE, out.width = "70%", fig.cap = "Paired Samples - Normal Q-Q Plot"----
  knitr::include_graphics("pairedsamples_qqplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # run Paired Samples T-test (two-sided)
  t.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, mu=0, alt="two.sided", paired=T, conf.level=0.95)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # run Wilcoxon Signed Rank Test
  wilcox.test(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, paired=T, conf.level=0.95)

## ----paired_samples_mcnemar, echo = FALSE, out.width = "70%", fig.cap = "Paired Samples - Mean Differences - Individual Items"----
  knitr::include_graphics("pairedsamples_individual_items.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Read the treatment and control group post-test data sets
  data_treat_post<- read_csv("data_treat_post.csv")
  data_ctrl_post<- read_csv("data_ctrl_post.csv")
  # Replace skipped answers with "0"
  data_treat_post[is.na(data_treat_post)]= 0
  data_ctrl_post[is.na(data_ctrl_post)]= 0
  # Change column names
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")
  # Calculate average scores and generate group variable
  data_treat_post <- data_treat_post %>%
    mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3)) %>%
    mutate(datagroup=1)
  data_ctrl_post <- data_ctrl_post %>%
    mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3)) %>%
    mutate(datagroup=0)
  treat_post_average <- describe(data_treat_post$avg_score_post)
  ctrl_post_average <- describe(data_ctrl_post$avg_score_post)
  # Bind treat/control data
  group_data_binded <- rbind(data_treat_post, data_ctrl_post)
  # Name datagroup -> 0=control, 1=treatment
  group_data_binded$datagroup<-factor(group_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Get descriptive statistics
  group_data_binded %>% group_by(datagroup) %>% get_summary_stats(avg_score_post, type = "mean_sd")

## ----independent_samples_boxplots, echo = FALSE, out.width = "70%", fig.cap = "Independent Samples - Boxplots"----
  knitr::include_graphics("independentsamples_boxplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check Normality
  group_data_binded %>% group_by(datagroup) %>% shapiro_test(avg_score_post)
  cat("## Interpretation: the assumption of equality of variances has been met (p>0.05)", sep="\n")

## ----independent_samples_normal_qq_plot, echo = FALSE, out.width = "70%", fig.cap = "Independent Samples - Normal Q-Q Plots"----
  knitr::include_graphics("independentsamples_qqplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check Equal Variances
  group_data_binded %>% levene_test(avg_score_post ~ datagroup)
  cat("## Interpretation: the assumption of equality of variances has been met (p>0.05)", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Run independent samples t-test (two-sided, non-equal variances)
  t.test(group_data_binded$avg_score_post~group_data_binded$datagroup, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# Run Mann-Whitney U Test
  wilcox.test(group_data_binded$avg_score_post~group_data_binded$datagroup)


## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Read all data sets
  data_treat_pre <- read_csv("data_treat_pre.csv")
  data_treat_post<- read_csv("data_treat_post.csv")
  data_ctrl_pre <- read_csv("data_ctrl_pre.csv")
  data_ctrl_post<- read_csv("data_ctrl_post.csv")

  # Replace skipped answers with "0"
  data_treat_pre[is.na(data_treat_pre)]= 0
  data_treat_post[is.na(data_treat_post)]= 0
  data_ctrl_pre[is.na(data_ctrl_pre)]= 0
  data_ctrl_post[is.na(data_ctrl_post)]= 0

  # Creat data sets for descriptive statistics
  data_c_pre <- data_ctrl_pre %>% mutate(avg_score = round(rowMeans(data_ctrl_pre[,-1]),3))
  data_c_post <- data_ctrl_post %>% mutate(avg_score = round(rowMeans(data_ctrl_post[,-1]),3))
  data_t_pre <- data_treat_pre %>% mutate(avg_score = round(rowMeans(data_treat_pre[,-1]),3))
  data_t_post <- data_treat_post %>% mutate(avg_score = round(rowMeans(data_treat_post[,-1]),3))

  # Change column names with their origin'sir
  colnames(data_treat_pre) <- paste( colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste( colnames(data_treat_post), "post", sep = "_")
  colnames(data_ctrl_pre) <- paste( colnames(data_ctrl_pre), "pre", sep = "_")
  colnames(data_ctrl_post) <- paste( colnames(data_ctrl_post), "post", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>% mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>% mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
  data_ctrl_pre <- data_ctrl_pre %>% mutate(avg_score_pre = round(rowMeans(data_ctrl_pre[,-1]),3))
  data_ctrl_post <- data_ctrl_post %>% mutate(avg_score_post = round(rowMeans(data_ctrl_post[,-1]),3))

  # Merge pre/post data and generate group code (treat=1, control=0)
  treat_data_merged<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"
  treat_data_merged <- treat_data_merged %>% mutate(datagroup=1)

  ctrl_data_merged<-merge(data_ctrl_pre, data_ctrl_post, by.x = "id_pre",  by.y = "id_post")
  names(ctrl_data_merged)[names(ctrl_data_merged) == 'id_pre'] <- "id"
  ctrl_data_merged <- ctrl_data_merged %>% mutate(datagroup=0)

  # Bind treat/control data
  full_data_binded <- rbind(ctrl_data_merged, treat_data_merged)

  # Name datagroup -> 0=control, 1=treatment
  full_data_binded$datagroup<-factor(full_data_binded$datagroup,levels=c(0,1),labels=c("Control", "Treatment"))
  # Inspect the model diagnostic metrics
  model <- lm(avg_score_post ~ avg_score_pre + datagroup, data = full_data_binded)
  model_metrics <- augment(model) %>% dplyr::select(-.hat, -.sigma, -.fitted)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Descriptive Statistics
  pre_descriptive <- group_by (full_data_binded, datagroup) %>%
    summarize(mean=mean(avg_score_pre), sd=sd(avg_score_pre), min=min(avg_score_pre), max=max(avg_score_pre))
  post_descriptive <- group_by (full_data_binded, datagroup) %>%
    summarize(mean=mean(avg_score_post), sd=sd(avg_score_post), min=min(avg_score_post), max=max(avg_score_post))

  data_c_pre <- data_c_pre %>% mutate(datagroup=1)
  data_c_post <- data_c_post %>% mutate(datagroup=2)
  data_t_pre <- data_t_pre %>% mutate(datagroup=3)
  data_t_post <- data_t_post %>% mutate(datagroup=4)

  df_Long <- rbind(data_c_pre, data_c_post, data_t_pre, data_t_post)
  df_Long <- df_Long %>% select(avg_score, datagroup)
  names(df_Long) <- c("Average_Score", "Group")
  # Name Time(group) -> 1=Control-Pre, 2=Control-Post, 3=Treatment-Pre, 4=Treatment-Post
  df_Long$Group <- factor(df_Long$Group,levels=c(1,2,3,4),labels=c("Control-Pre", "Control-Post", "Treatment-Pre", "Treatment-Post"))

  print(pre_descriptive)
  print(post_descriptive)

## ----ancova_boxplots, echo = FALSE, out.width = "70%", fig.cap = "One-way ANCOVA Boxplots"----
  knitr::include_graphics("onewayancova_boxplots.png")

## ----ancova_linearity, echo = FALSE, out.width = "70%", fig.cap = "One-way ANCOVA - Scatter Plot"----
  knitr::include_graphics("onewayancova_linearity_check_scatter_plot.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  cat("## Interpretation: if you are seeing a liner relationship between the covariate (i.e., pre-test scores for this analysis) and dependent variable (i.e., post-test scores for this analysis) for both treatment and control group in the plot, then you can say this assumption has been met or the data has not violated this assumption of linearity. If your relationships are not linear, you have violated this assumption, and an ANCOVA is not a siutable analysis. However, you might be able to coax your data to have a linear relationship by transforming the covariate, and still be able to run an ANCOVA.", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check normality of Residuals
  cat("# Normality of Residuals:", sep="\n")
  norm.all.aov <- aov(avg_score_post ~ datagroup, data=full_data_binded)
  shapiro.test(norm.all.aov$residuals)
  cat("## Interpretation: the assumption of normality by group has been met (p>0.05).", sep="\n")
  Residuals <- norm.all.aov$residuals

## ----ancova_histogram, echo = FALSE, out.width = "70%", fig.cap = "One-way ANCOVA - Histogram"----
  knitr::include_graphics("onewayancova_histogram.png")

## ----ancova_normal_qq_plot, echo = FALSE, out.width = "70%", fig.cap = "One-way ANCOVA - Normal Q-Q Plot"----
  knitr::include_graphics("onewayancova_qqplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check homogeneity of variances
  leveneTest(avg_score_post ~ datagroup, full_data_binded)
  cat("## Interpretation: the assumption of equality of error variances has been met (p>0.05).", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check outlier
  outlier_variables <- model_metrics %>%  dplyr::filter(abs(.std.resid) > 3)  %>% as.data.frame()
  print(outlier_variables)
  cat("# Outliers: No outlier has been found.", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check homogeneity of regression line slopes
  lineslopes <- (lm(avg_score_post ~ avg_score_pre + datagroup + avg_score_pre:datagroup, data = full_data_binded))
  summary(lineslopes)
  cat("## Interpretation: there was homogeneity of regression slopes as the interaction term (i.e., datagroup:avg_score_pre) was not statistically significant (p>0.05).", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Run one-way ANCOVA
  res.aov <- (full_data_binded %>%  rstatix::anova_test(avg_score_post ~ datagroup + avg_score_pre))
  print(res.aov)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Display the adjusted means (a.k.a., estimated marginal means) for each group
  emms <- emmeans_test(full_data_binded,
                       avg_score_post ~ datagroup,
                       covariate = avg_score_pre,
                       p.adjust.method = "bonferroni",
                       conf.level=0.95,
                       detailed=TRUE)
  posthoc_emm <- get_emmeans(emms) %>% as.data.frame()
  print(posthoc_emm)
    if (res.aov$p[1] < 0.05) {
    cat(paste0("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be significant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(posthoc_emm$emmean[2],2),", SE=,",round(posthoc_emm$se[2],2),") was significantly different from that of the control group (",round(posthoc_emm$emmean[1],2),", SE=,",round(posthoc_emm$se[1],2),")."), sep="\n")
  } else {
    cat(paste0("--> A sample summary of the outputs/results above: The difference of post-test scores between the treatment and control groups turned out to be insignificant with pre-test scores being controlled: F(1,",res.aov$DFd[1],")=",res.aov$F[1],", p=",res.aov$p[1]," (effect size=",res.aov$ges[1],"). The adjusted marginal mean of post-test scores of the treatment group (",round(posthoc_emm$emmean[2],2),", SE=,",round(posthoc_emm$se[2],2),") was not significantly different from that of the control group (",round(posthoc_emm$emmean[1],2),", SE=,",round(posthoc_emm$se[1],2),")."), sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Read all data sets
  data_treat_pre <- read_csv("data_treat_pre.csv", show_col_types = FALSE)
  data_treat_post<- read_csv("data_treat_post.csv", show_col_types = FALSE)
  data_treat_post2 <- read_csv("data_treat_post2.csv", show_col_types = FALSE)

  # Replace skipped answers with "0"
  data_treat_pre[is.na(data_treat_pre)]= 0
  data_treat_post[is.na(data_treat_post)]= 0
  data_treat_post2[is.na(data_treat_post2)]= 0

  # Change column names with their origin'sir
  colnames(data_treat_pre) <- paste(colnames(data_treat_pre), "pre", sep = "_")
  colnames(data_treat_post) <- paste(colnames(data_treat_post), "post", sep = "_")
  colnames(data_treat_post2) <- paste(colnames(data_treat_post2), "post2", sep = "_")

  # Calculate average scores
  data_treat_pre <- data_treat_pre %>% mutate(avg_score_pre = round(rowMeans(data_treat_pre[,-1]),3))
  data_treat_post <- data_treat_post %>% mutate(avg_score_post = round(rowMeans(data_treat_post[,-1]),3))
  data_treat_post2 <- data_treat_post2 %>% mutate(avg_score_post2 = round(rowMeans(data_treat_post2[,-1]),3))

  # Merge pre/post/post2 data
  data_treat_prepost<-merge(data_treat_pre, data_treat_post, by.x = "id_pre",  by.y = "id_post")
  treat_data_merged<-merge(data_treat_prepost, data_treat_post2, by.x = "id_pre",  by.y = "id_post2")
  names(treat_data_merged)[names(treat_data_merged) == 'id_pre'] <- "id"

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Convert Data Frame to a Long Format & Define the Variable
  avg_score_df <- cbind(treat_data_merged$avg_score_pre, treat_data_merged$avg_score_post, treat_data_merged$avg_score_post2)
  df_Long <- melt(avg_score_df)
  names(df_Long) <- c("id", "Time", "Score")
  # Name Time(group) -> 1=Pre, 2=Post1, 3=Post2
  df_Long$id <- factor(df_Long$id)
  df_Long$Time <- factor(df_Long$Time,levels=c(1,2,3),labels=c("Pre", "Post1", "Post2"))

  # Descriptive Statistics
  descriptive <- df_Long %>% group_by(Time) %>% get_summary_stats(Score, type="common")
  print(descriptive)

## ----repeated_anova_boxplots, echo = FALSE, out.width = "70%", fig.cap = "One-way Repeated Measures ANOVA - Boxplots"----
  knitr::include_graphics("onewayrepeatedanova_boxplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  outliers <- df_Long %>% group_by(Time) %>% identify_outliers(Score)
  cat("## Interpretation: No extreme outlier was identified in your data.", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check normality of Residuals
  res.aov <- aov(Score~Time, data=df_Long)
  shapiro.test(resid(res.aov))
  cat("--> Interpretation: the residuals were normally distributed (p>0.05).", sep="\n")

## ----repeated_anova_histogram, echo = FALSE, out.width = "70%", fig.cap = "One-way Repeated Measures ANOVA - Histogram"----
  knitr::include_graphics("onewayrepeatedanova_histogram.png")

## ----repeated_anova_normal_qq_plot, echo = FALSE, out.width = "70%", fig.cap = "One-way Repeated Measures ANOVA - Normal Q-Q Plot"----
  knitr::include_graphics("onewayrepeatedanova_qqplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  cat("--> Interpretation: if all the points fall in the plots above approximately along the reference line, users can assume normality.", sep="\n")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  res.aov <- anova_test(data=df_Long, dv=Score, wid=id, within=Time)
  result <- get_anova_table(res.aov)
  print(result)
  if (result$p<0.001) {
    cat(paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.001, eta2(g)=",result$ges,"."), sep="\n")
  } else if (result$p<0.01) {
    cat(paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.01, eta2(g)=",result$ges,"."), sep="\n")
  } else if (result$p<0.05) {
    cat(paste0("--> Interpretation: The average test score at different time points of the intervention are  statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p<0.05, eta2(g)=",result$ges,"."), sep="\n")
  } else {
    cat(paste0("--> Interpretation: The average test score at different time points of the intervention are not statistically different: F(",result$DFn," ",result$DFd,")=",result$F,", p>0.05, eta2(g)=",result$ges,"."), sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  pwc <- df_Long %>% pairwise_t_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")
  print(pwc)
  # Between Pre and Post
  if (pwc$p.adj[2]<0.001) {
    if((descriptive$mean[2]-descriptive$mean[1])>0) {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.001)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.001)."), sep="\n")
    }
  } else if (pwc$p.adj[2]<0.01) {
    if((descriptive$mean[2]-descriptive$mean[1])>0) {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post1-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.01)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post1-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.01)."), sep="\n")
    }
  } else if (pwc$p.adj[2]<0.05) {
    if((descriptive$mean[2]-descriptive$mean[1])>0) {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post1-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly greater than the average pre-test score (p.adj<0.05)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post1-test score (",descriptive$mean[2],") are significantly different. The average post-test score is significantly smaller than the average pre-test score (p.adj<0.05)."), sep="\n")
    }
  } else {
      cat(paste0("--> Interpretation for 1: The average pre-test score (",descriptive$mean[1],") and the average post1-test score (",descriptive$mean[2],") are not significantly different (p.adj>0.05)."), sep="\n")
  }
  # Between Post and Post2
  if (pwc$p.adj[1]<0.001) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.001)."), sep="\n")
    } else {
    cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.001)."), sep="\n")
    }
  } else if (pwc$p.adj[1]<0.01) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.01)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.01)."), sep="\n")
    }
  } else if (pwc$p.adj[1]<0.05) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average post-test score (p.adj<0.05)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average post-test score (p.adj<0.05)."), sep="\n")
    }
  } else {
    cat(paste0("--> Interpretation for 2: The average post1-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are not significantly different (p.adj>0.05)."), sep="\n")
  }
  # Between Pre and Post2
  if (pwc$p.adj[3]<0.001) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[1],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.001)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.001)."), sep="\n")
    }
  } else if (pwc$p.adj[3]<0.01) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[1],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.01)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[2],") and the average Post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.01)."), sep="\n")
    }
  } else if (pwc$p.adj[3]<0.05) {
    if((descriptive$mean[3]-descriptive$mean[2])>0) {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[1],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly greater than the average pre-test score (p.adj<0.05)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[2],") and the average post2-test score (",descriptive$mean[3],") are significantly different. The average post2-test score is significantly smaller than the average pre-test score (p.adj<0.05)."), sep="\n")
    }
  } else {
    cat(paste0("--> Interpretation for 3: The average pre-test score (",descriptive$mean[1],") and the average post2-test score (",descriptive$mean[3],") are not significantly different (p.adj>0.05)."), sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  friedman_result <- friedman.test(avg_score_df)
  friedman_pwc <- df_Long %>% wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")
  print(friedman_result)
    if (friedman_result$p.value<0.001) {
      cat(paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.001)."), sep="\n")
    } else if (friedman_result$p.value<0.01) {
      cat(paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.01)."), sep="\n")
    } else if (friedman_result$p.value<0.05) {
      cat(paste0("--> Interpretation: the median test score is significantly different at the different time points during the intervention (p<0.05)."), sep="\n")
    } else {
      cat(paste0("--> Interpretation: the median test score is not significantly different at the different time points during the intervention (p>0.05)."), sep="\n")
    }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  friedman_result <- friedman.test(avg_score_df)
  friedman_pwc <- df_Long %>% wilcox_test(Score~Time, paired=TRUE, p.adjust.method="bonferroni")
  print(friedman_pwc)
    # Between Pre and post
    if (friedman_pwc$p.adj[2]<0.001) {
      if((descriptive$median[2]-descriptive$median[1])>0) {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.001)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.001)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[2]<0.01) {
      if((descriptive$median[2]-descriptive$median[1])>0) {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.01)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.01)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[2]<0.05) {
      if((descriptive$median[2]-descriptive$median[1])>0) {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly greater than the median pre-test score (p.adj<0.05)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are significantly different. The median post-test score is significantly smaller than the median pre-test score (p.adj<0.05)."), sep="\n")
      }
    } else {
      cat(paste0("--> Interpretation for 1: the median pre-test score (",descriptive$median[1],") and the median post-test score (",descriptive$median[2],") are not significantly different (p.adj>0.05)."), sep="\n")
    }
    # Between post and Post2
    if (friedman_pwc$p.adj[1]<0.001) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.001)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.001)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[1]<0.01) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.01)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.01)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[1]<0.05) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median post-test score (p.adj<0.05)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median post-test score (p.adj<0.05)."), sep="\n")
      }
    } else {
      cat(paste0("--> Interpretation for 2: the median post-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are not significantly different (p.adj>0.05)."), sep="\n")
    }
    # Between Pre and Post2
    if (friedman_pwc$p.adj[3]<0.001) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[1],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.001)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.001)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[3]<0.01) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[1],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.01)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.01)."), sep="\n")
      }
    } else if (friedman_pwc$p.adj[3]<0.05) {
      if((descriptive$median[3]-descriptive$median[2])>0) {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[1],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly greater than the median pre-test score (p.adj<0.05)."), sep="\n")
      } else {
        cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[2],") and the median post2-test score (",descriptive$median[3],") are significantly different. The median post2-test score is significantly smaller than the median pre-test score (p.adj<0.05)."), sep="\n")
      }
    } else {
      cat(paste0("--> Interpretation for 3: the median pre-test score (",descriptive$median[1],") and the median post2-test score (",descriptive$median[3],") are not significantly different (p.adj>0.05)."), sep="\n")
    }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Reading
    data_original <- read_csv("data_treat_pre.csv",col_types = cols())
    demographic_data <- read_csv("demographic_data.csv", show_col_types = FALSE)

  # Replace skipped answers with "0"
  data_original[is.na(data_original)]= 0

  # Calculate average scores
  data_original <- data_original %>% mutate(avg_score = round(rowMeans(data_original[,-1]),3))

  # Merge assessment data and demographic data
  data_original <- merge(data_original, demographic_data, by.x = "id",  by.y = "id")

  # Take demographic variable name and create a subset of the dataframe with average score and demographic data
  # group_name <- readline(prompt="Enter demographic variable name: ")
  data_original <- select(data_original, c('avg_score', 'grade'))
  names(data_original) <- c("average_score", "group")

  one_way <- aov(average_score ~ factor(group), data=data_original)
  pwc <- TukeyHSD(one_way)
  one_way2 <- data_original %>% welch_anova_test(average_score ~ group)
  pwc2 <- data_original %>% games_howell_test(average_score ~ group)

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
# Descriptive statistics
  descriptive <- data_original %>% group_by(group) %>% get_summary_stats(average_score, type = "mean_sd")
  print(descriptive)

## ----group_diff_boxplots, echo = FALSE, out.width = "70%", fig.cap = "Demographic Group Difference - Boxplots"----
  knitr::include_graphics("demogroupdiff_boxplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  # Check normality of residuals
  shapirotest <- shapiro.test(resid(one_way))
  qqplots <- ggqqplot(residuals(one_way),title = "Normal Q-Q Plot of Residuals")
  Residuals <- residuals(one_way)
  print(shapirotest)
  if (shapirotest$p.value > 0.05) {
    cat("## Interpretation: the assumption of normality by group has been met (p>0.05).", sep="\n")
  } else {
    cat("## Interpretation: the assumption of normality by group has NOT been met (p<0.05). Although ANOVA is robust to a violation of the assumption of normality of residuals, you may want to mention this violation in you report. For example, you can say: 'The data has slightly violated the assumption of normality of residuals, but ANCOVA is known to be robust to this violation (so it's not a serious issue).", sep="\n")
  }

## ----group_diff_histogram, echo = FALSE, out.width = "70%", fig.cap = "Demographic Group Difference - Histogram"----
  knitr::include_graphics("demogroupdiff_histogram.png")

## ----group_diff_normal_qq_plot, echo = FALSE, out.width = "70%", fig.cap = "Demographic Group Difference - Normal Q-Q Plot"----
  knitr::include_graphics("demogroupdiff_qqplots.png")

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  levene_result <- leveneTest(average_score ~ factor(group), data=data_original)
    print(levene_result)
  if (levene_result$`Pr(>F)`[1] > 0.05) {
    cat("## Interpretation: the assumption of equality of variances has been met (p>0.05).", sep="\n")
  } else {
    cat("## Interpretation: the assumption of equality of variances has NOT been met (p<0.05).", sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  if (levene_result$`Pr(>F)`[1]>0.05) {
  cat("Results of One-way ANOVA: Group Difference(s) (Parametric: Equal variances assumed)", sep="\n")
  print(summary(one_way))
  cat("--> Interpretation: the difference among the demographic sub-groups is not significant (P>0.05).", sep="\n")
  cat("Pairwide Comparisons (Equal variances assumed)", sep="\n")
  } else {
  cat("Results of One-way ANOVA: Group Difference(s) (Parametric: Unequal variances assumed)")
  message("Since the assumption of equal variances has NOT been satisfied, the Welch one-way test and the Games-Howell post-hoc test results are presented below.")
  print(summary(one_way2))
  cat("--> Interpretation: the difference among the demographic sub-groups is not significant (P>0.05).", sep="\n")
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  if (levene_result$`Pr(>F)`[1]>0.05) {
    cat("Pairwide Comparisons (Equal variances assumed)", sep="\n")
    print(pwc)
  } else {
    message("Since the assumption of equal variances has NOT been satisfied, the Welch one-way test and the Games-Howell post-hoc test results are presented below.")
    cat("Pairwide Comparisons (Unequal variances assumed)", sep="\n")
    print(pwc2)
  }

## ---- message=FALSE, warning=FALSE, echo=FALSE--------------------------------
  np_one_way <- kruskal.test(average_score ~ group, data=data_original)
  np_pwc <- pairwise.wilcox.test(data_original$average_score, data_original$group, p.adjust.method = "BH")
  cat("Results of Kruskal_Wallis Test (Non-parametric)", sep="\n")
  print(np_one_way)
  cat("Interpretation: The difference among demographic sub-groups is not significant (p>0.05).", sep="\n")
  print(np_pwc)

