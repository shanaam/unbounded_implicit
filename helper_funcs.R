library(foreign)     # to load SPSS file
library(psych)       # for descriptives
library(effsize)     # for Cohen's d
library(BayesFactor) # to compute Bayes factors
library(tidyverse)

plot_group_density <- function(df, group, y, title = "Distributions within Groups"){
  # plot some density things for visualizing data in groups
  # group, and y are strings
  
  p <- df %>% 
    ggplot(aes(.data[[group]],.data[[y]])) +
    geom_violin(aes(fill=.data[[group]]), alpha=0.2, draw_quantiles = c(.25, .5, .75), scale = "count") +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size=3, color="red") +
    ggtitle(title)
  
  return(p)
}


bayes_t_test <- function(df, group_title, group1, group2, dv){
  # bayes t-test
  
  data <- df %>%
    group_by(NULL) %>%
    filter(.data[[group_title]] == group1 | .data[[group_title]] == group2) %>%
    select(group_title, dv)
  
  res.bayes           <- data.frame(matrix(NA, nrow = 1, ncol = 4))
  rownames(res.bayes) <- colnames(data)[-1]
  colnames(res.bayes) <- c("BF01", "Postd.Median", "Postd.LB", "Postd.UB")
  
  # the test
  
  i <- 1 # this is useful for iterating
  # Note: code from https://osf.io/gfdjy/
  # Removing NAs
  data.noNA <- data[!is.na(data[, i+1]), ]
  
  # Identify independent groups,
  # BF01:
  tmp  <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[ , dv]), 
                  pull(filter(data.noNA, .data[[group_title]] == group2)[ , dv]),
                  rscale = .707)
  BF01 <- 1 / extractBF(tmp)$bf
  
  # Posterior distribution for BF01:
  set.seed(i)
  tmp  <- ttestBF(pull(filter(data.noNA, .data[[group_title]] == group1)[ , dv]), 
                  pull(filter(data.noNA, .data[[group_title]] == group2)[ , dv]),
                  posterior = TRUE, 
                  iterations = 10000, 
                  rscale = .707)
  CI.bayes <- quantile(tmp[, "delta"], probs = c(.03, .97))
  
  res.bayes[i, ] <- c(BF01, median(tmp[, "delta"]), CI.bayes)
  
  # show
  res.bayes
  
  #    2.6 Posterior model probabilities for equal prior odds ----
  # post.H0:
  null <- res.bayes$BF01 / (1 + res.bayes$BF01)
  # post.H1:
  alt <- 1 / (1 + res.bayes$BF01)
  
  return (sprintf("BF01: %.3f, P(D|H0): %.3f, P(D|H1): %.3f, Groups: %s vs %s", res.bayes$BF01, null, alt, group1, group2))
}
