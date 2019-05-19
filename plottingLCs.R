library(ggplot2)


confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector, na.rm = TRUE)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector, na.rm = TRUE)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

abrupt_rawEI <- read.csv("data/60degRotation/60_rigid_implicit_EI_trial.csv")
abrupt_rawIE <- read.csv("data/60degRotation/60_rigid_implicit_IE_trial.csv")

abrupt_raw <- data.frame(append(abrupt_rawEI, abrupt_rawIE[, 5:16]))
abrupt_raw <- GetRowsWithString("rotated_training_", abrupt_raw)

abrupt_raw <- abrupt_raw[, 5:29]

abrupt_means <- apply(abrupt_raw, 1, mean, na.rm = TRUE)

abrupt_ci <- apply(abrupt_raw, 1, confidence_interval, interval = 0.95)

abrupt_meanDF <- data.frame('deviations' = abrupt_means,
                            'trials' = seq(from = 1, to = 66),
                            'ci_lower' = abrupt_ci[1,],
                            'ci_upper' = abrupt_ci[2,])


# step-wise
stepwise_rawEI <- read.csv("data/multipleRotation/rigid_implicit_exp_EI_trial.csv")
stepwise_rawIE <- read.csv("data/multipleRotation/rigid_implicit_exp_IE_trial.csv")

stepwise_raw <- data.frame(append(stepwise_rawEI, stepwise_rawIE[, 5:16]))
stepwise_raw <- GetRowsWithString("rotated_training_", stepwise_raw)

stepwise_raw <- stepwise_raw[, 5:29]

stepwise_means <- apply(stepwise_raw, 1, mean, na.rm = TRUE)

stepwise_ci <- apply(stepwise_raw, 1, confidence_interval, interval = 0.95)

stepwise_meanDF <- data.frame('deviations' = stepwise_means,
                            'trials' = seq(from = 1, to = 264),
                            'ci_lower' = stepwise_ci[1,],
                            'ci_upper' = stepwise_ci[2,])


# plots
s <- ggplot(abrupt_meanDF, aes(x = trials, y = deviations)) +
  theme_minimal() +
  geom_smooth(data = abrupt_meanDF[1:45, ], aes(x = trials,
                                                y = deviations, ymin =ci_lower, ymax = ci_upper), 
              stat = "identity", colour = "#005DE4", fill = "#005DE4", size = 3) +
  geom_smooth(data = abrupt_meanDF[46:66, ], aes(x = trials,
                                                y = deviations, ymin =ci_lower, ymax = ci_upper), 
              stat = "identity", colour = "#005DE4", fill = "#005DE4", size = 3) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(0, 60), breaks = c(15, 30, 45, 60), name = "cursor deviation") +
  theme(text = element_text(size=36), axis.text = element_text(size=36), legend.text = element_text(size=36))

s
ggsave(s, height = 8, width = 10, device = "svg", filename = "data/abrupt_LC_plot.svg")

# plots
t <- ggplot(stepwise_meanDF, aes(x = trials, y = deviations)) +
  theme_minimal() +
  geom_smooth(data = stepwise_meanDF, aes(x = trials,
                                                y = deviations, ymin =ci_lower, ymax = ci_upper), 
              stat = "identity", colour = "#009C43", fill = "#009C43", size = 3) +
  scale_x_continuous(name = "trial") +
  scale_y_continuous(limits = c(0, 60), breaks = c(15, 30, 45, 60), name = "cursor deviation") +
  theme(text = element_text(size=36), axis.text = element_text(size=36), legend.text = element_text(size=36))

t
ggsave(t, height = 8, width = 25, device = "svg", filename = "data/stepwise_LC_plot.svg")
