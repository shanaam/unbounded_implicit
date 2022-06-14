#### Script for making publication figures (no cursor data)
#### Author: Shanaa Modchalingam
#### Date: April 2022

# clean environment
rm(list = ls())
source("src/helper_funcs.R")
source("src/df_mod_funcs.R")
library(data.table)
library(tidyverse)
library(ggbeeswarm)

# palette for plotting:
# https://venngage-wordpress.s3.amazonaws.com/uploads/2019/08/color-blind-friendly-palette-9.png # nolint

# note: R has no has tables, but environments can work like one
# (they are hashed under the hood)
pallete <- new.env()
pallete$ramped <- "#f07c04"
pallete$stepped <- "#a30f15"
pallete$abrupt <- "#6aafd2"

# make implicit aftereffect
make_implicit_figure <- function() {
    # load in the data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "nocursor"
    ) %>% filter(strat_use == 0)

    # filter out the blocks of interest
    data <- data %>%
        filter((exp == "stepped" & block_num == "4") |
            (exp == "abrupt" & block_num == "1") |
            (exp == "ramped" & block_num == "1") |
            (exp == "abrupt" & block_num == "4") |
            (exp == "ramped" & block_num == "4")) %>%
        unite(exp_block, exp, block_num)

    # make individual table
    data_ind <- data %>%
        group_by(exp_block, ppt) %>%
        summarise(
            ind_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            n = n(), .groups = "drop"
        )

    # make group table
    data_group <- data_ind %>%
        group_by(exp_block) %>%
        summarise(
            group_mean = mean(ind_mean),
            sd = sd(ind_mean),
            ci = vector_confint(ind_mean),
            n = n(), .groups = "drop"
        )

    # view the data
    # View(head(data_ind))

    # make the figure
    # x axis is the block number, y axis is the angular deviation
    # color is the experiment type
    p <- data_group %>%
        ggplot(aes(
            x = exp_block, y = group_mean,
            colour = exp_block, fill = exp_block
        ))

    # add data points
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(x = exp_block, y = ind_mean, colour = exp_block),
            alpha = 0.1,
            size = 1
        ) +
        geom_linerange(aes(ymin = group_mean - ci, ymax = group_mean + ci),
            alpha = 0.5, lwd = 2
        ) +
        geom_point(aes(shape = exp_block),
            size = 2,
            alpha = 1
        )

    # add theme changes
    p <- p + theme_classic() +
        xlab("Block") + ylab("Hand Deviation Without Strategy (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = c(15, 30),
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # set colour palette
    p <- p + scale_color_manual(values = c(
        pallete$abrupt, pallete$abrupt,
        pallete$ramped, pallete$ramped,
        pallete$stepped
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    )) + scale_shape_manual(values = c(
        1, 19,
        1, 19,
        19
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    ))

    # change x axis text labels
    p <- p + scale_x_discrete(
        labels = c(
            "1", "4",
            "1", "4",
            "4"
        )
    )

    # set y axis ticks and labels
    p <- p + scale_y_continuous(
        limits = c(-10, 45),
        breaks = c(0, 15, 30, 45),
        labels = c(0, 15, 30, 45)
    )

    # remove the legend
    p <- p + theme(legend.position = "none")

    return(p)
}

# function to make include strategy figure
make_include_figure <- function() {
    # load in the data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "nocursor"
    ) %>% filter(strat_use == 1)

    # filter out the blocks of interest
    data <- data %>%
        filter((exp == "stepped" & block_num == "4") |
            (exp == "abrupt" & block_num == "1") |
            (exp == "ramped" & block_num == "1") |
            (exp == "abrupt" & block_num == "4") |
            (exp == "ramped" & block_num == "4")) %>%
        unite(exp_block, exp, block_num)

    # make individual table
    data_ind <- data %>%
        group_by(exp_block, ppt) %>%
        summarise(
            ind_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            n = n(), .groups = "drop"
        )

    # make group table
    data_group <- data_ind %>%
        group_by(exp_block) %>%
        summarise(
            group_mean = mean(ind_mean),
            sd = sd(ind_mean),
            ci = vector_confint(ind_mean),
            n = n(), .groups = "drop"
        )

    # view the data
    # View(head(data_ind))

    # make the figure
    # x axis is the block number, y axis is the angular deviation
    # color is the experiment type
    p <- data_group %>%
        ggplot(aes(
            x = exp_block, y = group_mean,
            colour = exp_block, fill = exp_block
        ))

    # add data points
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(x = exp_block, y = ind_mean, colour = exp_block),
            alpha = 0.1,
            size = 1
        ) +
        geom_linerange(aes(ymin = group_mean - ci, ymax = group_mean + ci),
            alpha = 0.5, lwd = 2
        ) +
        geom_point(aes(shape = exp_block),
            size = 2,
            alpha = 1
        )

    # add theme changes
    p <- p + theme_classic() +
        xlab("Block") + ylab("Hand Deviation With Strategy (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = c(15, 30),
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # set colour palette
    p <- p + scale_color_manual(values = c(
        pallete$abrupt, pallete$abrupt,
        pallete$ramped, pallete$ramped,
        pallete$stepped
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    )) + scale_shape_manual(values = c(
        1, 19,
        1, 19,
        19
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    ))

    # change x axis text labels
    p <- p + scale_x_discrete(
        labels = c(
            "1", "4",
            "1", "4",
            "4"
        )
    )

    # set y axis ticks and labels
    p <- p + scale_y_continuous(
        limits = c(-10, 45),
        breaks = c(0, 15, 30, 45),
        labels = c(0, 15, 30, 45)
    )

    # remove the legend
    p <- p + theme(legend.position = "none")

    return(p)
}

# function to make explicit strategy figure
make_strategy_figure <- function() {
    # load in the EXPLICIT data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "nocursor"
    )

    # filter out the blocks of interest
    data <- data %>%
        filter((exp == "stepped" & block_num == "4") |
            (exp == "abrupt" & block_num == "1") |
            (exp == "ramped" & block_num == "1") |
            (exp == "abrupt" & block_num == "4") |
            (exp == "ramped" & block_num == "4")) %>%
        unite(exp_block, exp, block_num)

    # make individual table
    data_ind <- data %>%
        group_by(exp_block, ppt, strat_use) %>%
        summarise(
            ind_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            n = n(), .groups = "drop"
        )
    # calculate explicit strategy for each participant
    data_ind <- data_ind %>%
        select(-sd, -ci) %>%
        pivot_wider(
            names_from = "strat_use",
            values_from = ind_mean,
            names_prefix = "strat_"
        ) %>%
        mutate(ind_mean = strat_1 - strat_0)

    # make descriptive table
    data_group <- data_ind %>%
        group_by(exp_block) %>%
        summarise(
            group_mean = mean(ind_mean),
            sd = sd(ind_mean),
            ci = vector_confint(ind_mean),
            n = n(), .groups = "drop"
        )

    # make the figure
    # x axis is the block number, y axis is the angular deviation
    # color is the experiment type
    p <- data_group %>%
        ggplot(aes(
            x = exp_block, y = group_mean,
            colour = exp_block, fill = exp_block
        ))

    # add theme changes
    p <- p + theme_classic() +
        xlab("Block") + ylab("Explicit Strategy (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = c(15, 30),
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # add data
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(x = exp_block, y = ind_mean, colour = exp_block),
            alpha = 0.1,
            size = 1
        ) +
        geom_linerange(aes(ymin = group_mean - ci, ymax = group_mean + ci),
            alpha = 0.5, lwd = 2
        ) +
        geom_point(aes(shape = exp_block),
            size = 2,
            alpha = 1
        )

    # set colour palette
    p <- p + scale_color_manual(values = c(
        pallete$abrupt, pallete$abrupt,
        pallete$ramped, pallete$ramped,
        pallete$stepped
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    )) + scale_shape_manual(values = c(
        1, 19,
        1, 19,
        19
    ), labels = c(
        "Abrupt 1", "Abrupt 4",
        "Ramped 1", "Ramped 4",
        "Stepped 4"
    ))

    # change x axis text labels
    p <- p + scale_x_discrete(
        labels = c(
            "1", "4",
            "1", "4",
            "4"
        )
    )

    # set y axis ticks and labels
    p <- p + scale_y_continuous(
        limits = c(-10, 45),
        breaks = c(0, 15, 30, 45),
        labels = c(0, 15, 30, 45)
    )

    # remove the legend
    p <- p + theme(legend.position = "none")

    return(p)
}

# function to make decay figure
make_decay_figure <- function() {
    # load in the DECAY data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "nocursor"
    )

    # add a column with 1-9 repeating
    data <- data %>%
        mutate(trial_in_block = rep(1:9, length.out = nrow(data)))

    # select useful columns
    data <- data %>%
        select(exp, ppt, strat_use, block_num, trial_in_block, angular_dev) %>%
        group_by(exp, ppt, strat_use, block_num, trial_in_block) %>%
        summarise(
            angular_dev_m = mean(angular_dev),
            n = n()
        ) %>%
        pivot_wider(
            names_from = strat_use,
            names_prefix = "strat_",
            values_from = angular_dev_m
        ) %>%
        mutate(strategy = strat_1 - strat_0) %>%
        pivot_longer(
            cols = 6:8,
            names_to = "strat_use", values_to = "angular_dev"
        )

    # in strat_use, replace "strat_0" with "Without Strategy"
    # and "strat_1" with "With Strategy",
    # and "strategy" with "Explicit Strategy"
    data <- data %>%
        mutate(strat_use = ifelse(strat_use == "strat_0", "Without Strategy",
            ifelse(strat_use == "strat_1", "With Strategy",
                ifelse(strat_use == "strategy", "Explicit Strategy",
                    strat_use
                )
            )
        ))

    # make strat_use a factor
    data <- data %>%
        mutate(strat_use = factor(strat_use,
            levels = c("Without Strategy", "With Strategy", "Explicit Strategy")
        ))

    # make individual table
    data_ind <- data %>%
        group_by(exp, ppt, strat_use, block_num) %>%
        summarise(
            ind_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            slope = decay_fit(trial_in_block, angular_dev)[2],
            intercept = decay_fit(trial_in_block, angular_dev)[1],
            n = n(), .groups = "drop"
        )

    # make group table
    data_group <- data_ind %>%
        group_by(exp, strat_use, block_num) %>%
        summarise(
            group_slope_mean = mean(slope),
            slope_ci = vector_confint(slope),
            group_intercept_mean = mean(intercept),
            intercept_ci = vector_confint(intercept),
            n = n(), .groups = "drop"
        )

    # add a nested vector column
    data_group <- data_group %>%
        mutate(trial_in_block = list((c(1:9))))

    # unnest
    data_group <- data_group %>%
        unnest_longer(trial_in_block)

    # make a column with inferred y values
    data_group <- data_group %>%
        mutate(y_inferred = group_slope_mean * trial_in_block +
            group_intercept_mean)
    
    #### TESTING ####
    data_group2 <- data %>%
      group_by(exp, strat_use, block_num) %>%
      summarise(
        ind_mean = mean(angular_dev),
        sd = sd(angular_dev),
        ci = vector_confint(angular_dev),
        slope = decay_fit(trial_in_block, angular_dev)[2],
        intercept = decay_fit(trial_in_block, angular_dev)[1],
        confint_low = reg_confints(trial_in_block, angular_dev)[[1]][1:9],
        confint_up = reg_confints(trial_in_block, angular_dev)[[2]][1:9],
        n = n(), .groups = "drop"
      )
    # add trial in block
    data_group2$trial_in_block = rep(c(1:9), length.out = nrow(data_group2))
    # make a column with inferred y values
    data_group2 <- data_group2 %>%
      mutate(y_inferred = slope * trial_in_block +
               intercept)


    # plot the figure using data_group,
    # split by blocks on x axis, and by experiment on colour
    p <- data %>%
        ggplot(aes(
            x = trial_in_block, y = angular_dev,
            colour = exp, fill = exp
        ))

    # add theme changes
    p <- p + theme_classic() +
        xlab("Trial in Block") + ylab("Hand Deviation (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = c(15, 30),
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # add data
    p <- p +
        # geom_line(
        #     data = data_group,
        #     aes(y = y_inferred),
        #     size = 1,
        #     alpha = 0.5,
        #     colour = "red"
        # ) +
        geom_line(
          data = data_group2,
          aes(y = y_inferred),
          size = 1,
          alpha = 1
        ) +
        geom_ribbon(
          data = data_group2,
          aes(y = y_inferred,
              ymin = confint_low,
              ymax = confint_up),
          size = 1,
          alpha = 0.2,colour = NA
        ) +
        # geom_smooth(
        #     method = "lm",
        #     se = TRUE,
        #     level = 0.99,
        #     size = 0.5,
        #     alpha = 0.1
        # ) +
        facet_grid(
            strat_use ~ block_num,
            space = "free"
        )

    # set y axis from 0 to 45
    p <- p + scale_y_continuous(
        limits = c(-10, 40),
        breaks = c(0, 15, 30),
        labels = c(0, 15, 30)
    ) + scale_x_continuous(
        breaks = c(3, 6, 9)
    )

    # set manual colour palette
    p <- p + scale_color_manual(values = c(
        pallete$abrupt,
        pallete$ramped,
        pallete$stepped
    )) + scale_fill_manual(values = c(
        pallete$abrupt,
        pallete$ramped,
        pallete$stepped
    ))

    # theme changes
    p <- p +
        theme(
            strip.background = element_blank(),
        )

    # remove the legend
    p <- p + theme(legend.position = "none")

    # return
    return(p)
}

make_decay_implicit_figure <- function() {
  # load in the DECAY data
  data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
                            type = "nocursor"
  )
  
  # add a column with 1-9 repeating
  data <- data %>%
    mutate(trial_in_block = rep(1:9, length.out = nrow(data)))
  
  # select useful columns
  data <- data %>%
    select(exp, ppt, strat_use, block_num, trial_in_block, angular_dev) %>%
    group_by(exp, ppt, strat_use, block_num, trial_in_block) %>%
    summarise(
      angular_dev_m = mean(angular_dev),
      n = n()
    ) %>%
    pivot_wider(
      names_from = strat_use,
      names_prefix = "strat_",
      values_from = angular_dev_m
    ) %>%
    mutate(strategy = strat_1 - strat_0) %>%
    pivot_longer(
      cols = 6:8,
      names_to = "strat_use", values_to = "angular_dev"
    )
  
  # in strat_use, replace "strat_0" with "Without Strategy"
  # and "strat_1" with "With Strategy",
  # and "strategy" with "Explicit Strategy"
  data <- data %>%
    mutate(strat_use = ifelse(strat_use == "strat_0", "Without Strategy",
                              ifelse(strat_use == "strat_1", "With Strategy",
                                     ifelse(strat_use == "strategy", "Explicit Strategy",
                                            strat_use
                                     )
                              )
    ))
  
  # make strat_use a factor
  data <- data %>%
    mutate(strat_use = factor(strat_use,
                              levels = c("Without Strategy", "With Strategy", "Explicit Strategy")
    )) %>%
    filter(strat_use == "Without Strategy") %>%
    unite(exp_block, exp, block_num)
  
  # make individual table
  data_ind <- data %>%
    group_by(exp_block, ppt, strat_use) %>%
    summarise(
      ind_mean = mean(angular_dev),
      sd = sd(angular_dev),
      ci = vector_confint(angular_dev),
      slope = decay_fit(trial_in_block, angular_dev)[2],
      intercept = decay_fit(trial_in_block, angular_dev)[1],
      n = n(), .groups = "drop"
    )
  
  # make group table
  data_group <- data_ind %>%
    group_by(exp_block, strat_use) %>%
    summarise(
      group_slope_mean = mean(slope),
      slope_ci = vector_confint(slope),
      group_intercept_mean = mean(intercept),
      intercept_ci = vector_confint(intercept),
      n = n(), .groups = "drop"
    )
  
  # color is the experiment type
  p <- data_group %>%
    ggplot(aes(
      x = exp_block, y = group_slope_mean,
      colour = exp_block, fill = exp_block
    ))
  
  # add data points
  p <- p +
    geom_beeswarm(
      data = data_ind,
      aes( y = slope),
      alpha = 0.1,
      size = 1
    ) +
    geom_linerange(aes(ymin = group_slope_mean - slope_ci, 
                       ymax = group_slope_mean + slope_ci),
                   alpha = 0.5, lwd = 2
    ) +
    geom_point(
               size = 2, alpha = 1
    )
  
  # add theme changes
  p <- p + theme_classic() +
    xlab("Block") + ylab("Decay Rate (°/trial)") +
    geom_hline(
      yintercept = 0,
      linetype = "solid",
      size = 0.4,
      colour = "#CCCCCC"
    )
  
  # set colour palette
  p <- p + scale_color_manual(values = c(
    pallete$abrupt, pallete$abrupt, pallete$abrupt, pallete$abrupt, 
    pallete$ramped,pallete$ramped,pallete$ramped,pallete$ramped,
    pallete$stepped,pallete$stepped,pallete$stepped,pallete$stepped
  ), labels = c(
    "Abrupt","Abrupt","Abrupt","Abrupt",
    "Ramp", "Ramped","Ramped","Ramped",
    "Step", "Step", "Step", "Step"
  )) 
  
  # change x axis text labels
  p <- p + scale_x_discrete(
    labels = c(
      "1", "2", "3", "4",
      "1", "2", "3", "4",
      "1", "2", "3", "4"
    )
  )
  
  # # set y axis ticks and labels
  # p <- p + scale_y_continuous(
  #   limits = c(-10, 40),
  #   breaks = c(0, 15, 30),
  #   labels = c(0, 15, 30)
  # )
  # 
  # remove the legend
  #p <- p + theme(legend.position = "none")
  
  return(p)
}

# Note: all final figures will have a width of 7 inches
# save the implicit figure
ggsave(make_implicit_figure(),
    height = 4, width = 2.3, device = "pdf",
    filename = "data/paper_figs/implicit_aes.pdf"
)

# save the include figure
ggsave(make_include_figure(),
    height = 4, width = 2.3, device = "pdf",
    filename = "data/paper_figs/include_aes.pdf"
)

# save the strategy figure
ggsave(make_strategy_figure(),
    height = 4, width = 2.3, device = "pdf",
    filename = "data/paper_figs/explicit_strats.pdf"
)

# save the decay figure
ggsave(make_decay_figure(),
    height = 6, width = 7, device = "pdf",
    filename = "data/paper_figs/decay_aes.pdf"
)

ggsave(make_decay_implicit_figure(),
       height = 6, width = 7, device = "pdf",
       filename = "data/paper_figs/decay_ind_implicit_aes.pdf"
)


# test <- data %>% 
#   filter(ppt == "001_longAbruptExp", block_num == 1,
#          strat_use == "Without Strategy")
# 
# reg_confints(test$trial_in_block, test$angular_dev)
