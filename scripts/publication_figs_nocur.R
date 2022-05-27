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


    # plot the figure using data_group,
    # split by blocks on x axis, and by experiment on colour
    p <- data %>%
        ggplot(aes(
            x = trial_in_block, y = angular_dev,
            colour = exp, fill = exp
        )) +
        #         geom_line(
        # aes(y = y_inferred),
        # size = 1,
        # alpha = 0.7
        # ) +
        geom_smooth(
            method = "lm",
            se = TRUE,
            level = 0.99,
            size = 0.5,
            alpha = 0.5
        ) +
        facet_grid(
            strat_use ~ block_num,
            space = "free"
        ) +
        theme_minimal()

    # set y axis from 0 to 45
    p <- p + scale_y_continuous(
        limits = c(0, 45),
        breaks = c(0, 15, 30, 45),
        labels = c(0, 15, 30, 45)
    )

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
    height = 4, width = 9, device = "pdf",
    filename = "data/paper_figs/decay_aes.pdf"
)