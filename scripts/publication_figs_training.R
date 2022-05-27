#### Script for making publication figures (training data)
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

# function to plot the learning curve
make_learning_curve <- function() {
    # load in the data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "reach"
    )

    # group by experiment and trial_num_cont
    # then make a summary df
    data_group <- data %>%
        group_by(exp, block_num_detailed, trial_num_cont) %>%
        summarise(
            group_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            n = n(), .groups = "drop"
        )

    # set up the plot
    p <- data_group %>%
        ggplot(aes(
            x = trial_num_cont, y = group_mean,
            color = exp, group = paste(exp, block_num_detailed)
        ))

    # add classic theme
    p <- p + theme_classic()

    # add a solid horizontal lines at 0 and 60 and dashed lines at 15, 30, 45
    p <- p +
        geom_hline(
            yintercept = c(0, 60), size = 0.4,
            colour = "#CCCCCC", linetype = "solid"
        ) +
        geom_hline(
            yintercept = c(15, 30, 45), size = 0.4,
            colour = "#CCCCCC", linetype = "dashed"
        )

    # add vertial lines at the block boundaries
    p <- p +
        geom_vline(
            xintercept = c(0, 102, 204, 306), colour = "#CCCCCC",
            linetype = "dashed"
        )

    # add a grey box behind trial sets
    p <- p +
        geom_rect(
            xmin = 1, xmax = 3,
            ymin = -5, ymax = 60,
            colour = "#CCCCCC", fill = "#CCCCCC"
        ) +
        geom_rect(
            xmin = 79, xmax = 84,
            ymin = -5, ymax = 60,
            colour = "#CCCCCC", fill = "#CCCCCC"
        ) +
        geom_rect(
            xmin = 385, xmax = 390,
            ymin = -5, ymax = 60,
            colour = "#CCCCCC", fill = "#CCCCCC"
        )

    # add the confidence intervals and data
    p <- p + geom_ribbon(aes(
        x = trial_num_cont,
        ymin = group_mean - ci,
        ymax = group_mean + ci,
        fill = exp,
        group = paste(exp, block_num_detailed)
    ), colour = NA, alpha = 0.3) +
        geom_line(aes(colour = exp))

    # set colour palette
    p <- p +
        scale_color_manual(values = c(
            pallete$abrupt, pallete$ramped, pallete$stepped
        ), labels = c(
            "Abrupt", "Ramped", "Stepped"
        )) + scale_fill_manual(values = c(
            pallete$abrupt, pallete$ramped, pallete$stepped
        ), labels = c(
            "Abrupt", "Ramped", "Stepped"
        ))

    # set x and y axis labels
    p <- p +
        labs(
            x = "Block Number",
            y = "Hand Path Correction (°)"
        )
    # set y tick labels
    p <- p +
        scale_y_continuous(
            limits = c(-5, 65),
            breaks = c(0, 15, 30, 45, 60),
            labels = c(0, 15, 30, 45, 60)
        ) +
        scale_x_continuous(
            limits = c(0, 390),
            breaks = c(0, 102, 204, 306),
            labels = c(1, 2, 3, 4)
        )

    # set font size to 11
    p <- p +
        theme(text = element_text(size = 11))
    # remove the legend
    p <- p + theme(legend.position = "none")
    return(p)
}

# function to plot the trial sets
make_trial_set_figure <- function() {
    # load in the data
    data <- load_bl_corrected("data/bl_corrected/bl_corrected_data.csv",
        type = "reach"
    )

    # filter out the trials that are not in the trial set
    data <- data %>%
        filter(trial_set < 10) %>%
        unite(exp_trial_set, exp, trial_set)

    # make individual table
    data_ind <- data %>%
        group_by(exp_trial_set, ppt) %>%
        summarise(
            ind_mean = mean(angular_dev),
            sd = sd(angular_dev),
            ci = vector_confint(angular_dev),
            n = n(), .groups = "drop"
        )

    # make group table
    data_group <- data_ind %>%
        group_by(exp_trial_set) %>%
        summarise(
            group_mean = mean(ind_mean),
            sd = sd(ind_mean),
            ci = vector_confint(ind_mean),
            n = n(), .groups = "drop"
        )

    # make the plot
    # x axis is trial set, y axis is mean angular deviation, colour is exp
    p <- data_group %>%
        ggplot(aes(
            x = exp_trial_set, y = group_mean,
            color = exp_trial_set
        ))

    # add theme changes
    p <- p + theme_classic() +
        xlab("Trial Set") + ylab(NULL) +
        geom_hline(
            yintercept = c(0, 60),
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = c(15, 30, 45),
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )
    # add data points
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(y = ind_mean),
            alpha = 0.1,
            size = 1
        ) +
        geom_linerange(aes(
            ymin = group_mean - ci,
            ymax = group_mean + ci
        ), alpha = 0.5, lwd = 2) +
        geom_point(aes(shape = exp_trial_set),
            alpha = 1, size = 2
        )

    # set colour palette
    p <- p + scale_color_manual(values = c(
        pallete$abrupt, pallete$abrupt, pallete$abrupt,
        pallete$ramped, pallete$ramped, pallete$ramped,
        pallete$stepped, pallete$stepped
    )) + scale_shape_manual(values = c(
        1, 1, 19,
        1, 1, 19,
        1, 19
    ))

    # change x axis labels
    p <- p + scale_x_discrete(
        labels = c(
            "Init", "1", "4",
            "Init", "1", "4",
            "Init", "4"
        )
    )

    # set y axis ticks and labels
    p <- p +
        scale_y_continuous(
            limits = c(-5, 65),
            breaks = c(0, 15, 30, 45, 60),
            labels = c(0, 15, 30, 45, 60)
        )

    # set font size to 11
    p <- p +
        theme(text = element_text(size = 11))

    # remove the legend
    p <- p + theme(legend.position = "none")

    return(p)
}

# save learning curve figure
ggsave(make_learning_curve(),
    height = 4, width = 4.5, device = "pdf",
    filename = "data/paper_figs/learning_curve.pdf"
)

# save trial set figure
ggsave(make_trial_set_figure(),
    height = 4, width = 2, device = "pdf",
    filename = "data/paper_figs/trial_set.pdf"
)