#### Script for making publication figures
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
pallete$ramped <- "#f9b82c"
pallete$stepped <- "#a30f15"
pallete$abrupt <- "#6aafd2"


# load in the baseline corrected data
load_bl_corrected <- function(path, type = "nocursor") {
    # load in the large df
    rot_all <- read_delim(path,
        delim = ",",
        col_types = cols(
            .default = col_double(),
            targetangle_deg = col_factor(),
            strat_use = col_factor(),
            ppt = col_factor(),
            exp = col_factor(),
            block_num = col_factor(),
            reach_type = col_factor()
        )
    )

    # separate the nocursor and training data
    rot_nocur <- rot_all %>%
        filter(
            reach_type == type
        )

    return(rot_nocur)
}

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

    # make descriptive table
    data_group <- data_ind %>%
        group_by(exp_block) %>%
        summarise(
            group_mean = mean(ind_mean),
            sd = sd(ind_mean),
            ci = vector_confint(ind_mean),
            n = n(), .groups = "drop"
        )

    # view the data
    View(head(data_ind))

    # make the figure
    # x axis is the block number, y axis is the angular deviation
    # color is the experiment type
    p <- data_group %>%
        ggplot(aes(
            x = exp_block, y = group_mean,
            colour = exp_block, fill = exp_block
        )) +
        geom_linerange(aes(ymin = group_mean - ci, ymax = group_mean + ci),
            alpha = 0.5, lwd = 2
        ) +
        geom_point(aes(shape = exp_block),
            size = 2,
            alpha = 1
        )

    # add individual data points as beeswarm
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(x = exp_block, y = ind_mean, colour = exp_block),
            alpha = 0.1,
            size = 1
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

    # add theme changes
    p <- p + theme_classic() +
        xlab(NULL) + ylab("Implicit Aftereffect (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = 15,
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = 30,
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # change x axis text labels
    p <- p + scale_x_discrete(
        labels = c(
            "A1", "A4",
            "R1", "R4",
            "S4"
        )
    )

    # set y axis ticks and labels
    p <- p + scale_y_continuous(
        limits = c(-10, 35),
        breaks = c(0, 15, 30),
        labels = c(0, 15, 30)
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
        )) +
        geom_linerange(aes(ymin = group_mean - ci, ymax = group_mean + ci),
            alpha = 0.5, lwd = 2
        ) +
        geom_point(aes(shape = exp_block),
            size = 2,
            alpha = 1
        )

    # add individual data points as beeswarm
    p <- p +
        geom_beeswarm(
            data = data_ind,
            aes(x = exp_block, y = ind_mean, colour = exp_block),
            alpha = 0.1,
            size = 1
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

    # add theme changes
    p <- p + theme_classic() +
        xlab(NULL) + ylab("Explicit Strategy (°)") +
        geom_hline(
            yintercept = 0,
            linetype = "solid",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = 15,
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        ) +
        geom_hline(
            yintercept = 30,
            linetype = "dashed",
            size = 0.4,
            colour = "#CCCCCC"
        )

    # change x axis text labels
    p <- p + scale_x_discrete(
        labels = c(
            "A1", "A4",
            "R1", "R4",
            "S4"
        )
    )

    # set y axis ticks and labels
    p <- p + scale_y_continuous(
        limits = c(-10, 35),
        breaks = c(0, 15, 30),
        labels = c(0, 15, 30)
    )

    # remove the legend
    p <- p + theme(legend.position = "none")

    return(p)
}

# save the implicit figure
ggsave(make_implicit_figure(),
    height = 4, width = 3, device = "svg",
    filename = "data/paper_figs/implicit_aes.svg"
)

# save the strategy figure
ggsave(make_strategy_figure(),
    height = 4, width = 3, device = "svg",
    filename = "data/paper_figs/explicit_strats.svg"
)