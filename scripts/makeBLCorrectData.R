## --------------------------------
##
## Script name: makeBLCorrectData.R
##
## Purpose of script: Make baseline corrected data frames to be used by analysisNotebook.Rmd
##
## Input: omnibus dataframes (make sure to run makeOmnibus script on raw data if these are missing)
##
## --------------------------------
## 
## Author: Shanaa Modchalingam
##
## Date created: 2019-07-09
##
## Email: s.modcha@gmail.com
##
## --------------------------------

## ----
# Packages and required scripts
rm(list = ls())      # clean environment

source("src/helper_funcs.R")
source("src/df_mod_funcs.R")
library(data.table)
library(tidyverse)

## ----

# Cursor data
omnibus_training <- read_delim("data/omnibus/omnibus_training.csv", 
                               delim = ",", 
                               col_types = cols(.default = col_double(), 
                                                task_num = col_double(),
                                                trial_num = col_double(),
                                                rotation_angle = col_double(),
                                                targetangle_deg = col_factor(),
                                                stratUse = col_factor(),
                                                ppt = col_factor(),
                                                exp = col_factor()))

# filter out unused experiment groups
omnibus_training <- omnibus_training %>% 
  filter(exp != "abruptExp", exp != 'reintroExp') %>%
  select(-selected, -maxV) %>%
  rename(strat_use = stratUse)

# recode exp groups
omnibus_training$exp <- recode_factor(omnibus_training$exp, "longAbruptExp" = "abrupt",
                                      "gradualExp" = "ramped",
                                      "stepwiseExp" = "stepped",
                                      "abruptExp" = NULL,
                                      "reintroExp" = NULL)

# separate rotated and bl reaches
rot_training <- filter(omnibus_training, ((exp == "abrupt" | exp == "stepped") & rotation_angle != 0) | (exp == "ramped" & task_num >= 14 ))
bl_training <- filter(omnibus_training, ((exp == "abrupt" | exp == "stepped") & rotation_angle == 0) | (exp == "ramped" & task_num < 14 )) %>%
  filter(trial_num_cont > 15, !trial_num_cont %in% c(46, 47, 48))

# apply baseline correction ()
# make bl_df first (faster than doing this in apply)
bl_df <- bl_training %>%
  group_by(ppt, targetangle_deg) %>%
  summarise(bl_per_target = mean(angular_dev), .groups = "drop")

rot_training <- rot_training %>%
  left_join(bl_df, by=c("ppt", "targetangle_deg")) %>%
  mutate(temp = angular_dev - bl_per_target) %>%
  select(-bl_per_target)

# rename some columns
rot_training <- 
  rot_training %>% 
  rename(raw_angular_dev = angular_dev) %>%
  rename(angular_dev = temp)

# add the block nums
rot_training$block_num <- apply(rot_training[ , c("trial_num_cont", "exp")], 
                                1, add_training_block_num)
rot_training$block_num <- factor(rot_training$block_num, 
                                 levels=c("0", "1", "2", "3", "4", "10"))

# add the trial sets
rot_training$trial_set <- apply(rot_training[ , c("trial_num_cont", "exp")], 
                                1, add_trial_set)
rot_training$trial_set <- factor(rot_training$trial_set, 
                                 levels=c("1", "2", "3", "10"))

# fix the trial nums
# rot_training$trial_num_cont <- rot_training$trial_num_cont - 66

# arrange for trial numbering
rot_training <- rot_training %>% 
  arrange(exp)

# making continuous trial numbers
trials_abr_step_train <- c(1:45, 64:84, 103:147, 166:186, 205:249, 268:288, 307:351, 370:390) #these are all the training trials
trials_ramp_train <- c(1:66, 85:129, 148:168, 187:231, 250:270, 289:333, 352:372) # the numbers are different for the ramped experiment (missing a block of nocur)

# repeat these based on the number of ppt and concat
reps_abr_train <- rep(trials_abr_step_train, length(unique(filter(rot_training, exp == 'abrupt')$ppt)))
reps_step_train <- rep(trials_abr_step_train, length(unique(filter(rot_training, exp == 'stepped')$ppt)))
reps_ramp_train <- rep(trials_ramp_train, length(unique(filter(rot_training, exp == 'ramped')$ppt))) + 18
reps_train <- c(reps_abr_train, reps_ramp_train, reps_step_train)

# add the column to rot_training_t
rot_training$trial_num_cont <- reps_train

# log the reach_type
rot_training$reach_type <- "reach"

## ----
## No Cursor data

# Import data
omnibus_nocur <- read_delim("data/omnibus/omnibus_nocur.csv", 
                            delim = ",", 
                            col_types = cols(.default = col_double(), 
                                             task_num = col_double(),
                                             trial_num = col_double(),
                                             rotation_angle = col_double(),
                                             targetangle_deg = col_factor(),
                                             ppt = col_factor(),
                                             stratuse = col_factor(),
                                             exp = col_factor()))

# filter out unused experiment groups
omnibus_nocur <- omnibus_nocur %>% 
  filter(exp != "abruptExp", exp != "reintroExp") %>%
  rename(strat_use = stratuse)

# recode experiment groups
omnibus_nocur$exp <- recode_factor(omnibus_nocur$exp, "longAbruptExp" = "abrupt",
                                   "gradualExp" = "ramped",
                                   "stepwiseExp" = "stepped",
                                   "abruptExp" = NULL,
                                   "reintroExp" = NULL)


# separate the trials with rotation
rot_nocur <- filter(omnibus_nocur, rotation_angle != 0) 
bl_nocur <- filter(omnibus_nocur, rotation_angle == 0)

# remove first 3 baseline trials from baseline correction
bl_nocur_mins <- bl_nocur %>%
  group_by(ppt) %>%
  summarise(min_trial = min(trial_num), .groups = "drop")

bl_nocur <- bl_nocur %>%
  left_join(bl_nocur_mins, by="ppt") %>%
  filter(((exp == "abrupt" | exp == "ramped") & trial_num >= (min_trial + 3)) |
           (exp == "stepped" & trial_num >= (min_trial + 3)) |
           (exp == "stepped" & task_num == 11)) %>%
  select(-min_trial)

# apply the bl correction (this can take a while)
# make bl_df first (faster than doing this in apply)
bl_df <- bl_nocur %>%
  group_by(ppt, targetangle_deg) %>%
  summarise(bl_per_target = mean(angular_dev), .groups = "drop")

rot_nocur <- rot_nocur %>%
  left_join(bl_df, by=c("ppt", "targetangle_deg")) %>%
  mutate(temp = angular_dev - bl_per_target) %>%
  select(-bl_per_target)

# rename columns
rot_nocur <- rot_nocur %>% 
  rename(raw_angular_dev = angular_dev) %>%
  rename(angular_dev = temp)

# add block nums
rot_nocur$block_num <- apply(rot_nocur[ , c("task_num", "exp")], 1, add_nocur_block_num)
rot_nocur$block_num <- factor(rot_nocur$block_num, levels=c("1", "2", "3", "4"))


# add dummy trial sets (these aren't needed)
rot_nocur$trial_set <- "20"

# arrange for trial numbering
rot_nocur <- rot_nocur %>%
  arrange(exp, ppt, task_num)

# making continuous trial numbers for nocursors
# get the missing trials (nocusors)
temp <- 1:408
reps_abr_nc <- rep(temp[!(temp %in% trials_abr_step_train)], length(unique(filter(rot_training, exp == 'abrupt')$ppt)))
reps_step_nc <- rep(temp[!(temp %in% trials_abr_step_train)], length(unique(filter(rot_training, exp == 'stepped')$ppt)))

temp <- 1:390
reps_ramp_nc <- rep( temp[!(temp %in% trials_ramp_train)], length(unique(filter(rot_training, exp == 'ramped')$ppt))) + 18
reps_nc <- c(reps_abr_nc, reps_ramp_nc, reps_step_nc)

# add to rot_nocur
rot_nocur$trial_num_cont <- reps_nc

# log the trial type
rot_nocur$reach_type <- "nocursor"

## ----

# combine the data frames
rot_all <- bind_rows(rot_training, rot_nocur) %>%
  arrange(exp, ppt, trial_num_cont) %>%
  mutate(norm_angular_dev = angular_dev / rotation_angle * -1)

# add detailed blocks (1.1, 1.2, 2.1, ..., 4.2)
rot_all$block_num_detailed <- as.factor(apply(array(rot_all$trial_num_cont), 1, FUN = detailed_block_w_trial_num))
rot_all$strat_use <- as.factor(rot_all$strat_use)


# save the data
fwrite(rot_all, file = "data/bl_corrected/bl_corrected_data.csv")
