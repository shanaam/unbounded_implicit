## Load packages
library(data.table)
library(tidyverse)
library(ggbeeswarm)
source("analysisFunctions.R")

## ----
## Functions
# function for making the omnubus thing

applyAtan2 <- function(x, y){
  return((atan2( y, x) * 180/pi) %% 360) # atan2(y,x) -- atan2 takes y first
}

## given a string of numbers from pavlovia experiments, makes an array
online_string_to_array <- function(str_array){
  
  x <- str_sub(str_array, 2, str_length(str_array)-1) # subset string top get rid of beginning of end
  x <- simplify(lapply(str_split(x, ','), as.double)) # make this a list of doubles (simplify gets rid of unnecessary levels)
  
  return(x)
}

# given an array of 2 long and ugly strings, figure out the angle at the .3 point (needs mousexrel, mouseyrel, target)
apply_find_angle <- function(mouse_move_xy){
  
  mousex <- online_string_to_array(mouse_move_xy[1])
  mousey <- online_string_to_array(mouse_move_xy[2])
  
  # make a df to work with
  mouse_move <- tibble(.rows = length(mousex), datapoint = 1: length(mousex))
  
  mouse_move <- mouse_move %>% 
    add_column(mouse_x = mousex, mouse_y = mousey)
  
  homeposy <- -0.5
  for(i in mouse_move$datapoint){
    distance <- norm_vec(c(mouse_move$mouse_x[i], mouse_move$mouse_y[i] - homeposy)) #this bit accounts for home position
    if(distance > 0.3){
      x <- mouse_move$mouse_x[i]
      y <- mouse_move$mouse_y[i] - homeposy
      
      # do a rotation matrix to move the reach to be normalized to a target at 100deg *************
      # theta_deg <- 100 - as.double( mouse_move_xy[3] )
      # theta <- theta_deg * pi/180
      # 
      # newx <- x*cos(theta) - y*sin(theta)
      # newy <- x*sin(theta) + y*cos(theta)
      # 
      # reach_angle <- applyAtan2(newx, newy)
      # angular_dev <- reach_angle - 100
      
      reach_angle <- applyAtan2(x, y)
      angular_dev <- reach_angle - as.double( mouse_move_xy[3] )
      
      # apply Atan2 to our coordinates get the angle
      return(angular_dev)
    }
  }
}


# actually save a csv
makeOnlineOmnibus <- function(){
  
  path <- "data/raw/online_direct_repetition"
  datalist <- list()
  i <- 1
  
  for (ppt in list.files(path = path, full.names = TRUE)){
    # print(ppt)
    
    df <- fread(ppt, stringsAsFactors = FALSE)
    
    df$angular_dev <- apply(df[ , c('mousex_rel', 'mousey_rel', 'target')], 
                            1, FUN = apply_find_angle)
    
    ###########CLEAN UP THE CSV##############################
    # get rid of the really long ugly stuff
    df <- df %>%
      select(-mousex_rel, -mousey_rel, -mouse_time, -step, 
             -trials.thisTrialN, -trials.thisRepN, -trials.thisIndex, -trials.ran,
             -qualtrics, -expName, -psychopyVersion, -OS) %>%
      rename(trial_num = trials.thisN, cursor_vis = feedback_type)
    # recode 1s and 2s in the condition to be more descriptive
    df$condition <- recode(df$condition, '1' = 'abrupt', '2' = 'step') 
  
    
    ############OUTLIER REMOVAL##############################
    learningscore <- df %>%
      filter(trial_num < 394 & trial_num > 382) %>%
      select(angular_dev) %>%
      summarise(mean = mean(angular_dev, na.rm = TRUE))

    df$learner <- FALSE
    if(learningscore$mean < -20)  ##make sure people learn at least 1/3 of rotation
      df$learner <- TRUE
    
    
    # save this one df to datalist
    datalist[[i]] <- df
    
    i <- i+1
  }
  omnibus_online <- do.call(rbind, datalist)
  
  # some changes
  omnibus_online$trial_num <- omnibus_online$trial_num + 1 
  
  
  # some reaches close to 300 + at the 22 target (the reach angles are obv below the 360 line)*****
  
  # save the omnibus df
  fwrite(omnibus_online, file = "data/omnibus/omnibus_online.csv")
}

apply_outlier_simple <- function(x){
  if(x > 90 | x < -90)
    return(NaN)
  else
    return(x)
}


## Some actual work (migrate to notebook)
omnibus_online <- read_delim("data/omnibus/omnibus_online.csv", 
                            delim = ",", 
                            col_types = cols(.default = col_double(), 
                                             trial_num = col_double(),
                                             rotation_deg = col_factor(),
                                             target = col_factor(),
                                             participant = col_factor(),
                                             cursor_vis = col_factor(),
                                             date = col_character(),
                                             condition = col_factor(),
                                             learner = col_logical()))



# simple outlier removal
omnibus_online <- omnibus_online %>%
  filter(learner)

omnibus_online$angular_dev <- apply(omnibus_online[ , c('angular_dev')], 
                                    1, FUN = apply_outlier_simple)

omnibus_online$angular_dev <- omnibus_online$angular_dev * (-1)


all_means <- omnibus_online %>%
  group_by(condition, trial_num) %>%
  summarise(mean_devs = mean(angular_dev, na.rm = TRUE), 
            sd = sd(angular_dev, na.rm = TRUE), 
            ci = vector_confint(angular_dev))



# # first split up conditions
# abrupt_online <- omnibus_online %>%
#   filter(condition == 'abrupt')
# 
# step_online <- omnibus_online %>%
#   filter(condition == 'step')

  
# plot(mousex, mousey)
p <- ggplot(all_means, mapping = aes(trial_num, mean_devs, colour = condition)) +
  geom_smooth(aes(ymin = mean_devs - ci, ymax = mean_devs + ci, fill = condition),
              stat = "identity", size = 3) +
  scale_y_continuous(name = "hand deviation (°)") +
  scale_x_continuous(name = "trial") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour = "#CCCCCC")) +
  NULL
p

p <- p +
  theme(text = element_text(size=40), 
        axis.text = element_text(size=40), 
        legend.text = element_text(size=48))

ggsave(p, height = 13, width = 20, device = "svg", filename = "data/online_plot.svg")


# plot nocursors

nocursors_60 <- omnibus_online %>%
  filter(cursor_vis == 0, trial_num > 390)

nocur_means <- nocursors_60 %>%
  group_by(condition, participant) %>%
  summarise(mean_devs = mean(angular_dev, na.rm = TRUE), 
            sd = sd(angular_dev, na.rm = TRUE), 
            ci = vector_confint(angular_dev),
            n = n())

nocursors_mini <- nocur_means %>%
  group_by(condition) %>%
  summarise(condition_mean_dev = mean(mean_devs, na.rm = TRUE), 
            sd = sd(mean_devs, na.rm = TRUE), 
            ci = vector_confint(mean_devs),
            n = n())


p <- ggplot(nocur_means, mapping = aes(condition, mean_devs, colour = condition)) +
  geom_beeswarm(size = 3, alpha = 0.6, dodge.width = 0.4) +
  geom_point(data = nocursors_mini, aes(y = condition_mean_dev), size = 14, alpha = 0.6, 
             position = position_dodge(width = .4)) + 
  geom_linerange(data = nocursors_mini, aes(y = condition_mean_dev, ymin = condition_mean_dev - ci, ymax = condition_mean_dev + ci), 
                 lwd = 20, alpha = 0.4, 
                 position = position_dodge(width = .4)) +
  scale_y_continuous(name = "hand deviation (°)") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour = "#CCCCCC")) +
  NULL
p

p <- p +
  theme(text = element_text(size=40), 
        axis.text = element_text(size=40), 
        legend.text = element_text(size=48))

ggsave(p, height = 13, width = 10, device = "svg", filename = "data/online_nc_plot.svg")



### temp (over trials)
nocursors_60 <- omnibus_online %>%
  filter(cursor_vis == 0, trial_num > 100)

nocur_means <- nocursors_60 %>%
  group_by(condition, trial_num) %>%
  summarise(mean_devs = mean(angular_dev, na.rm = TRUE), 
            sd = sd(angular_dev, na.rm = TRUE), 
            ci = vector_confint(angular_dev),
            n = n())

p <- ggplot(nocur_means, mapping = aes(trial_num, mean_devs, colour = condition)) +
  geom_point(stat = "identity", size = 3) +
  scale_y_continuous(name = "hand deviation (°)") +
  scale_x_continuous(name = "trial") +
  theme_minimal() +
  theme(panel.grid.major.y = element_line(colour = "#CCCCCC")) +
  NULL
p

p <- p +
  theme(text = element_text(size=40), 
        axis.text = element_text(size=40), 
        legend.text = element_text(size=48))

ggsave(p, height = 13, width = 10, device = "svg", filename = "data/online_nc_plot.svg")

