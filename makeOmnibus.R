## --------------------------------
##
## Script name: makeOmnibus.R
##
## Purpose of script: Make omnibus dataframes -- one for no-cursor data, one for cursor data
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-07-09
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: Uses selected files as input
## 
## For no-cursor data:
## Required headers: pptNum (unique), experiment, trainedRot (angle), stratUse (exc or inc)
##
## --------------------------------

## ----
## Load packages
library(data.table)
library(tidyverse)
## ----
## Functions
# function for making the omnubus thing

applyAtan2 <- function(df){
  return(((atan2(df[2] - df[4], df[1] - df[3]) * 180/pi) %% 360) - df[5]) # atan2(y,x) -- atan2 takes y first
}

applyStratUse <- function(taskName){
  if (grepl("include", taskName) | taskName == 1){
    return (1)
  }
  else {
    return (0)
  }
}

makeNoCurOmnibus <- function(){
  
  path <- "data/selected"
  datalist <- list()
  i <- 1
  
  # load a df
  for (expVersion in list.files(path = path)){
    for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
      
      # make a vector of filenames to load (these are entire paths)       
      filesToLoad <- 
        list.files(path = paste(path, expVersion, ppt, sep = '/'), 
                                pattern = glob2rx("*no-cursor*selected*|*no_cursor*selected*"), 
                                full.names = TRUE)
      
      # filter out the practice trials if there are any
      filesToLoad <-
        filesToLoad[!grepl("practice",filesToLoad)]
      
      for (filePath in filesToLoad){
        
        df <- fread(filePath, stringsAsFactors = FALSE)
        
        # fix column names (any data from MATLAB select gets "VX" colnames; only stepwise rn)
        if (colnames(df)[1] == "V1"){ 
          colnames(df) <- c("task_num", "task_name", "trial_type", "trial_num", 
                                       "terminalfeedback_bool", "rotation_angle", "targetangle_deg", 
                                       "targetdistance_percmax", "homex_px", "homey_px", "targetx_px", 
                                       "targety_px", "time_s", "mousex_px", "mousey_px", "cursorx_px", 
                                       "cursory_px", "selected", "P", "?", "maxV", "??")
        }
        
        maxVrows <- df[df$selected == 1 & df$maxV == 1, task_num:cursory_px]
        
        #add new columns
        maxVrows$ppt <- paste(ppt, expVersion, sep = '_')
        
        if (expVersion == "abruptExp"){
          maxVrows$rotation_angle[maxVrows$task_num >= 14] <- -60
        } 
        
        if ((expVersion %in% c("longAbruptExp", "gradualExp")) & grepl("rotated", filePath)) {
          maxVrows$rotation_angle <- -60
        }
        
        
        # add in whether the trial was instructed or not
        if (typeof(maxVrows$trial_type) == "character" | expVersion %in% c("longAbruptExp", "gradualExp")) {
          maxVrows$stratuse <- lapply(maxVrows$task_name, applyStratUse)
        }
        else {
          if (grepl("include", filePath)){
            maxVrows$stratuse <- 1
          } else {
            maxVrows$stratuse <- 0
          }
        }
        
        maxVrows$exp <- expVersion
        
        # save this one df to datalist
        datalist[[i]] <- maxVrows
        
        i <- i+1
      }
    }  
  }
  
  omnibus_nocur <- do.call(rbind, datalist)
  
  
  colnames(omnibus_nocur) <- c("task_num", "task_name", "trial_type", "trial_num", 
                               "terminalfeedback_bool", "rotation_angle", "targetangle_deg", 
                               "targetdistance_percmax", "homex_px", "homey_px", "targetx_px", 
                               "targety_px", "time_s", "mousex_px", "mousey_px", "cursorx_px", 
                               "cursory_px", "ppt", "stratuse", "exp")
  
  # get the angles
  omnibus_nocur$angular_dev <- apply(omnibus_nocur[ , c('mousex_px', 'mousey_px', 'homex_px', 'homey_px', 'targetangle_deg')], 
                                     1, FUN = applyAtan2)
  
  # remove useless columns
  omnibus_nocur[ , c('task_name', 'trial_type', 'terminalfeedback_bool', 
                     'targetdistance_percmax')] <- NULL
  
  # save the omnibus df
  fwrite(omnibus_nocur, file = "data/omnibus/omnibus_nocur.csv")
  
}


makeTrainingOmnibus <- function(){
  path <- "data/selected"
  datalist <- list()
  i <- 1
  for (expVersion in c("longAbruptExp", "stepwiseExp", "gradualExp")){
    for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
      
      trial_counter <- 1
      
      # make a vector of filenames to load (these are entire paths)       
      filesToLoad <- list.files(path = paste(path, expVersion, ppt, sep = '/'), 
                                pattern = glob2rx("*training*selected*|*traning*selected*"), 
                                full.names = TRUE)
      
      # reorder if needed
      if (expVersion == "stepwiseExp"){
        filesToLoad <- filesToLoad[c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9)]
      }
      
      for (filePath in filesToLoad){
        
        df <- fread(filePath, stringsAsFactors = FALSE)
        
        maxVrows <- filter(df, selected == 1, maxV == 1)
        
        maxVrows$stratUse <- 0 #this is just filler
        
        #add new columns
        maxVrows$ppt <- paste(ppt, expVersion, sep = '_')
        
        # add exp version
        maxVrows$exp <- expVersion
        
        # get rid of these columns if they occur
        maxVrows$old_trial_num <- NULL
        
        maxVrows$trial_num_cont <- seq(from = trial_counter, length.out =nrow(maxVrows))
        # save this one df to datalist
        datalist[[i]] <- maxVrows
        
        i <- i+1
        trial_counter <- trial_counter + nrow(maxVrows)
      }
    }  
  }
  
  omnibus_training <- do.call(rbind, datalist)
  
  # get the angles
  omnibus_training$angular_dev <- apply(omnibus_training[ , c('mousex_px', 'mousey_px', 'homex_px', 'homey_px', 'targetangle_deg')], 
                                     1, FUN = applyAtan2)
  
  # remove useless columns
  omnibus_training[ , c('task_name', 'trial_type', 'terminalfeedback_bool', 
                     'targetdistance_percmax')] <- NULL
  
  # save the omnibus df
  fwrite(omnibus_training, file = "data/omnibus/omnibus_training.csv")
}

## ----
## Run the functions
library(future)
plan(multiprocess)

#NOTE: %<-% is a "future assignment"
# tempjob1 %<-% makeNoCurOmnibus()
tempjob2 %<-% makeTrainingOmnibus()

temp.list <- lapply(ls(pattern = "temp"), get)










# testing
test <- data.frame("x" = c(1, 2, 3, 4, 5, 6, 7, 8))

test$y <- c(1, 2, 3, 4, 5, 6, 7, 8)
test$x <- factor(test$x)

p <- ggplot (data = test, aes(x, y, colour = x)) +
  geom_point() +
  scale_colour_brewer()

p

ggsave(p, height = 13, width = 20, device = "svg", filename = "data/test.svg")
