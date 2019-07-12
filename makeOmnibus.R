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

## ----
## Functions
# function for making the omnubus thing

applyAtan2 <- function(df){
  return(((atan2(df[2] - df[4], df[1] - df[3]) * 180/pi) %% 360) - df[5]) # atan2(y,x) -- atan2 takes y first
}

makeNoCurOmnibus <- function(){
  
  path <- "data/selected"
  datalist <- list()
  i <- 1
  
  # load a df
  for (expVersion in list.files(path = path)){
    for (ppt in list.files(path = paste(path, expVersion, sep = '/'))){
      
      # make a vector of filenames to load (these are entire paths)       
      filesToLoad <- list.files(path = paste(path, expVersion, ppt, sep = '/'), 
                                pattern = glob2rx("*no-cursor*selected*"), 
                                full.names = TRUE)
      
      for (filePath in filesToLoad){
        
        df <- fread(filePath, stringsAsFactors = FALSE)
        
        maxVrows <- df[df$V21 == 1 & df$V18 == 1, V1:V17]
        
        #add new columns
        maxVrows$V18 <- paste(ppt, expVersion, sep = '_')
        
        if (expVersion == "abruptExp"){
          maxVrows$V6[maxVrows$V1 >= 14] <- -60
        } 
        
        # add in whether the trial was instructed or not
        if (grepl("include", filePath)){
          maxVrows$V19 <- 1
        }
        else {
          maxVrows$V19 <- 0
        }
        
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
                               "cursory_px", "ppt", "stratuse")
  
  # get the angles
  omnibus_nocur$angular_dev <- apply(omnibus_nocur[ , c('cursorx_px', 'cursory_px', 'homex_px', 'homey_px', 'targetangle_deg')], 
                                     1, FUN = applyAtan2)
  
  # remove useless columns
  omnibus_nocur[ , c('task_name', 'trial_type', 'terminalfeedback_bool', 
                     'targetdistance_percmax')] <- NULL
  
  # save the omnibus df
  fwrite(omnibus_nocur, file = "data/omnibus/omnibus_nocur.csv")
  
}


## ----
## Run the functions
makeNoCurOmnibus()
