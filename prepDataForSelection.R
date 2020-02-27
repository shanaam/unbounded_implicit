## --------------------------------
##
## Script name: prepDataForSelection.R
##
## Purpose of script: prepare unbounded implicit data for selecting
##
## Author: Shanaa Modchalingam
##
## Date created: 2019-05-28
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
## Notes: Uses raw files from PyVMEC as input -- outputs selectable data files
## 
## Trial numbers must not repeat
##
## --------------------------------

## ----
## Load packages
library(data.table)
library(tidyverse)

## ----
## function for data from the abrupt experiment

makeSelectable_abrupt <- function() {
    
  dataPathAbrupt <- 'data/raw/abruptExp'
  
  for (expVersion in list.files(path = dataPathAbrupt)){
    for (participant in list.files(path = paste(dataPathAbrupt, expVersion, sep = '/'))){
      
      filePath <- paste(dataPathAbrupt, expVersion, participant, sep = '/')
      dir.create(paste('data/raw/toSelect/abruptExp/', participant, sep = ''))
      
      fileToLoad <- list.files(path = filePath, pattern = "COMPLETE", full.names = TRUE)
      
      rawDataFile <- fread(fileToLoad, stringsAsFactors = FALSE)
      
      #separate the files by unique names in task_name
      for (taskName in unique(rawDataFile$task_name)) {
        fileToSave <- rawDataFile[rawDataFile$task_name == taskName, ]
        fileToSave$task_name <- 1
        fileToSave$trial_type <- 1
        fileToSave$terminalfeedback_bool <- 0
        
        write.table(fileToSave, file = paste('data/raw/toSelect/abruptExp/', participant, '/', participant, '_', taskName, '.txt', sep = ''), sep = '\t', row.names = FALSE)
      }
    }
  }
}

## ----
## function for data from the abrupt experiment

makeSelectable_stepwise <- function() {
  dataPathStepWise <- 'data/raw/stepwiseExp'
  
  for (expVersion in list.files(path = dataPathStepWise)){
    for (participant in list.files(path = paste(dataPathStepWise, expVersion, sep = '/'))){
      
      filePath <- paste(dataPathStepWise, expVersion, participant, sep = '/')
      dir.create(paste('data/toSelect/stepwiseExp/', participant, sep = ''))
      
      filesToLoad <- list.files(path = filePath, pattern = "Complete")
      
      taskCounter <- 1
      taskList <- list()
      
      for (file in filesToLoad){
        fileToLoad <- paste(dataPathStepWise, expVersion, participant, file, sep = '/')
        rawDataFile_temp <- fread(fileToLoad, stringsAsFactors = FALSE)
        # rawDataFile$task_name <- 1
        # rawDataFile$trial_type <- 1
        rawDataFile_temp$terminalfeedback_bool <- 0
        
        taskList[[taskCounter]] <- rawDataFile_temp
        
        taskCounter <- taskCounter + 1
      }
        
      # merge the files
      rawDataFile <- do.call(rbind, taskList)  
        
      # reorder rows
      rawDataFile <- rawDataFile %>%
        arrange(task_num, trial_num, time_s)
      
      # get unique numbers for tasks
      trialCounter <- 1
      trialList <- list()
      
      for(uq_task_num in unique(rawDataFile$task_num)){
        
        df <- 
          rawDataFile %>%
          filter(task_num == uq_task_num)
        
        for(uq_trial_num in unique(df$trial_num)) {
          
          trialList[[trialCounter]] <- rep(trialCounter, times = nrow(filter(df, trial_num == uq_trial_num)))
          
          trialCounter <- trialCounter + 1
        }
      }
      
      uq_trials <- do.call(c, trialList)
      
      rawDataFile$old_trial_num <- rawDataFile$trial_num
      rawDataFile$trial_num <- uq_trials
      
      # add a stratUse column
      rawDataFile$stratUse <- as.numeric(grepl("include", rawDataFile$task_name))
        
      # remove practice trials
      rawDataFile <-
        rawDataFile %>%
        filter(!grepl("practice",task_name))
      
      # recode
      rawDataFile$terminalfeedback_bool <- 0
      
      # make aligned no_cursor
      nocur_aligned_df <- 
        rawDataFile %>%
        filter(trial_type == "no_cursor", grepl("baseline",task_name))
      nocur_aligned_df$trial_type <- 0
      nocur_aligned_df$task_name <- as.numeric(grepl("include", nocur_aligned_df$task_name))
      
      for(uq_trial_num in unique(nocur_aligned_df$trial_num)) {
        nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s <-
          nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s -
          nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
      }
      
      
      cursor_aligned_df <- 
        rawDataFile %>%
        filter(trial_type == "cursor", grepl("aligned",task_name))
      cursor_aligned_df$trial_type <- 0
      cursor_aligned_df$task_name <- as.numeric(grepl("include", cursor_aligned_df$task_name))
      
      for(uq_trial_num in unique(cursor_aligned_df$trial_num)) {
        cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s <-
          cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s -
          cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
      }
      
      
      nocur_rotated_df <- 
        rawDataFile %>%
        filter(trial_type == "no_cursor", !grepl("baseline",task_name))
      nocur_rotated_df$trial_type <- 0
      nocur_rotated_df$task_name <- as.numeric(grepl("include", nocur_rotated_df$task_name))
      
      for(uq_trial_num in unique(nocur_rotated_df$trial_num)) {
        nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s <-
          nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s -
          nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
      }
      
      
      cursor_rotated_df <- 
        rawDataFile %>%
        filter(trial_type == "cursor", !grepl("aligned",task_name))
      cursor_rotated_df$trial_type <- 0
      cursor_rotated_df$task_name <- as.numeric(grepl("include", cursor_rotated_df$task_name))
      
      for(uq_trial_num in unique(cursor_rotated_df$trial_num)) {
        cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s <-
          cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s -
          cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
      }
      
      # taskName = substr(file, 1, nchar(file) - 13) #??

      # save
      write.table(nocur_aligned_df, file = paste('data/toSelect/stepwiseExp/', participant, '/', participant, '_aligned_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
      write.table(cursor_aligned_df, file = paste('data/toSelect/stepwiseExp/', participant, '/', participant, '_aligned_training.txt', sep = ''), sep = '\t', row.names = FALSE)
      write.table(nocur_rotated_df, file = paste('data/toSelect/stepwiseExp/', participant, '/', participant, '_rotated_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
      write.table(cursor_rotated_df, file = paste('data/toSelect/stepwiseExp/', participant, '/', participant, '_rotated_training.txt', sep = ''), sep = '\t', row.names = FALSE)
      
    }
  }
}


makeSelectable_longAbrupt <- function() {
  dataPathlongAbrupt <- 'data/raw/longAbruptExp'
  
  for (expVersion in list.files(path = dataPathlongAbrupt)){
    for (participant in list.files(path = paste(dataPathlongAbrupt, expVersion, sep = '/'))){
      
      filePath <- paste(dataPathlongAbrupt, expVersion, participant, sep = '/')
      dir.create(paste('data/toSelect/longAbruptExp/', participant, sep = ''))
      
      filesToLoad <- list.files(path = filePath, pattern = "COMPLETE")
      
      for (file in filesToLoad){
        fileToLoad <- paste(dataPathlongAbrupt, expVersion, participant, file, sep = '/')
        rawDataFile <- fread(fileToLoad, stringsAsFactors = FALSE)
        # rawDataFile$task_name <- 1
        # rawDataFile$trial_type <- 1
        rawDataFile$terminalfeedback_bool <- 0
        
        trialCounter <- 1
        trialList <- list()
        
        for(uq_task_num in unique(rawDataFile$task_num)){
          
          df <- 
            rawDataFile %>%
            filter(task_num == uq_task_num)
          
          for(uq_trial_num in unique(df$trial_num)) {
            
            trialList[[trialCounter]] <- rep(trialCounter, times = nrow(filter(df, trial_num == uq_trial_num)))
            
            trialCounter <- trialCounter + 1
          }
        }
        
        uq_trials <- do.call(c, trialList)
        
        rawDataFile$old_trial_num <- rawDataFile$trial_num
        rawDataFile$trial_num <- uq_trials
        
        # add a stratUse column
        rawDataFile$stratUse <- as.numeric(grepl("include", rawDataFile$task_name))
        
        # taskName = substr(file, 1, nchar(file) - 13)
        
        # remove practice trials
        rawDataFile <-
          rawDataFile %>%
          filter(!grepl("practice",task_name))
        
        # recode
        rawDataFile$terminalfeedback_bool <- 0
        
        
        nocur_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", !grepl("_60",task_name))
        nocur_aligned_df$trial_type <- 0
        nocur_aligned_df$task_name <- as.numeric(grepl("include", nocur_aligned_df$task_name))
        
        for(uq_trial_num in unique(nocur_aligned_df$trial_num)) {
          nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s -
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", !grepl("_60",task_name))
        cursor_aligned_df$trial_type <- 0
        cursor_aligned_df$task_name <- as.numeric(grepl("include", cursor_aligned_df$task_name))
        
        for(uq_trial_num in unique(cursor_aligned_df$trial_num)) {
          cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s -
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        nocur_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", grepl("_60",task_name))
        nocur_rotated_df$trial_type <- 0
        nocur_rotated_df$task_name <- as.numeric(grepl("include", nocur_rotated_df$task_name))
        
        for(uq_trial_num in unique(nocur_rotated_df$trial_num)) {
          nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s -
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", grepl("_60",task_name))
        cursor_rotated_df$trial_type <- 0
        cursor_rotated_df$task_name <- as.numeric(grepl("include", cursor_rotated_df$task_name))
        
        for(uq_trial_num in unique(cursor_rotated_df$trial_num)) {
          cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s -
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        write.table(nocur_aligned_df, file = paste('data/toSelect/longAbruptExp/', participant, '/', participant, '_aligned_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_aligned_df, file = paste('data/toSelect/longAbruptExp/', participant, '/', participant, '_aligned_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(nocur_rotated_df, file = paste('data/toSelect/longAbruptExp/', participant, '/', participant, '_rotated_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_rotated_df, file = paste('data/toSelect/longAbruptExp/', participant, '/', participant, '_rotated_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        
      }
    }
  }
}

makeSelectable_gradual <- function() {
  dataPathGradual <- 'data/raw/gradualExp'
  
  for (expVersion in list.files(path = dataPathGradual)){
    for (participant in list.files(path = paste(dataPathGradual, expVersion, sep = '/'))){
      
      filePath <- paste(dataPathGradual, expVersion, participant, sep = '/')
      dir.create(paste('data/toSelect/gradualExp/', participant, sep = ''))
      
      filesToLoad <- list.files(path = filePath, pattern = "COMPLETE")
      
      for (file in filesToLoad){
        fileToLoad <- paste(dataPathGradual, expVersion, participant, file, sep = '/')
        rawDataFile <- fread(fileToLoad, stringsAsFactors = FALSE)
        # rawDataFile$task_name <- 1
        # rawDataFile$trial_type <- 1
        rawDataFile$terminalfeedback_bool <- 0
        
        trialCounter <- 1
        trialList <- list()
        
        for(uq_task_num in unique(rawDataFile$task_num)){
          
          df <- 
            rawDataFile %>%
            filter(task_num == uq_task_num)
          
          for(uq_trial_num in unique(df$trial_num)) {
            
            trialList[[trialCounter]] <- rep(trialCounter, times = nrow(filter(df, trial_num == uq_trial_num)))
            
            trialCounter <- trialCounter + 1
          }
        }
        
        uq_trials <- do.call(c, trialList)
        
        rawDataFile$old_trial_num <- rawDataFile$trial_num
        rawDataFile$trial_num <- uq_trials
        
        # add a stratUse column
        rawDataFile$stratUse <- as.numeric(grepl("include", rawDataFile$task_name))
        
        # taskName = substr(file, 1, nchar(file) - 13)
        
        # remove practice trials
        rawDataFile <-
          rawDataFile %>%
          filter(!grepl("practice",task_name))
        
        # recode
        rawDataFile$terminalfeedback_bool <- 0
        
        
        nocur_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", grepl("baseline",task_name))
        nocur_aligned_df$trial_type <- 0
        nocur_aligned_df$task_name <- as.numeric(grepl("include", nocur_aligned_df$task_name))
        
        for(uq_trial_num in unique(nocur_aligned_df$trial_num)) {
          nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s -
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", grepl("aligned",task_name))
        cursor_aligned_df$trial_type <- 0
        cursor_aligned_df$task_name <- as.numeric(grepl("include", cursor_aligned_df$task_name))
        
        for(uq_trial_num in unique(cursor_aligned_df$trial_num)) {
          cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s -
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        nocur_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", !grepl("baseline",task_name))
        nocur_rotated_df$trial_type <- 0
        nocur_rotated_df$task_name <- as.numeric(grepl("include", nocur_rotated_df$task_name))
        
        for(uq_trial_num in unique(nocur_rotated_df$trial_num)) {
          nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s -
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", !grepl("aligned",task_name))
        cursor_rotated_df$trial_type <- 0
        cursor_rotated_df$task_name <- as.numeric(grepl("include", cursor_rotated_df$task_name))
        
        for(uq_trial_num in unique(cursor_rotated_df$trial_num)) {
          cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s -
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        write.table(nocur_aligned_df, file = paste('data/toSelect/gradualExp/', participant, '/', participant, '_aligned_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_aligned_df, file = paste('data/toSelect/gradualExp/', participant, '/', participant, '_aligned_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(nocur_rotated_df, file = paste('data/toSelect/gradualExp/', participant, '/', participant, '_rotated_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_rotated_df, file = paste('data/toSelect/gradualExp/', participant, '/', participant, '_rotated_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        
      }
    }
  }
}

makeSelectable_reintro <- function() {
  dataPathReintro <- 'data/raw/reintroExp'
  
  for (expVersion in list.files(path = dataPathReintro)){
    for (participant in list.files(path = paste(dataPathReintro, expVersion, sep = '/'))){
      
      filePath <- paste(dataPathReintro, expVersion, participant, sep = '/')
      dir.create(paste('data/toSelect/reintroExp/', participant, sep = ''))
      
      filesToLoad <- list.files(path = filePath, pattern = "COMPLETE")
      
      for (file in filesToLoad){
        fileToLoad <- paste(dataPathReintro, expVersion, participant, file, sep = '/')
        rawDataFile <- fread(fileToLoad, stringsAsFactors = FALSE)
        # rawDataFile$task_name <- 1
        # rawDataFile$trial_type <- 1
        rawDataFile$terminalfeedback_bool <- 0
        
        trialCounter <- 1
        trialList <- list()
        
        for(uq_task_num in unique(rawDataFile$task_num)){
          
          df <- 
            rawDataFile %>%
            filter(task_num == uq_task_num)
          
          for(uq_trial_num in unique(df$trial_num)) {
            
            trialList[[trialCounter]] <- rep(trialCounter, times = nrow(filter(df, trial_num == uq_trial_num)))
            
            trialCounter <- trialCounter + 1
          }
        }
        
        uq_trials <- do.call(c, trialList)
        
        rawDataFile$old_trial_num <- rawDataFile$trial_num
        rawDataFile$trial_num <- uq_trials
        
        # add a stratUse column
        rawDataFile$stratUse <- as.numeric(grepl("include", rawDataFile$task_name))
        
        # taskName = substr(file, 1, nchar(file) - 13)
        
        # remove practice trials
        rawDataFile <-
          rawDataFile %>%
          filter(!grepl("practice",task_name))
        
        # recode
        rawDataFile$terminalfeedback_bool <- 0
        
        
        nocur_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", grepl("baseline",task_name))
        nocur_aligned_df$trial_type <- 0
        nocur_aligned_df$task_name <- as.numeric(grepl("include", nocur_aligned_df$task_name))
        
        for(uq_trial_num in unique(nocur_aligned_df$trial_num)) {
          nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s -
            nocur_aligned_df[nocur_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_aligned_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", grepl("aligned",task_name))
        cursor_aligned_df$trial_type <- 0
        cursor_aligned_df$task_name <- as.numeric(grepl("include", cursor_aligned_df$task_name))
        
        for(uq_trial_num in unique(cursor_aligned_df$trial_num)) {
          cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s -
            cursor_aligned_df[cursor_aligned_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        nocur_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "no_cursor", !grepl("baseline",task_name))
        nocur_rotated_df$trial_type <- 0
        nocur_rotated_df$task_name <- as.numeric(grepl("include", nocur_rotated_df$task_name))
        
        for(uq_trial_num in unique(nocur_rotated_df$trial_num)) {
          nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s -
            nocur_rotated_df[nocur_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        cursor_rotated_df <- 
          rawDataFile %>%
          filter(trial_type == "cursor", !grepl("aligned",task_name))
        cursor_rotated_df$trial_type <- 0
        cursor_rotated_df$task_name <- as.numeric(grepl("include", cursor_rotated_df$task_name))
        
        for(uq_trial_num in unique(cursor_rotated_df$trial_num)) {
          cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s <-
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s -
            cursor_rotated_df[cursor_rotated_df$trial_num == uq_trial_num, ]$time_s[1]
        }
        
        write.table(nocur_aligned_df, file = paste('data/toSelect/reintroExp/', participant, '/', participant, '_aligned_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_aligned_df, file = paste('data/toSelect/reintroExp/', participant, '/', participant, '_aligned_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(nocur_rotated_df, file = paste('data/toSelect/reintroExp/', participant, '/', participant, '_rotated_no_cursor.txt', sep = ''), sep = '\t', row.names = FALSE)
        write.table(cursor_rotated_df, file = paste('data/toSelect/reintroExp/', participant, '/', participant, '_rotated_training.txt', sep = ''), sep = '\t', row.names = FALSE)
        
      }
    }
  }
}


## ----
## Run the above functions

library(future)
plan(multiprocess)

#NOTE: %<-% is a "future assignment"
# tempjob1 %<-% makeSelectable_abrupt()
# tempjob2 %<-% makeSelectable_stepwise()
# tempjob3 %<-% makeSelectable_longAbrupt()
# tempjob4 %<-% makeSelectable_gradual()
tempjob5 %<-% makeSelectable_reintro()

temp.list <- lapply(ls(pattern = "temp"), get)
