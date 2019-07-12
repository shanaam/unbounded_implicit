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
      dir.create(paste('data/raw/toSelect/stepwiseExp/', participant, sep = ''))
      
      filesToLoad <- list.files(path = filePath, pattern = "Complete")
      
      for (file in filesToLoad){
        fileToLoad <- paste(dataPathStepWise, expVersion, participant, file, sep = '/')
        rawDataFile <- fread(fileToLoad, stringsAsFactors = FALSE)
        rawDataFile$task_name <- 1
        rawDataFile$trial_type <- 1
        rawDataFile$terminalfeedback_bool <- 0
        
        taskName = substr(file, 1, nchar(file) - 13)
  
        write.table(rawDataFile, file = paste('data/raw/toSelect/stepwiseExp/', participant, '/', participant, '_', taskName, '.txt', sep = ''), sep = '\t', row.names = FALSE)
      }
    }
  }
}

## ----
## Run the above functions

library(future)
plan(multiprocess)

#NOTE: %<-% is a "future assignment"
tempjob1 %<-% makeSelectable_abrupt()
tempjob2 %<-% makeSelectable_stepwise()

temp.list <- lapply(ls(pattern = "temp"), get)
