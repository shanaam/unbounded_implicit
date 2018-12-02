###############################
## Rigid Implicit Experiment ##
###############################

#set the working directory to wherever this file is located
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

options(stringsAsFactors = FALSE)

rawDataEI_trial <- read.csv("data/multipleRotation/rigid_implicit_exp_EI_trial.csv")
rawDataIE_trial <- read.csv("data/multipleRotation/rigid_implicit_exp_IE_trial.csv")

#merging the data will mess up the include/exclude strategy values

#####
##functions

##given a string and a df, grabs all rows that include that string in the "task" column
GetRowsWithString <- function (string, df) {
  return(df[grep(string, df$task), ])
}

##given a string and 2 dfs, grabs all rows that include that string in the "task" column, and merges. Uses GetRowsWithString()
GetAllRowsWithString <- function(string,df1,df2) {
  df1RelevantRows <- GetRowsWithString(string, df1)
  df2RelevantRows <- GetRowsWithString(string, df2)
  
  mergedRelevantRows <- merge(df1RelevantRows, df2RelevantRows)
  return(mergedRelevantRows)
}


##given a df (in format colnames = [task, trial, rotation_angle_deg, p1, p2, ...]), finds the median of all rows for each participants
##returns the original df with a median row binded onto the end
MedianOfAllValuesPerColumn <- function(df){
  
  subsetOfDF <- subset(df, select=-c(task, trial))
  medianList <- lapply(subsetOfDF, median)
  
  medianList$task <- "median"
  medianList$trial <- NA
  #medianList$rotation_angle_deg <- NA
  
  return(rbind(df, medianList))
  
}


#function to make cleaned data file given the two raw data files
MakeNoCursorClean <- function(rawDataIE,rawDataEI){
  excludeData <- GetAllRowsWithString("exclude", rawDataEI, rawDataIE)
  includeData <- GetAllRowsWithString("include", rawDataEI, rawDataIE)
  baselineNoCursorData <- GetAllRowsWithString("no-cursor_baseline", rawDataEI, rawDataIE)
  
  #per participant
  #get baseline
  
  #required for adding a string ("median") for the task column
  baselineNoCursorData <- MedianOfAllValuesPerColumn(baselineNoCursorData)
  
  #get rows with "_15_", "_30_", "_45_", and "_60_" separately
  #from exclude data
  exclude15 <- GetRowsWithString("_15_", excludeData)
  exclude15 <- MedianOfAllValuesPerColumn(exclude15)
  
  exclude30 <- GetRowsWithString("_30_", excludeData)
  exclude30 <- MedianOfAllValuesPerColumn(exclude30)
  exclude45 <- GetRowsWithString("_45_", excludeData)
  exclude45 <- MedianOfAllValuesPerColumn(exclude45)
  exclude60 <- GetRowsWithString("_60_", excludeData)
  exclude60 <- MedianOfAllValuesPerColumn(exclude60)
  
  #get only the median rows
  baselineMedians <- baselineNoCursorData[baselineNoCursorData$task == "median", ]
  baselineMedians$task <- "baseline"
  exclude15Medians <- exclude15[exclude15$task == "median", ]
  exclude15Medians$task <- "exclude 15deg"
  exclude30Medians <- exclude30[exclude30$task == "median", ]
  exclude30Medians$task <- "exclude 30deg"
  exclude45Medians <- exclude45[exclude45$task == "median", ]
  exclude45Medians$task <- "exclude 45deg"
  exclude60Medians <- exclude60[exclude60$task == "median", ]
  exclude60Medians$task <- "exclude 60deg"
  
  
  ##repeat for include
  include15 <- GetRowsWithString("_15_", includeData)
  include15 <- MedianOfAllValuesPerColumn(include15)
  
  include30 <- GetRowsWithString("_30_", includeData)
  include30 <- MedianOfAllValuesPerColumn(include30)
  include45 <- GetRowsWithString("_45_", includeData)
  include45 <- MedianOfAllValuesPerColumn(include45)
  include60 <- GetRowsWithString("_60_", includeData)
  include60 <- MedianOfAllValuesPerColumn(include60)
  
  #get only the median rows
  include15Medians <- include15[include15$task == "median", ]
  include15Medians$task <- "include 15deg"
  include30Medians <- include30[include30$task == "median", ]
  include30Medians$task <- "include 30deg"
  include45Medians <- include45[include45$task == "median", ]
  include45Medians$task <- "include 45deg"
  include60Medians <- include60[include60$task == "median", ]
  include60Medians$task <- "include 60deg"
  
  #merge all the Medians rows
  
  masterCSV <- rbind(baselineMedians, exclude15Medians, exclude30Medians, exclude45Medians, exclude60Medians, include15Medians, include30Medians, include45Medians, include60Medians)
  masterCSV$task <- NULL
  masterCSV$trial <- NULL
  masterCSV$rotation_angle_deg <- NULL
  
  transposedMasterCSV <- t(masterCSV)
  
  colnames(transposedMasterCSV) <- c("baselineMedians", "exclude15Medians", "exclude30Medians", "exclude45Medians", "exclude60Medians", "include15Medians", "include30Medians", "include45Medians", "include60Medians")
  
  write.csv(transposedMasterCSV, file= "data/noCursorMedians.csv")
}

MakeNoCursorClean(rawDataIE_trial, rawDataEI_trial)
