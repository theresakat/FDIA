# 1_load.R

# Purpose; Use this script to load your CSV data for clean up using the 2_clean.R script. 

# Sample loading code that reads a directory #
# library(plyr) 
# 
# files <- dir("raw", full = T) 
# names(files) <- gsub("\\.csv", "", dir("raw")) 
#    
# # Load all csv files into a single data frame and give informative column names   
# bnames <- ldply(files, read.csv, header = F, skip = 1, nrows = 1000, stringsAsFactors = FALSE) 
# names(bnames) <- c("file", "rank", "boy_name", "boy_num", "girl_name", "girl_num") 

# Source the functions & libraries #
#source("C:\\temp\\BLM Leks to Landscapes Project_287315\\Analysis\\SpatialScaling_task2\\Friedmans\\code\\func.r")

library(tidyr)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)

# Set working directory
setwd("C:\\temp\\FDIA-Analysis-in-R")

# Set path to input CSV
myfile<-"C:\\temp\\FDIA-Analysis-in-R\\CSV\\Framework Data Inventory  Assessment v. 1.0.csv"

# Read the CSV file (Survey Monkey provides a CSV in its downloads)
x<-read.csv(myfile,header=F, sep=",", skip = 2)
