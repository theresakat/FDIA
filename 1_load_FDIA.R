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
library(dplyr)
library(reshape2)
library(ggplot2)
library(rlang)
library(rapportools)

# Set working directory and path to input CSV on Windows laptop
mywd<-"C:\\temp\\FDIA"
setwd(mywd)
# myfile<-paste(mywd,"\\CSV\\Framework Data Inventory  Assessment v. 1.0.csv", sep = "")
myfile<-paste(mywd,"\\CSV\\data_num.csv", sep = "")
scores<-paste(mywd,"\\CSV\\Scoring.csv", sep="")

# Set working directory and path to input CSV on Mac
# mywd<-"/Users/tkb/Work/GEO/fdia-mac"
# setwd(mywd)
# myfile<-paste(mywd,"/CSV/Framework Data Inventory  Assessment v. 1.0.csv", sep = "")

# Read the CSV file (Survey Monkey provides a CSV in its downloads)
x<-read.csv(c(myfile),header=F, sep=",", skip = 3)
scoring<-read.csv(c(scores), header=T, sep=",", nrows = 267)
