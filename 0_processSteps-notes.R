## processSteps-notes.R
# 4-19-2018
#
# PURPOSE: This document describes the process steps for the survey data.
#
#

# STEP 1. 
#Read in the data to be merged (LOAD)
#

# STEP 2. 
#Generate the code for merging (CLEAN)

# STEP 3. 
#Merging (CLEAN)
dd<-unite(d,"response",1:3, sep = "_")

# Other steps:

#(a) Identify the variables that will be used for calculating maturity: use subsetting to do accomplish this

#(b) Calculate maturity over multiple variables and outputing the index to a new variable

#(c) Create a score card showing the expected vs. observed responses for each theme based on the original input data elements.

#(d) Create a score card showing the expected vs. observed responses for each theme based on the reduced input data elements.
#     by subtracting the number of data elements identified for omission

#(e) Split Names into First and Last variables using strsplit() function

