## processSteps-notes.R
# 4-19-2018
#
# PURPOSE: This script merges multple response columns into a single column.
#
#

# STEP 1. Read in the column headings table (LOAD)

# STEP 2. Read in the data to be merged (LOAD)

# STEP 3. Generate the code for merging (CLEAN)

# STEP 4. Merging (CLEAN)
dd<-unite(d,"response",1:3, sep = "_")
