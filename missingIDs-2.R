# missingIDs-2.R
# Function to input FIT data element numeric IDs where missing from survey responses
# 
# Created: 5/16/2018
# 
# Purpose: Use this function to replace all missing numeric IDs with the numeric ID from the
#     Framework database. The matches were made manually outside of R.
#
# Workflow: 
#   1. read the csv file containing errors and their corresponding corrections ("corrections")
#   2. shorten imported file to pertinant columns
#   3. use a FOR loop to insert IDs where they are missing
# 
# Usage:
#   missingIDs(testdat,corrections)
#
# Arguments:
#   surveyData = data frame, previously imported
#   correctionsData = full path to CORRECTIONS.CSV file that contains the corrections to be made

missingIDs<-function(surveyData, correctionsData) {
  a<-read.csv(correctionsData, header = TRUE, sep = ",", stringsAsFactors = TRUE)
  corrections<-a[,c("rowID","DataElem","newID")]
  # corrections<-correctionsData[,c("rowID","newID")]
  for(i in 1:nrow(corrections)) {
    print(corrections[i,"rowID"])
    print(corrections[i,"newID"])
    surveyData[i,10]<-corrections[i,"newID"]
  }
  return(surveyData)
}