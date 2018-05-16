# missingIDs-3.R
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
  corrections<-a[,c("DataElem","newID")]
  for(i in corrections$DataElem) {
    # xtest2[ which(xtest2$V10==i), 11]<-testcors[ which(testcors$DataElem==i),"newID"]
    surveyData[ which(surveyData$V10==i), 11]<-corrections[ which(corrections$DataElem==i),"newID"] # need to set up a search for "i" in surveyData
  }
  return(surveyData) # the results aren't being saved
}