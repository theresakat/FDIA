# missingIDs-2.R
# Function to input FIT data element numeric IDs where missing from survey responses
# 
# Created: 5/16/2018
# 
# Purpose: Use this function to replace all missing numeric IDs with the numeric ID from the
#     Framework database. The matches were made manually outside of R.
#
# Steps: 
#   1. read the table containing errors and their corresponding corrections ("corrections")
#   2. 
# 
# 


missingIDs<-function(surveyData, correctionsData) {
  # a<-read.csv(correctionsData, header = TRUE, sep = ",", stringsAsFactors = TRUE)
  # corrections<-correctionsData[,c("rowID","DataElem","newID")]
  corrections<-correctionsData[,c("rowID","newID")]
  for(i in 1:nrow(corrections)) {
    print(corrections[i,"rowID"])
    print(corrections[i,"newID"])
    surveyData[i,"var3"]<-corrections[i,"newID"]
  }
  return(surveyData)
}