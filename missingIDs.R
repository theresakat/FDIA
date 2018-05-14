# missingIDs.R
# Function to input FIT data element numeric IDs where missing from survey responses
# 
# Created: 5/14/2018
# 
# Purpose: Use this function to replace all missing numeric IDs with the numeric ID from the 
#     Framework database. The matches were made manually outside of R.

missingIDs<-function(surveyData, correctionsData) {
 # a<-read.csv(correctionsData, header = TRUE, sep = ",", stringsAsFactors = TRUE)
 # corrections<-correctionsData[,c("rowID","DataElem","newID")]
  corrections<-correctionsData[,c("rowID","newID")]
  for(i in corrections$rowID) {
    for(j in 1:length(corrections$rowID)){
      surveyData[i,"X1"]<-corrections[j,"newID"]
      
      return(surveyData)
      }
    return(surveyData)
    }
}