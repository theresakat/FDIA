# cleanSurveyData.R
# 
# Purpose
# Script used to clean errors in the raw data:
#   - add numeric IDs where missing
#   
# Future enhancements
#   - align spelling of data elements with Framework MASTER
#   
# Prerequisites
#   surveyData      loaded using the 2_importData.R
#   dat             Framework MASTER table loaded using 2_importData.R
# 

################################################

### Set environment - Windows
# Errors in the raw survey data will go here
mywd<-"C:\\temp\\FDIA"
setwd(mywd)
outfile<-paste(mywd,"\\CSV\\errors_20180806.csv", sep="")

# Corrections will come from here
correctionsData<-"C:\\temp\\FDIA\\CSV\\corrections_20180604.csv"

### Set environment - MAC
# Errors in the raw survey data will go here
mywd<-"C:/temp/FDIA"
setwd(mywd)
outfile<-paste(mywd,"/CSV/errors_20180806.csv", sep="")

# Corrections will come from here
correctionsData<-"/Users/tkb/Work/GEO/fdia-mac/CSV/corrections.csv"


### Data processing steps start below ###

# Create empty data frame
de <- data.frame(ID=c(1:nrow(surveyData)))

# Add survey responses for data element names and IDs to the empty data frame 
de<-data.frame(surveyData[,10:11])
names(de)<-c("DataElem","ID")

# Merge the survey response variables and Framework MASTER table to match responses with themes
y<-merge(de,dat, all.x=TRUE)

# Create a selection vector to identify the survey responses that don't match the Framework MASTER table
select<-which(is.na(y$ID))

### Correct survey response data element names and IDs ###

# Export missing ID errors to CSV to match errors to their corrections. Use Excel to create the error-to-correction LUT
write.table(y[select,c("ID","DataElem")], outfile, sep = ",", row.name=FALSE)

# Reminder message
message<-c("    Next step: Use Excel to build the error-to-corrections LUT from ")
cat("\n",c(message,outfile),"\n")

# MANUALLY IN EXCEL --> correct the known errors using corrections LUT called "correctionsData" 
# (the exported errors from above with the corrections added)

# Read in the corrections LUT
a<-read.csv(correctionsData, header = TRUE, sep = ",", stringsAsFactors = TRUE)
corrections<-a[,c("DataElem","newID")]
for(i in corrections$DataElem) {
  surveyData[ which(surveyData$V10==i), 11]<-corrections[ which(corrections$DataElem==i),"newID"] }

# Return the data element IDs = NA
surveyData[which(is.na(surveyData$V11)),10:11]

# Inner join themes with the corrected survey data by ID
surveyDataThm <- merge(surveyData, dat, by.x = "V11", by.y = "ID") 

# Reminder message
message <- "Use \`surveyDataThm\` for creating the scoresheet tally. surveyDataThm can also be used as basis for uniting variables using \`clean-02-UniteSurveyData.R\`"
cat("\n",c(message))
