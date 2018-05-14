# Scoresheet.R

# Purpose: this script is used to create the scoresheet tally of data elements by theme
# 
# Required files:
#   dat   contains the contents of the 1_tblFrameworkData_MASTER.csv
#   x     contains the raw imported survey data

# Workflow
#   import data to R
#   identify and export mismatched records
#   correct data element names
#   add data element numeric IDs where missing
#   apply this script

# Create empty data frame
de<- data.frame(ID=c(1:length(x[,1])))

# Add survey responses for data element names and IDs to the data frame 
de<-data.frame(x[,10:11])
names(de)<-c("DataElem","ID")

# Merge the Framework theme info and the survey data responses
y<-merge(de,dat, all.x=TRUE)

# Create a selection vector to identify the unmatched survey responses
select<-which(is.na(y$ID))

# Export the results to CSV to use to perform corrections
mywd<-"C:\\temp\\FDIA"
setwd(mywd)
outfile<-paste(mywd,"\\CSV\\errors.csv", sep="")
write.table(y[select,c("ID","DataElem")], outfile, sep = ",", row.name=FALSE)

# Correct the known errors
# add some cleaning functions here

