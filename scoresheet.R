# Scoresheet.R

# Purpose: this script is used to create the scoresheet tally of data elements by theme
# 
# Required files:
#   dat   contains the contents of the 1_tblFrameworkData_MASTER.csv
#   x     contains the raw imported survey data

# Create empty data frame
de<- data.frame(ID=c(1:length(x[,1])))

# Add survey responses for data element names and IDs to the data frame 
de<-data.frame(x[,10:11])
names(de)<-c("DataElem","ID")

# Merge the Framework theme info and the survey data responses
y<-merge(de,dat, all.x=TRUE)

# Correct the known errors
# add some cleaning functions here

# Create a selection vector to identify the unmatched survey responses
select<-which(is.na(y$ID))

# Export the results to CSV
mywd<-"C:\\temp\\FDIA"
setwd(mywd)
outfile<-paste(mywd,"\\CSV\\errors.csv", sep="")
write.table(y[select,1:2], outfile, sep = ",", na="NA")
