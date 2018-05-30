# Scoresheet.R

# Purpose: this script is used to create the scoresheet tally of data elements by theme
# 
# Required files:
#   dat   contains the contents of the 1_tblFrameworkData_MASTER.csv (load using 1_load_FDIA.R)
#   x     contains the raw imported survey data

# Workflow
#   import data to R
#   identify and export mismatched records
#   correct data element names
#   add data element numeric IDs where missing
#   apply this script

# Create empty data frame
de <- data.frame(ID=c(1:nrow(surveyData)))

# Add survey responses for data element names and IDs to the data frame 
de<-data.frame(surveyData[,10:11])
names(de)<-c("DataElem","ID")

# Merge the survey responses and Framework theme info
y<-merge(de,dat, all.x=TRUE)

# Create a selection vector to identify the unmatched survey responses
select<-which(is.na(y$ID))

# Export the results to CSV to use to perform corrections. Use Excel to create the error to correction LUT
# mywd<-"C:\\temp\\FDIA"
# setwd(mywd)
# outfile<-paste(mywd,"\\CSV\\errors.csv", sep="")
# write.table(y[select,c("ID","DataElem")], outfile, sep = ",", row.name=FALSE)

# Correct the known errors using corrections LUT called "correctionsData" (the exported errors from above with the corrections added)
# correctionsData<-"/Users/tkb/Work/GEO/fdia-mac/CSV/corrections.csv"
correctionsData<-"C:\\temp\\FDIA\\CSV\\corrections.csv"
a<-read.csv(correctionsData, header = TRUE, sep = ",", stringsAsFactors = TRUE)
corrections<-a[,c("DataElem","newID")]
for(i in corrections$DataElem) {
  surveyData[ which(surveyData$V10==i), 11]<-corrections[ which(corrections$DataElem==i),"newID"] }

# Return the data element IDs = NA
surveyData[which(is.na(surveyData$V11)),10:11]

# Construct the Scoresheet tallies:

# Inner join themes with the corrected survey data by ID
surveyDataThm <- merge(surveyData, dat, by.x = "V11", by.y = "ID") 

# Count the expected number of elements per theme
expected<-with(dat, table(Theme))

# Count the observed number of elements per theme
observed<-as.numeric(with(surveyDataThm, table(Theme)))
# observed<-data.frame(with(surveyDataThm, table(Theme)))

# Calculate the percentage of surveys returned (observed/expected)
retrate<-observed/expected
diff<-expected-observed
tally<-cbind(observed,expected,diff,retrate)
# write.table(tally,"/Users/tkb/Work/GEO/fdia-mac/CSV/tally.csv",sep = ",")

# Create bar charts showing observed and expected
tallyplt<-spineplot(tally[,c(1,3)],
          main = "Data Elements Surveyed Relative to Theme",
          xlab = "", ylab = "",
          yaxlabels = c("Observed","Expected"),
          off = 10,
          col = c("steelblue3","bisque"))

legend("top", 
       legend = c("Observed", "Expected"), 
       ncol=2, 
       cex = 0.8, 
       col=c("steelblue3","bisque"), 
       bty = "n", 
       lty=1, 
       lwd=2)

tallydf<-as.data.frame(tally)
text(tallyplt,tallydf$retrate+2,labels=as.character(tallydf$retrate))

library(ggplot2)

tallyplt<-ggplot() + geom_bar(aes(y = observed),data=tallydf, 
                              stat = "identity")

tallyplt
