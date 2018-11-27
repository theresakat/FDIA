# Scoresheet.R

# Purpose: this script is used to create the scoresheet tally of data elements by theme
# 
# Required files:
#   dat             contains the contents of the 1_tblFrameworkData_MASTER.csv (load using 1_load_FDIA.R)
#   surveyData      contains the raw imported survey data (missing IDs corrected)
#   surveyDataThm   corrected survey data with themes field. Created using "cleanSurveyData.R"

# Workflow
#   1. import data to R using import script
#   2. set up empty data frame for identifying and exporting mismatched records in the data
#   3. correct data element names
#   4. add data element numeric IDs where missing
#   5. construct scoresheet tallies

### Set environment - Windows
mywd<-"C:\\temp\\FDIA"
setwd(mywd)

### Set environment - MAC
mywd<-"C:/temp/FDIA"
setwd(mywd)


### CONSTRUCT THE SCORESHEET TALLIES ###

# Count the expected number of elements per theme
expected<-with(dat, table(Theme))

# Count the observed number of elements per theme
observed<-as.numeric(with(surveyDataThm, table(Theme)))
# observed<-data.frame(with(surveyDataThm, table(Theme)))

# Calculate the percentage of surveys returned (observed/expected)
retrate<-observed/expected
diff<-expected-observed
tally<-cbind(observed,expected,diff,retrate)
tally<-tally[,c(1,2,4,6,8)]
names(tally)<-c("Theme", "Observed", "Expected","Difference", "RetRate")

write.table(tally, "C:\\temp\\FDIA\\CSV\\tally_20181014.csv", sep = ",")
# write.table(tally,"/Users/tkb/Work/GEO/fdia-mac/CSV/tally.csv",sep = ",") #MAC path

# Create bar charts showing observed and expected
tallyplt<-spineplot(tallydf[,1:2],
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

###
###
library(ggplot2)

tallyplt<-ggplot() + geom_bar(aes(y = observed),data=tallydf, 
                              stat = "identity")

tallyplt<-ggplot() + geom_dotplot(aes(y = observed),data=tallydf, 
                              stat = "identity")

tallyplt
