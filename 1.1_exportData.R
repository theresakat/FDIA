# Export data to preserve it.
# 


################################################

### Set environment - Windows
mywd<-"C:\\temp\\FDIA"
setwd(mywd)


### Set environment - MAC
# outfile<-paste(mywd,"/CSV/errors_", Sys.Date(), ".csv", sep="")


### Provide the data to be exported 
indata<-x
mydata<-"x"  # input the name of the output data file

### Export data
outfile<-paste(mywd,"\\CSV\\", mydata, "_", Sys.Date(), ".csv", sep="")
write.table(indata, outfile, sep = ",", row.name=FALSE)


