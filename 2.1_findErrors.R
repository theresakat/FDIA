# 3.0.1 Find errors

# Purpose
# Script used to find and confirm errors in the raw data, e.g., 
#   - missing numeric IDs (NAs)
#   - duplicated ID values
#   - duplicated data element values
#   
#   
# Prerequisites
#   de      table created in 3.0_cleanSurveyData.R. Contains
#           mismatched survey data element names and Framework 
#           MASTER table element names
# 

################################################


# Return records where ID isn't given (NAs)
de[is.na(de$ID),]

# Return duplicated data element names 
# IMPORTANT: duplicated() along only provides the second occurrence or greater, so
#   if there are 3 occurrences of A, it only returns the second and third, but not
#   the first occurrence. By piping it to duplicated() with fromLast = TRUE, it searches
#   in both directions and returns the desired set of values

which(duplicated(de$ID))
select<-which(duplicated(de$ID) | duplicated(de$ID, fromLast = TRUE))
dups<-de[select,c("DataElem","ID")]
arrange(dups,DataElem)
arrange(dups,ID)

which(duplicated(de$DataElem))
which(duplicated(de$DataElem) | duplicated(de$DataElem, fromLast = TRUE))

# View digital elevation model redundancies
grep("^digital elevation", de$DataElem)
grep("^digital elevation", y$Element)
y[grep("^digital elevation", y$Element), c("ID","DataElem", "Theme", "Element")]

# View bathymetry ID issue
y[grep("bathymetry", y$DataElem), c("ID","DataElem", "Theme", "Element")]
dat[dat$ID==122,1:5]
