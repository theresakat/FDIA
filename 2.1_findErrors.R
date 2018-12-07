# Find errors
# 

# Return records where ID isn't given
de[is.na(de$ID),]

# Return duplicated data element names 
# IMPORTANT: duplicated() along only provides the second occurrence or greater, so
#   if there are 3 occurrences of A, it only returns the second and third, but not
#   the first occurrence. By piping it to duplicated() with fromLast = TRUE, it searches
#   in both directions and returns the desired set of values

which(duplicated(de$ID))
which(duplicated(de$ID) | duplicated(de$ID, fromLast = TRUE))

which(duplicated(de$DataElem))
which(duplicated(de$DataElem) | duplicated(de$DataElem, fromLast = TRUE))


grep("^digital elevation", de$DataElem)
grep("^digital elevation", y$Element)
y[grep("^digital elevation", y$Element), c("ID","DataElem", "Theme", "Element")]
y[grep("bathymetry", y$DataElem), c("ID","DataElem")]
dat[dat$ID==122,1:5]