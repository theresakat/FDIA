# tidying.R

# PUrpose
# The purpose of this script is to restructure the united survey data questions for maturity
# into a long form and relate the responses to their point values for calculating the theme
# maturity values


# Prerequisites
# mydataNoComm         united survey data with comments removed, and themes merged. Alternatively
#                       could use "mydata" if the themes have been merged.
#
# scoring               data frame that contains the contents of Scoring.xlsx
# 

# Future enhancements
# 1. adapt all maturity calculations to use the long data form


###############################

# Create the list of questions that are used to calculate maturity
select_vect<-unique(scoring[scoring$MaturityVar == 1, "QuesID"])
MatQs <- paste0(rep("QuesID."), select_vect)
cnk4 <- select(mydata, c(ID, DataElem, MatQs, 55))

# Tidy Maturity Questions data
long <- cnk4 %>% gather(QuesID, Response, c(MatQs), na.rm = FALSE)
s<-long %>% separate(QuesID, c(NA, "QuesID"))

# Merge labels and variable (question) names
  # Create the LUT from Scoring
lut<-scoring[22:252, c("MaturityVar", "QuesID", "shortLabel","PointValue")] 
val<-as.integer(lut$PointValue)
lut<-cbind(lut,val)
names(lut)[3]<-"Response"

  # Import Variable Name LUT
VarNameLUT<-read.csv("C:\\Temp\\FDIA\\LUT\\VarNameLUT.csv", stringsAsFactors = FALSE)
VarNameLUT <- VarNameLUT[VarNameLUT$Drop == 0,]

  # Merge the long data with the LUT
l2<-merge(s,lut, by.x = c("Response","QuesID"), by.y = c("Response", "QuesID"), all.x = TRUE)
l3 <- merge(l2, VarNameLUT, by = "QuesID", all.x = TRUE)
  
# Subset and calculate the summaries by theme
longVals <- l3[l3$MaturityVar == 1,]
write.csv(longVals, "c:\\Temp\\FDIA\\CSV\\longVals.csv", sep = ",")

# cdata<-ddply(longVals, c("DataElem", "Theme"), summarize,
#              N = sum((val)),
#              mean = mean(!is.na(val)))
