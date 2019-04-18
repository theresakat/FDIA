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
# 2. convert variables Theme and PointValue to "chr" and "int" respectively during an earlier 
#     cleaning step. 


###############################

# Create the list of questions that are used to calculate maturity
select_vect<-unique(scoring[scoring$MaturityVar == 1, "QuesID"])
MatQs <- paste0(rep("QuesID."), select_vect)
cnk4 <- select(mydata, c(ID, DataElem, MatQs, 55))

# Tidy Maturity Questions data
long <- cnk4 %>% gather(QuesID, Response, c(MatQs), na.rm = FALSE)
long2 <-long %>% separate(QuesID, c(NA, "QuesID")) %>% mutate(QuesID = as.integer(QuesID))

# Merge maturity var flag and variable (question) names
  # Create the LUT from Scoring
lut<-scoring[22:252, c("MaturityVar", "QuesID", "shortLabel","PointValue")] 
val<-as.integer(lut$PointValue)
lut<-cbind(lut,val)
lut<-lut %>% select(everything()) %>% rename(PtVal_int = val, Response = shortLabel) 


  # Import Variable Name LUT
VarNameLUT<-read.csv("C:\\Temp\\FDIA\\luts\\VarNameLUT.csv", stringsAsFactors = FALSE)
VarNameLUT <- filter(VarNameLUT, Drop == 0)

  # Join the long data with the look up tables of maturity index values and var names
l2<-left_join(x = long2, y = lutu, by = c("Response","QuesID")) 
l3<-left_join(x = l2, y = VarNameLUT, by = "QuesID")

# Subset and calculate the summaries by theme. (NOTES: grouping variable "Theme" is a factor
#   and point value variable is a character variable. Future enhancement: convert these to 
#   "chr" and "int" respectively during an earlier step.)
longVals <- l3[l3$MaturityVar == 1,]
write.csv(longVals, "c:\\Temp\\FDIA\\CSV\\longVals.csv", sep = ",")
rm(l2,l3)

# Theme Maturity Index
matThm <- longVals %>% 
  group_by(as.character(Theme)) %>% 
  summarize(mean = mean(PtVal_int, na.rm=TRUE), 
            sum = sum(PtVal_int))

# Descriptive stats per question. Useful for 
longVals %>% 
  group_by(VarName) %>% 
  summarize(n = n(),
            min = min(PtVal_int),
            mean = mean(PtVal_int), 
            max = max(PtVal_int),
            varitn = var(PtVal_int),
            sd = sd(PtVal_int),
            se = sd/sqrt(n))




# Functions

# Calculate the most frequently occurring value, the mode. NOTE: not working consistently.
Mode<- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}