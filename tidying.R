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
lut<-lut %>% select(everything()) %>% rename(PtVal_int = val, Response = shortLabel) %>% unique()

  # Import Variable Name LUT
VarNameLUT<-read.csv("C:\\Temp\\FDIA\\luts\\VarNameLUT.csv", stringsAsFactors = FALSE)
VarNameLUT <- filter(VarNameLUT, Drop == 0)

  # Join the long data with the look up tables of maturity index values and var names
l2<-left_join(x = long2, y = lut, by = c("Response","QuesID")) 
l3<-left_join(x = l2, y = VarNameLUT, by = "QuesID")

# Subset and calculate the summaries by theme. (NOTES: grouping variable "Theme" is a factor
#   and point value variable is a character variable. Future enhancement: convert these to 
#   "chr" and "int" respectively during an earlier step.)
longVals <- filter(l3, MaturityVar == 1)
write.csv(longVals, "c:\\Temp\\FDIA\\CSV\\longVals.csv", sep = ",")
rm(l2,l3, long2, long)

# Theme Maturity Index
matThm <- longVals %>% 
  group_by(as.character(Theme)) %>% 
  summarize(n = n(),
            mean = mean(PtVal_int, na.rm=TRUE), 
            mode = Mode(PtVal_int),
            sum = sum(PtVal_int)) 
write.csv(matThm, "c:\\Temp\\FDIA\\CSV\\matThm.csv")


# Produce response summaries grouped by theme and question 
longVals %>% 
  group_by(Theme, VarName) %>% 
  select(Theme, DataElem, VarName, PtVal_int) %>% 
  summarize(n = n(),
            mean = mean(PtVal_int, na.rm=TRUE), 
            mode = Mode(PtVal_int),
            sum = sum(PtVal_int))


######################################
# Life Cycle Stage Maturity

# Wrangle lookup tables
quescats<-read.csv("C:\\Temp\\FDIA\\CSV\\QuesCats.csv", stringsAsFactors = FALSE)
l1 <- scoring %>% 
  select(QuesID, CatID) %>% 
  distinct() 

lutStages <- left_join(x = l1, y = quescats) %>% select(-N.questions)
rm(l1)

# Join lookup table to data frame
longVals <- left_join(x = longVals, y = lutStages, by = "QuesID")

# Maturity by Theme and Life Cycle Stage
matThmStage <- longVals %>% 
  group_by(Theme, Category, CatID) %>% 
  summarize(n = n(),
            min = min(PtVal_int),
            mean = mean(PtVal_int, na.rm=TRUE), 
            mode = Mode(PtVal_int),
            max = max(PtVal_int),
            varitn = var(PtVal_int),
            sd = sd(PtVal_int),
            se = sd/sqrt(n),
            pos = round(100*(mean/5),digits=0)) %>% 
  arrange(Theme,CatID)

write.csv(matThmStage, "c:\\Temp\\FDIA\\CSV\\matThmStage.csv")

# Example code to create multiple graphs on a single view
grid.arrange(arrangeGrob(gg.gauge(62,0.5),gg.gauge(20,1),gg.gauge(52,2.6),gg.gauge(90,4.5),ncol=2))

             # customize code above to use the rounded value stored in matThmStage$mean,
             # and positioning the mean using value in matThmStage$pos. Alternatively,
             # omit the label associated with the value and let the graphic tell the value
             
             
plot<-lapply(...)
test<-marrangeGrob(gg.gauge(10,0.5),gg.gauge(20,1),gg.gauge(52,2.6),gg.gauge(90,4.5),nrow=3,ncol=2)
ggsave("C:\\Temp\\FDIA\\graphics\\test.pdf",test)

# Maturity by Life Cycle Stage (overall)
matStage <- longVals %>% 
  group_by(Category, CatID) %>% 
  summarize(n = n(),
            min = min(PtVal_int),
            mean = mean(PtVal_int, na.rm=TRUE), 
            mode = Mode(PtVal_int),
            max = max(PtVal_int),
            varitn = var(PtVal_int),
            sd = sd(PtVal_int),
            se = sd/sqrt(n),
            pos = round(100*(mean/5),digits=0)) %>% 
  arrange(CatID)
write.csv(matStage,  "c:\\Temp\\FDIA\\CSV\\matStage.csv")

# Data Element Maturity Index
matElem <- longVals %>% 
  group_by(as.character(DataElem)) %>% 
  summarize(n = n(),
            mean = mean(PtVal_int, na.rm=TRUE), 
            mode = Mode(PtVal_int),
            sum = sum(PtVal_int)) %>% 
  arrange(DataElem)


# Functions

# Calculate the most frequently occurring value, the mode. NOTE: not working consistently.
Mode<- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
