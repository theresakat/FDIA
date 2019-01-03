# maturityInd.R
# 
# Maturity Index Calculation
# 
# Purpose
# Calculate the maturity index for data themes
# 
# Future enhancements
# 
#   
# Prerequisites
#   mydata      united survey data developed using the clean-02-UniteSurveyData.R script
#   scoring     imported Excel sheet containing the Maturity Index flag and weights
#       
# Processing steps
# 1. select the questions to be used in the calculation
# 2. assign the values based on the scoring$PointValue field
# 3. omit the muydata$XXX.Comments fields from the calculations
# 4. calculate the index (likely to use row sums and apply)       
#
# Future enhancements
# - update object naming from "test" and other nonsensical names
# 
################################################

# Create the list of questions that are used to calculate maturity
select_vect<-unique(scoring[scoring$MaturityVar == 1, "QuesID"])
select_vect <- select_Vect[2:length(select_Vect)]


# Omit the Comments fields from united survey data by creating a selection vector
mydataNoComm<-select(mydata, -ends_with("Comments"))
# myvars<-c("V11", "V10", paste("QuesID.", select_vect, sep=""), "Theme", "Element")
# myvars<-c(paste("QuesID.", select_vect, sep=""),"Element")
myvars<- paste("QuesID.", select_vect, sep="")
test<-select(mydataNoComm, myvars)

# Calculate column sums
sapply(test,function(x) sum(as.numeric(x)))

# Convert factors to numbers
testnum<-lapply(test, function(x) as.numeric(x))
testnumdf<-data.frame(testnum)

# Calculate maturity totals (row sum & avg) & attach data element IDs
matSum<-rowSums(testnumdf)
maturity<-cbind(mydata[,c("V11", "V10")], matSum)

matMean<-rowMeans(testnumdf)
maturity<-cbind(maturity, matMean)

# Sort maturity values in ascending order
matSort<-maturity[order(maturity$matSum),]


# What is the maximum level in each maturity variable? (consider normalizing point values for each question)
sapply(testnumdf, function(x) max(x))


hist(maturity$matMean)
hist(maturity$matSum)
qqnorm(maturity$matSum)




