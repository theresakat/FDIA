# maturityInd.R
# 
# Maturity Index Calculation
# 
# Purpose
# Calculate the maturity index
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
################################################


select_vect<-unique(scoring[scoring$MaturityVar == 1, "QuesID"])
select_vect <- select_Vect[2:length(select_Vect)]

