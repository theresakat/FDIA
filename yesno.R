# yesno.R function
# Function to import and clean Yes/No questions
# 
# Created: 5/3/2018
# 
# Purpose: Use this function to import questions that have the Yes column, then the No column in the
# numeric data exported from Survey Monkey. 
#
# Instructions:
#   To preserve the function's output, assign the output to the data frame that will be used to collect
#   the imported and cleaned data. In this case, "mydata", e.g.:
#       output <- yesno(x,quesLabel, startVar, endVar) becomes
#       mydata <- yesno(x,"QuesID.17", 30, 31)
#
# Prerequisites
#   x         raw data loaded using 1_load_FDIA.R
#   mydata    data frame created in the clean-part-01.R to hold imported data. Precursor to 
#             function "yesno.R"
#
# Background. 
# I used Question 17 (QID105) Levels: Yes, No (in this order)  VarIDs: 30-31 to create this function.
#


yesno<-function(x, quesLabel, startVar, endVar) {
  q<-paste(quesLabel)
  i<-startVar
  j<-endVar
  
  a<-x[,i]; head(a)
  alog<-is.empty(as.character(a))
  aa<-recode(as.character(alog),'FALSE'="1", .default = "", .missing = NULL) #"recode "Yes" responses
  b<-x[,j]; head(b)
  blog<-is.empty(as.character(b))
  bb<-recode(as.character(blog),'FALSE' = "2", .default = "", .missing = NULL) #recode "No" responses
  
  df<-data.frame(aa,bb); head(df)
  r<-unite(df,paste(c(q)),1:2, sep=""); 
  
  QuesID.17<-data.frame(factor(r[,1]))
  names(QuesID.17)<-q
  levels(QuesID.17[,1])<-c("missing","Yes","No")
  head(QuesID.17)
  mydata<-cbind(mydata,QuesID.17)
  
  rm(a,b, df,aa,bb,r,alog,blog,i,j,q)
  
  return(mydata)
}