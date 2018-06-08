# impFactor.R function
# 
# Purpose
#   To import and recode factor response columns that have long text responses. 
#   Function uses the numeric "labelcode" to recode the 
#   long text comments, using the recode() function.
# 
# Note: this function is likely replacable with simple factor functions


impFactor<- function(x, scoring, var) {
  a<-x[,var]; head(a); str(a)
  alog<-is.empty(as.character(a))
  aa<-recode(as.character(alog),
             'FALSE'= c(paste(scoring[scoring$VarID==var,"labelCode"])), 
             .default = "", 
             .missing = NULL)
  return(as.character(aa))
}