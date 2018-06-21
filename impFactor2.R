
## Function to import a factor

impFactor2<- function(x, scoring, var) {
  a<-x[,var]; head(a); str(a)
  return(a)
}


#### Function to import "Other with comments" fields and to retain only the "Other" 
####   code and label. Comments are dropped. ####

impOther<-function(x,scoring,var) {
  a<-as.character(x[,var])
  alog<-is.empty(c)
  aa<-recode(as.character(alog), 
             'FALSE' = c(paste(scoring[scoring$VarID==var,"label"])), 
             .default = "", 
             .missing = NULL)
  return(as.character(aa))
}


# impComments<-functions(x,scoring,var)