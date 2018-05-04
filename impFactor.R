impFactor<- function(x, scoring, var) {
  a<-x[,var]; head(a); str(a)
  alog<-is.empty(as.character(a))
  aa<-recode(as.character(alog),
             'FALSE'= c(paste(scoring[scoring$VarID==var,"labelCode"])), 
             .default = "", 
             .missing = NULL)
  return(as.character(aa))
}