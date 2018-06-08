### impLongFactor.R ###

# This is a function.
# 
# Purpose:
# Import factor responses with long text responses and a comment field. 
# 

# mydata    the data frame that is used to collect united variables
# x         the survey data
# questionID  character vector containing the QuesID, E.g.: enter "QuesID.21"
# startCol  initial response column in survey data (x) for questionID
# endCol    last response column in survey data (x) for questionID
# namesVect character vector of names/levels for the imported factor
# scoring   the scoring spreadsheet that contains labels and label codes

# Future enhancement:
# add option that will import a comment field if indicated to do so.


impLongFactor<- function(mydata, x, questionID, startCol, endCol, namesVect, scoring) {
  df<-mydata[,1]
  cols<-c(startCol:endCol)
  for (i in cols[-length(cols)]) {
    df<-data.frame(df,impFactor(x, scoring,i))
  }
  names(df)<-namesVect
  r<-unite(df,questionID,2:length(df), sep="")
  rf<-factor(r$questionID)
  levels(rf)<-c("missing",names(df)[-1])
  
  # Alternative method for generating and assigning factor levels
  # mylevels<-c("missing")
  #   for (i in cols)
  #     mylevels<-c(mylevels,paste(scoring[scoring$VarID==i,"label"])) # these aren't the same as names(df)
  # levels(rf)<-c(mylevels)

  # Import comments to mydata
  comments<-data.frame(x[,cols[length(cols)]], stringsAsFactors = FALSE)
  outdata<-cbind(rf, comments)
  names(outdata)<-c(paste(questionID),paste(questionID,"Comments",sep="."))
  return(outdata)
}
