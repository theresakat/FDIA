### FDIA-Functions.R
### 
### 


### impFactor.R ###
# 
# Purpose
#   To import and recode factor response columns that have long text responses. 
#   Function uses the numeric "labelcode" to recode the 
#   long text comments, using the recode() function.
# 
# Warning: used in impLongFactWComm and impLongFactor functions
# 
# Prerequisites:
# x         survey data
# scoring   imported scoring table from Scoring.xlsx
# var       numeric variable ID called "VarID" in the Scoring.xlsx "Scoring" tab
# 
# Note: this function is not replacable with simple factor() functions because 
#       of the way they handle empty values in the imported data.
# 
# Fixes: 
#   - fixed issue when there aren't any values in the imported data (i.e., no one selected 
#         an option in the survey). 
#         
# Future enhancements: update to replace the for() loop with an apply() functional
# 
# old code:
# impFactor<- function(x, scoring, var) {
#   a<-x[,var]; head(a); str(a)
#   alog<-is.empty(as.character(a))
#   aa<-recode(as.character(alog),
#              'FALSE'= c(paste(scoring[scoring$VarID==var,"labelCode"])), 
#              .default = "", 
#              .missing = NULL)
#   return(as.character(aa))
# }

impFactor<- function(x, scoring, var) {
  a<-x[,var]; head(a); str(a)
  if(all(sapply(a,is.na))) {
    alog<-is.na(a)
    aa<-recode(as.character(alog),
               'TRUE' = c(""),
               .default = "",
               .missing = NULL)
  } else {
    alog<-is.empty(as.character(a))
    aa<-recode(as.character(alog),
               'FALSE'= c(paste(scoring[scoring$VarID==var,"labelCode"])), 
               .default = "", 
               .missing = NULL)
  }
  return(as.character(aa))
}


### impFactor2 ###
#
# Purpose: Simple function to import a factor

impFactor2<- function(x, scoring, var) {
  a<-x[,var]; head(a); str(a)
  return(a)
}


### impOther ###
# 
# Purpose: Function to import "Other with comments" fields and retain only the "Other" 
#   code and label. Comments are dropped. ####

impOther<-function(x,scoring,var) {
  a<-as.character(x[,var])
  alog<-is.empty(a)
  aa<-recode(as.character(alog), 
             'FALSE' = c(paste(scoring[scoring$VarID==var,"label"])), 
             .default = "", 
             .missing = NULL)
  return(as.character(aa))
}


### impLongFactWComm and impLongFactor ###

# Purpose: Import and unite factor responses with long text responses and a comment field. 
# 
# Prequisites:
# mydata    the data frame that is used to collect united variables
# x         the survey data
# questionID  character vector containing the QuesID, E.g.: enter "QuesID.21"
# startCol  initial response column in survey data (x) for questionID
# endCol    last response column in survey data (x) for questionID
# namesVect character vector of names/levels for the imported factor
# scoring   the scoring spreadsheet that contains labels and label codes

# Future enhancement:
# add option that will import a comment field when indicated to do so.
# recode "missings" to NA_character_  E.g., myoutdata[[1]]<-recode_factor(myoutdata[[1]], 'missing' = NA_character_) 


impLongFactWComm<- function(mydata, x, questionID, startCol, endCol, namesVect, scoring) {
  df<-mydata[,1]
  cols<-c(startCol:endCol)
  for (i in cols[-length(cols)]) {
    df<-data.frame(df,impFactor(x, scoring,i))
    newlevels<-c("", as.character(scoring[scoring$VarID==i, "labelCode"]))
    if(all(is.empty(as.character(df[,length(df)])))) {
        levels(df[,length(df)])<-newlevels
     }
  }
  names(df)<-namesVect
  r<-unite(df,questionID,2:length(df), sep="")
  rflevels<- c("", as.character(1:(length(namesVect)-1)))
  rf<-factor(r$questionID, ordered = TRUE, levels = rflevels) 
  
  levels(rf)<-c("missing",names(df)[-1])
  
  # Alternative method for generating and assigning factor levels
  # mylevels<-c("missing")
  #   for (i in cols)
  #     mylevels<-c(mylevels,paste(scoring[scoring$VarID==i,"label"])) # these aren't coming out to be the same as names(df)
  # levels(rf)<-c(mylevels)
  
  # Import comments to mydata
  comments<-data.frame(x[,cols[length(cols)]], stringsAsFactors = FALSE)
  outdata<-cbind(rf, comments)
  names(outdata)<-c(paste(questionID),paste(questionID,"Comments",sep="."))
  return(outdata)
}

### impLongFactor ###

# Purpose: to import and unite multiple factors given the start and end columns
impLongFactor<- function(mydata, x, questionID, startCol, endCol, namesVect, scoring) {
  df<-mydata[,1]
  cols<-c(startCol:endCol)
  for (i in cols) {
    df<-data.frame(df,impFactor(x, scoring,i))
    newlevels<-c("", as.character(scoring[scoring$VarID==i, "labelCode"]))
    if(all(is.empty(as.character(df[,length(df)])))) {
      levels(df[,length(df)])<-newlevels
    }
  }
  names(df)<-namesVect
  r<-unite(df,questionID,2:length(df), sep="")
  rflevels<- c("", as.character(1:(length(namesVect)-1)))
  rf<-factor(r$questionID, ordered = TRUE, levels = rflevels) 
  levels(rf)<-c("missing",names(df)[-1])
  outdata<-data.frame(rf)
  names(outdata)<-c(paste(questionID))
  return(outdata)
}